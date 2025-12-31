(ns gmrs.preprocess
  "Operate on semantic data such as data columns to prepare for mills. The
  meaning-agnostic things about reformatting etc. should go into wrangle."
  (:require [clojure.string :as str :refer [starts-with?]]
            [gmrs.math :as math]
            [gmrs.wrangle :as wrangle]))

;; NOTE: If we don't debug, we try to ignore them.
(def ^:dynamic *debug-preproc-exceptions* false)

(defn longer [coll1 coll2]
  (if (> (count coll1) (count coll2))
    coll1 coll2))

(defn z-logistic-scale
  "Fit a numerical seq encoding. It first applies a Z-score scaling (by standard
  deviation around the mean as 0), then calls a sigmoid (logistic function). The
  result is between 0 and 1."
  [coll]
  (math/desc-stats coll))

(defn apply-z-logistic-scale
  "Apply a numerical seq encoding obtained from z-logistic-scale."
  [coll transf]
  (assert (:sd transf))
  (math/z-logistic-scale coll (:mean transf) (:sd transf)))

(defn split-tags-str [tags-str]
  (map str/trim
       (str/split tags-str #"(\|)|,")))

(defn tag-value?
  "Check if the value is usable for multihot taggs encoding."
  [value]
  (and value (not (and
                    (some true? (map #(% value) [string? symbol? keyword?]))
                    (= "" (str/trim (name value)))))))

(defn get-multihot-values
  "Get a set of column values that can be used for future preprocessing; so all
  these and only these will be present after calling multihot-from-tags with
  this set."
  [tags-column]
  (reduce into #{} (filter
                     some?
                     (map (fn [value] (when (tag-value? value)
                                        (map keyword (split-tags-str value))))
                          tags-column))))

; TODO: profile against a cleaner impl (this is the oldest code in the project)
(defn multihot-from-tags
  "Given a column of tags separated by pipes, return a mapping of columns to
  vectors to 0s and 1s. The col names are prefix+$tag. You can supply values-set
  so all and only these will be processed from the column."
  ([tags-column] (multihot-from-tags tags-column nil "proc-"))
  ([tags-column values-set] (multihot-from-tags tags-column values-set "proc-"))
  ([tags-column values-set prefix]
   (let [zeros (vec (repeat (count tags-column) 0.0)),
         tag->cols (atom (if values-set
                           (reduce into {}
                                   (map
                                     (fn [value] { (keyword (str prefix
                                                                (name value)))
                                                   zeros })
                                     values-set))
                           ;; init with empty if no values-set
                           {}))]
     (dorun (map-indexed
              (fn [row-idx row-val]
                (when row-val
                  (run!
                    (fn [tag-col-name]
                      ;; Add a column in output, if we don't have the set
                      ;; pre-determined.
                      (when (and (not values-set)
                                 (not (@tag->cols tag-col-name))
                                 (tag-value? tag-col-name))
                        (swap! tag->cols assoc tag-col-name zeros))
                      (when (@tag->cols tag-col-name)
                        (swap! tag->cols
                               update-in [tag-col-name]
                               #(assoc % row-idx 1.0))))
                    (set (map #(keyword (str prefix %))
                              (split-tags-str row-val))))))
              tags-column))
     @tag->cols)))

;;;
;;; Column preprocessing structures.
;;;

;; Prepare function generates any transf object that should be passed as the
;; second arg to the execute function.
;; Priority is 5 for regular preprocessing, 10 for conversions necessary as
;; a start.
(defrecord PreprocessingTransform [prepare execute priority])

; TODO: binning
(def ZLogisticScale
  (->PreprocessingTransform z-logistic-scale apply-z-logistic-scale 5))

(def MultihotFromTags
  (->PreprocessingTransform get-multihot-values multihot-from-tags 5))

(defn safe-parse
  ([parse-fn] (safe-parse parse-fn nil))
  ([parse-fn default]
   (->PreprocessingTransform
     (fn [_] (str parse-fn ", default " default))
     (fn [coll _]
       (when coll
         (map #(try (parse-fn %) (catch Exception _ default))
              coll)))
     10)))

;; FIXME: taps here should be printed by default
(defn report-transf-failure
  [transf exception]
  (tap> { :place :preprocess-execute
          :reason :execution-error
          :transf transf
          :exception (pr-str exception) }))

(defn get-safe-execute
  "Wrap transformation execution so nil is returned on any exception."
  [transf]
  (fn [coll prepared]
    (try ((.execute transf) coll prepared)
         (catch Exception e
           (if *debug-preproc-exceptions*
             (throw e)
             (do
               (report-transf-failure transf e)
               nil))))))

(defn quick-transform [^PreprocessingTransform transf coll]
  ((get-safe-execute transf)
   coll
   ((.prepare transf) coll)))

;;;
;;; Applying transforms to column sets (i.e. maps).
;;;

(defn func? [x] (instance? clojure.lang.IFn x))

(defn get-col-groups
  "Group columns from multiple column sets if they have the same :group-... tag,
  otherwise put a column under its own :ungroup-... key. Return a map of group
  keys to group entries (which are maps of :col-name and :set-n)."
  ([set-taggings] (get-col-groups {} set-taggings 0))
  ([accum-groups-map set-taggings set-n]
   (if (empty? set-taggings)
     accum-groups-map
     (recur (reduce-kv
              (fn [groups-map col-name tags]
                (if-let [group-key (some
                                     (fn [tag]
                                       (when (starts-with? (name tag) "group")
                                         tag))
                                     tags)]
                  (update-in groups-map [group-key]
                             conj { :col-name col-name
                                   :set-n set-n })
                  (update-in groups-map
                             [(keyword
                                (str "ungroup:" set-n col-name))]
                             conj { :col-name col-name
                                    :set-n set-n })))
              accum-groups-map
              (first set-taggings))
            (rest set-taggings) (inc set-n)))))

(defn prepared-transf
  "Prepare transform and create the function."
  [transf coll]
  (let [prep ((.prepare transf) coll)]
    (fn [coll] ((.execute transf) coll prep))))

(defn get-groups-to-ready-transfs
  "Get a mapping of groups to functions combining all necessary preprocessing,
  fitted to the data supplied in the col-sets.

  Args are similar to retag-with-preproc-transforms and
  execute-preprocessing-instructions. Col-groups should be the output of
  get-col-groups on the set-taggings.

  If a group cannot be preprocessed, it will be skipped in the output and a
  warning may appear in tap> if this is due to a transformation error."
  [tags-table set-taggings col-sets col-groups]
  (reduce
    into {}
    (map
      (fn [[group-name group]]
        (let [all-cols (map (fn [{:keys [col-name set-n]}]
                              (get (nth col-sets set-n)
                                   col-name))
                            group),
              ;; Lump all relevant columns from all sets for transformations.
              cols-lumped (apply concat all-cols),
              tag-transforms
              (map
                (partial get tags-table)
                ;; Lookup the col-name in set-taggings to get the tags.
                ;; NOTE: tags must be the same for every column!
                (get (nth set-taggings (-> group first :set-n))
                     (-> group first :col-name))),
              sorted-tag-transforms
              (sort-by #(.priority %) > (filter some? tag-transforms)),
              accumulated-group-funs
              (filter
                some?
                (map first
                     ;; Here, we need to accumulate the prepared transformations
                     ;; in the correct order and (for the accumulation) also
                     ;; their results so the subsequent transfs can be prepared.
                     ;; The transf is the first in acc, and the running result
                     ;; second.
                     (reductions
                       (fn [acc-transf-and-cols-lumped transf]
                         (when *debug-preproc-exceptions*
                           (println "Preprocessing" group-name "- transform:"
                                    (.execute transf)))
                         (let [cols (second acc-transf-and-cols-lumped),
                               prep-transf (try
                                             ((.prepare transf) cols)
                                             (catch Exception e
                                               (report-transf-failure transf e)
                                               nil)),
                               transf-fun
                               (do
                                 (when *debug-preproc-exceptions*
                                   (println "Prepared:" prep-transf
                                            "from" (take 5 cols) "..."))
                                 (when prep-transf
                                   (fn [coll] ((get-safe-execute transf)
                                               coll prep-transf)))),
                               transf-coll (when transf-fun (transf-fun cols))]
                           (tap> {:place :groups-to-ready-transfs
                                  :group-name group-name
                                  :transf transf
                                  :prepared-transf prep-transf})
                           ;; Accumulate the function for later use and coll for
                           ;; use for the subsequent transformations. Skip the
                           ;; transfs that crash and return nil.
                           (if transf-coll
                             [transf-fun transf-coll]
                             acc-transf-and-cols-lumped)))
                       [nil cols-lumped]
                       sorted-tag-transforms)))]
          (when (some any? accumulated-group-funs)
            { group-name
              (apply
                comp
                ;; As the last funcs to comp will be executed first:
                (reverse accumulated-group-funs)) })))
      col-groups)))

(defn retag-with-preproc-transforms
  "Get tags-table and new set-taggings. It will add group tags for the columns
  from col-sets in set-taggings. The group tags will be mapped in tags-table
  to already prepared transformation funcs, already combining the relevant
  .execute fields of PreprocessingTransform records and results from their
  .prepare fields."
  [tags-table set-taggings col-sets id-cols]
  (let [col-groups (get-col-groups set-taggings),
        groups-to-transfs (get-groups-to-ready-transfs
                            tags-table
                            set-taggings
                            (map (fn [s id] (dissoc s id))
                                 col-sets id-cols)
                            col-groups),
        new-col-taggings
        (fn [group-name col-entries]
          (reduce into {}
                  (map (fn [entry] { [(:set-n entry) (:col-name entry)]
                                     (conj (set ((:col-name entry)
                                                 (nth set-taggings
                                                      (:set-n entry))))
                                           group-name) })
                       col-entries)))
        unroll-new-taggings (fn [accum-separate-taggings new-taggings]
                              (reduce-kv (fn [taggings col-key new-tags]
                                           (assoc-in
                                             taggings
                                             [(first col-key) (second col-key)]
                                             new-tags))
                                         accum-separate-taggings
                                         new-taggings))]
    { :tags-table groups-to-transfs
      :set-taggings (unroll-new-taggings (vec (repeat (count set-taggings) {}))
                                         (reduce into {}
                                                 (map new-col-taggings
                                                      (keys col-groups)
                                                      (vals col-groups)))) }))

(defn execute-preprocessing-instructions
  "Apply all transformation functions from tags-table to the columns in
  col-sets. The cols are mapped to transformations with the help of set-taggings
  which map column names to data type tags.

  The metadata of original col-sets will be preserved.

  id-cols is a sequence of ID columns for each of the col-sets, which will be
  preserved with no preprocessing.

  Other columns with no tags or no preprocessing will be skipped in the output.

  ## Grouped processing
  Special tags in the form of :group-XYZ guarantee that all cols tagged this
  way will be seamlessly preprocessed together - for example for encoding tags
  or scaling number features.

  If a group cannot be preprocessed, it will be skipped in the output and a
  warning may appear in tap> if this is due to a transformation error.

  ## Multi-column returns from transformations
  The preprocessing functions get the column sequence as their argument. They
  can return either the resulting vector, or a map of :proc-X -> vector, which
  will then all be renamed to :col-name-X in the final col-sets."
  [tags-table set-taggings col-sets id-cols]
  (letfn [(unroll-col-group [accum-col-sets group-col-entries]
            (reduce
              (fn [group-col-sets {:keys [col-name set-n done]}]
                (let [result
                (if (map? done)
                  ;; The multi-column "proc-" case.
                  (let [final-col-names
                        (map (fn [proc-key]
                               (keyword (str
                                          (name col-name) "-"
                                          ;; cut "proc-"
                                          (subs (name proc-key) 5))))
                             (keys done))]
                    (update group-col-sets set-n merge
                            (zipmap final-col-names (vals done))))
                  ;; The base one-vector result case.
                  (assoc-in group-col-sets [set-n col-name] done))]
                  result))
              accum-col-sets
              group-col-entries)),
          (unroll-col-groups [groups]
            (reduce unroll-col-group
                    ;; prepare the initial recreated col-sets:
                    (mapv (fn [col-set] (with-meta {} (meta col-set)))
                          col-sets)
                    groups))]
    (map
      (fn [orig-col-set col-id-col prepr-col-set]
        (assoc prepr-col-set col-id-col
               (col-id-col orig-col-set)))
      col-sets
      id-cols
      ;; Here, the preprocessed data will be organized by the groups - allowing
      ;; them to be processed together. The columns will be placed in the correct
      ;; col-set in (unroll-col-group).
      (unroll-col-groups
        (filter
          some?
          (map
            ;; Preprocess each group together.
            (fn [[group-name group]]
              (let [all-cols (map (fn [{:keys [col-name set-n]}]
                                    (get (nth col-sets set-n)
                                         col-name))
                                  group),
                    starts-in-lump (reductions + 0 (map count all-cols))
                    set-indices-in-lump (map vector
                                             (butlast starts-in-lump)
                                             (rest starts-in-lump)),
                    cols-lumped (apply concat all-cols),
                    processed (reduce
                                (fn [coll transf]
                                  (when *debug-preproc-exceptions*
                                    (println "Preprocessing" group-name
                                             "- transform:" transf))
                                  ;; transform if there's a defined transf,
                                  ;; otherwise nil the col
                                  (if transf (transf coll) nil))
                                cols-lumped
                                ;; Get tags and make them into a
                                ;; transformations list to be reduced.
                                (longer
                                  (list nil)
                                  (filter
                                    some?
                                    (map (fn [tag]
                                           (let [transf (get tags-table tag)]
                                             (cond
                                               (nil? transf) nil
                                               ;; checking if
                                               ;; PreprocessingTransform instance
                                               ;;isn't reliable between code reloads
                                               (func? transf) transf
                                               :else (partial
                                                       quick-transform transf))))
                                         ;; get the tags for these columns
                                         ;; NOTE: tags must be the same for every
                                         ;; column! that's why we can take the
                                         ;; first entry of the group
                                         (get (nth set-taggings
                                                   (-> group first :set-n))
                                              (-> group first :col-name))))))]
                (when processed
                  (map-indexed (fn [i entry]
                                 (assoc entry :done
                                        ;; to the group entry, add the :done part
                                        ;; of the lumped column vector
                                        (apply wrangle/slice
                                               (into [processed]
                                                     (nth set-indices-in-lump i)))))
                               group))))
            ;; "a map of group keys to group entries, which are maps of :col-name
            ;; and :set-n)"
            (get-col-groups set-taggings)))))))
