"Operate on semantic data such as data columns to prepare for mills. The
meaning-agnostic things about reformatting etc. should go into wrangle."

(ns gmrs.preprocess
  (:require [clojure.string :as str :refer [starts-with?]]
            [gmrs.math :as math]
            [gmrs.wrangle :as wrangle]))

(defn longer [coll1 coll2]
  (if (> (count coll1) (count coll2))
    coll1 coll2))

; TODO: currently none of this handles nulls

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

; TODO: profile against a cleaner impl (this is the oldest code in the project)
(defn multihot-from-tags
  "Given a column of tags separated by pipes, return a mapping of columns to
  vectors to 0s and 1s. The col names are prefix+$tag."
  ([tags-column] (multihot-from-tags tags-column "proc-"))
  ([tags-column prefix]
   (println "TGCOL" tags-column)
   (let [tag->cols (atom {})
         zeros (vec (repeat (count tags-column) 0.0))]
     (dorun (map-indexed
              (fn [row-idx row-val]
                (when row-val
                  (run!
                    (fn [tag-col-name]
                      (when (not (@tag->cols tag-col-name))
                        (swap! tag->cols assoc tag-col-name zeros))
                      (swap! tag->cols
                             update-in [tag-col-name]
                             #(assoc % row-idx 1.0)))
                    (set (map #(keyword (str prefix %))
                              (str/split row-val #"\|"))))))
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

(defn quick-transform [^PreprocessingTransform transf coll]
  ((.execute transf)
   coll
   ((.prepare transf) coll)))

; TODO: binning
(def ZLogisticScale
  (->PreprocessingTransform z-logistic-scale apply-z-logistic-scale 5))

(def MultihotFromTags
  (->PreprocessingTransform (fn [_] "proc-") multihot-from-tags 5))

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
  already fitted to the data supplied in the col-sets. Args are similar to
  retag-with-preproc-transforms and execute-preprocessing-instructions, along
  with output of get-col-groups on the set-taggings."
  [tags-table set-taggings col-sets col-groups]
  (reduce
    into {}
    (map
      (fn [[group-name group]]
        (println "----" group-name (get (nth set-taggings (-> group first :set-n))
                     (-> group first :col-name)))
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
              (sort-by #(.priority %) > (filter some? tag-transforms))]
          (when (some any? sorted-tag-transforms)
            { group-name
             (apply
               comp
               (map first
                    (->
                      (reductions
                        (fn [acc-transf-and-cols-lumped transf]
                          (let [cols (second acc-transf-and-cols-lumped),
                                prep-transf ((.prepare transf) cols),
                                transf-fun
                                (fn [coll] ((.execute transf) coll prep-transf))]
                            ;; Accumulate the function for later use and coll for
                            ;; use for the subsequent transformations.
                            #_(println "For next:" transf (transf-fun cols))
                            [transf-fun (transf-fun cols)]))
                        [nil cols-lumped]
                        sorted-tag-transforms)
                      rest
                      ;; As the last funcs to comp will be executed first:
                      reverse))) })))
      col-groups)))

(defn retag-with-preproc-transforms
  "Get tags-table and new set-taggings. It will add group tags for the columns
  from col-sets in set-taggings. The group tags will be mapped in tags-table
  to already prepared transformation funcs, already combining the relevant
  .execute fields of PreprocessingTransform records and results from their
  .prepare fields."
  [tags-table set-taggings col-sets]
  (let [col-groups (get-col-groups set-taggings),
        groups-to-transfs (get-groups-to-ready-transfs tags-table
                                                       set-taggings
                                                       col-sets
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
  "Apply all functions from tags-table to the columns in col-sets, that are
  indicated by tags in the set-taggings which map column names to data type tags.

  Columns with no tags or no preprocessing will be skipped in the output.

  Special tags in the form of :group-XYZ guarantee that all cols tagged this
  way will be seamlessly preprocessed together - for example for encoding tags
  or scaling number features.

  The preprocessing functions get the column sequence as their argument. They
  can return either the resulting vector, or a map of :proc-X -> vector, which
  will then all be renamed to :col-name-X in the final col-sets.

  The metadata of original col-sets will be preserved."
  [tags-table set-taggings col-sets]
  (letfn [(unroll-col-group [accum-col-sets group-col-entries]
            (reduce
              (fn [group-col-sets {:keys [col-name set-n done]}]
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
                  (assoc-in group-col-sets [set-n col-name] done)))
              accum-col-sets
              group-col-entries)),
          (unroll-col-groups [groups]
            (reduce unroll-col-group
                    (mapv (fn [col-set] (with-meta {} (meta col-set))) col-sets)
                    groups))]
    (unroll-col-groups
      (filter
        some?
        (map
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
                              (fn [coll transf] (if transf (transf coll)
                                                  nil))
                              cols-lumped
                              ;; Get tags them and make them into a
                              ;; transformations list.
                              (longer
                                (list nil)
                                (filter
                                  some?
                                  (map (fn [tag]
                                         (let [transf (get tags-table tag)]
                                           (cond
                                             (nil? transf) nil
                                             (instance? PreprocessingTransform
                                                        transf)
                                             (partial quick-transform transf)
                                             :else transf)))
                                       ;; NOTE: tags must be the same for every
                                       ;; column! (that's why we can take the
                                       ;; first entry of the group)
                                       (get (nth set-taggings
                                                 (-> group first :set-n))
                                            (-> group first :col-name))))))]
              (when processed
                (map-indexed (fn [i entry]
                               (assoc entry :done
                                      (apply wrangle/slice
                                             (into [processed]
                                                   (nth set-indices-in-lump i)))))
                             group))))
          (get-col-groups set-taggings))))))
