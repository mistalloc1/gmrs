(ns gmrs.governor
  (:require [clojure.set :as set]
            [clojure.string :refer [starts-with?]]
            [gmrs.io.getters :refer [getter]]
            [gmrs.data-diag :refer [diag-all-values]]
            [gmrs.preprocess :as preproc]
            [gmrs.wrangle :as wrangle]))

(defn sum-scores-with-decreasing-weight
  "Sum recommendation scores at each index from idx, stopping at final-idx. Each
  subsequent score has a weight of 1/index. The recommendations should be in the
  sorted form, map of case -> [option maps with :score]"
  [running-sum recommendations idx final-idx recs-length]
  (if (= idx (inc final-idx))
    running-sum
    (recur (+ running-sum
              (if (>= (inc idx) recs-length)
                0.0
                (/
                 (reduce + (map #(:score (nth % idx))
                                (vals recommendations)))
                 (inc idx))))
           recommendations (inc idx) final-idx recs-length)))

(defn target-top-heavy-pull-strategy
  "Pages pull strategy based on linearly increasing the tolerance on distance
  from the target recommendation score (1.0). Top 3 recommendations are
  considered for each case, with descending weight.

  current-recs should be sorted with wrangle/sort-rec-options. Returns binary
  decision on whether to pull a further sample."
  [gov current-recs step-number]
  (let [scored-opts-count (wrangle/cols-row-count current-recs),
        cost-of-recommendation (- 1.0
                                  (if (or (empty? current-recs)
                                          (zero? scored-opts-count))
                                    0.0
                                    (/ (sum-scores-with-decreasing-weight
                                         0.0 current-recs 0 3 scored-opts-count)
                                       scored-opts-count))),
        cost-of-next-pull (* (:score-weakness-tolerance gov) step-number)]
    (> cost-of-recommendation cost-of-next-pull)))

(defn new-governor
  "Create a bare empty governor."
  []
  { :recs-amount 5,
    :score-weakness-tolerance 0.02, :pull-strategy :target-top-heavy,
    :tags-preprocessing
    { :tags preproc/multihot-from-tags
      :number-scale preproc/find-and-apply-z-logistic-scale }})

(defn diagnose-columns-from-source
  "Get diagnostics for columns that are supplied from a give functions.

  For possible column attributes see docs/column-attibutes.md."
  [govern io-settings give-sources]
  (let [sample (first (getter io-settings give-sources))]
    (reduce into
            (map (fn [col-name col]
                   {col-name (diag-all-values col)})
                 (keys sample)
                 (vals sample)))))

(defn update-columns-diagnostics
  "Add columns diagnostic info to the governor."
  [old-govern io-settings option-gives case-gives inter-gives]
  (merge old-govern
         {:option-columns (diagnose-columns-from-source
                            old-govern io-settings option-gives),
          :case-columns (diagnose-columns-from-source
                          old-govern io-settings case-gives),
          :inter-columns (diagnose-columns-from-source
                           old-govern io-settings inter-gives)}))

(defn get-col-groups
  "A helper function for execute-preprocessing-instructions.

  Group columns from multiple column sets if they have the same :group-... tag,
  otherwise put a column under its own :ungroup-... key."
  [accum-groups-map set-taggings set-n]
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
           (rest set-taggings) (inc set-n))))

(defn execute-preprocessing-instructions
  "Apply all functions from tags-table to the columns in col-sets, that are
  indicated by tags in the set-taggings which map column names to data type tags.

  Columns with no tags will be skipped in the output.

  Special tags in the form of :group-XYZ guarantee that all cols tagged this
  way will be seamlessly preprocessed together - for example for encoding tags
  or scaling number features.

  The preprocessing functions get the column sequence as their argument. They
  can return either the resulting vector, or a map of :proc-X -> vector, which
  will then all be renamed to :col-name-X in the final col-sets.

  The metadata of original col-sets will be preserved."
  [tags-table set-taggings col-sets]
  (letfn [(unroll-col-group [result-col-sets group-entries]
            (reduce
              (fn [group-col-sets {:keys [col-name set-n done]}]
                (if (map? done)
                  ;; The multi-column "proc-" case.
                  (let [final-col-names
                        (map (fn [proc-key] (keyword (str
                                                       (name col-name) "-"
                                                       ;; cut "proc-"
                                                       (subs (name proc-key) 5))))
                             (keys done))]
                    (update group-col-sets set-n merge
                            (zipmap final-col-names (vals done))))
                  ;; The base one-vector result case.
                  (assoc-in group-col-sets [set-n col-name] done)))
              result-col-sets
              group-entries)),
          (unroll-col-groups [groups]
            (reduce unroll-col-group
                    (mapv (fn [col-set] (with-meta {} (meta col-set))) col-sets)
                    groups))]
    (unroll-col-groups
      (map
        (fn [group]
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
                            (fn [coll tag] ((get tags-table tag identity)
                                            coll))
                            cols-lumped
                            ;; NOTE: tags must be the same for every column!
                            (get (nth set-taggings (-> group first :set-n))
                                 (-> group first :col-name)))]
            (map-indexed (fn [i entry]
                           (assoc entry :done
                                  (apply wrangle/slice
                                         (into [processed]
                                               (nth set-indices-in-lump i)))))
                         group)))
        (vals (get-col-groups {} set-taggings 0))))))

(defn choose-and-prepare-mill
  [old-govern]
  (merge old-govern
         (let [common-num-feats
               (set/intersection (set (filter :num (:option-columns old-govern)))
                             (set (filter :num (:case-columns old-govern)))),
               common-tags-feats
               (set/intersection (set (filter :tags (:option-columns old-govern)))
                             (set (filter :tags (:case-columns old-govern))))]
           (if (pos? (+ (count common-num-feats) (count common-tags-feats)))
             {:mill :nearest-options :tag-fields common-tags-feats
              :number-fields common-num-feats}
             {:mill :informed-popularity}))))

(defn autogovern
  "Automatically try to select the mill and mark columns as features."
  [old-govern io-settings option-gives case-gives inter-gives]
  (choose-and-prepare-mill
    (update-columns-diagnostics old-govern io-settings
                                option-gives case-gives inter-gives)))
