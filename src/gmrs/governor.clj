(ns gmrs.governor
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [gmrs.io.getters :as get]
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
  (when (< step-number 2000)
    (let [scored-opts-count (wrangle/cols-row-count current-recs),
          cost-of-recommendation (- 1.0
                                    (if (or (empty? current-recs)
                                            (zero? scored-opts-count))
                                      0.0
                                      (/ (sum-scores-with-decreasing-weight
                                           0.0 current-recs 0 3 scored-opts-count)
                                         scored-opts-count))),
          cost-of-next-pull (* (:score-weakness-tolerance gov) step-number)]
      (> cost-of-recommendation cost-of-next-pull))))

(def DefaultProcessing
  { :tags preproc/MultihotFromTags
    :float-needs-conv (preproc/safe-parse #(Float/parseFloat %) 0.0)
    :integer-needs-conv (preproc/safe-parse #(Integer/parseInt (str/trim %)) 0)
    :date-local-needs-conv (preproc/safe-parse #(java.time.LocalDate/parse %))
    :time-local-needs-conv (preproc/safe-parse #(java.time.LocalTime/parse %))
    :date-zoned-with-time-needs-conv (preproc/safe-parse
                                       #(java.time.ZonedDateTime/parse %))
    :float preproc/ZLogisticScale
    :integer preproc/ZLogisticScale })

(defn new-governor
  "Create a bare empty governor."
  []
  { :recs-amount 5,
    :score-weakness-tolerance 0.02, :pull-strategy :target-top-heavy,
    :tags-preprocessing DefaultProcessing})

(defn diagnose-columns-from-source
  "Get diagnostics for columns that are supplied from a give functions.

  For possible column attributes see docs/column-attibutes.md."
  [getter]
  (let [sample (first getter)]
    (reduce into
            (map (fn [col-name col]
                   {col-name (diag-all-values col)})
                 (keys sample)
                 (vals sample)))))

(defn update-columns-diagnostics
  "Add columns diagnostic info to the governor."
  [old-govern io-settings io-setup]
  (merge old-govern
         {:option-columns (diagnose-columns-from-source
                            (get/get-options io-settings io-setup)),
          :case-columns (diagnose-columns-from-source
                          (get/get-cases io-settings io-setup)),
          :inter-columns (diagnose-columns-from-source
                           (get/get-inters io-settings io-setup))}))

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
             {:mill :nearest-options}
             {:mill :informed-popularity}))))

(defn autogovern
  "Automatically try to select the mill and mark columns as features."
  [old-govern io-settings io-setup]
  (choose-and-prepare-mill
    (update-columns-diagnostics old-govern io-settings io-setup)))
