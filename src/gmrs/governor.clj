(ns gmrs.governor
  (:require
    [clojure.set :as set]
    [gmrs.io.getters :refer [getter]]
    [gmrs.data-diag :refer [diag-all-values]]
    [gmrs.wrangle :as wrangle]))

(defn sum-scores-with-decreasing-weight
  "Sum recommendation scores at indices from idx to final-idx. The recommendations
  should be as a standard map of case -> [option maps with :score]"
  [running-sum recommendations idx final-idx]
  (if (= idx (inc final-idx))
    running-sum
    (recur (+ running-sum
              (if (>= (inc idx) scored-opts-count)
                0.0
                (/
                 (reduce + (map #(:score (nth % idx))
                                (vals recommendations)))
                 (inc idx))))
           recommendations (inc idx) final-idx)))

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
                                         0.0 current-recs 0 3)
                                       scored-opts-count))),
        cost-of-next-pull (* (:score-weakness-tolerance gov) step-number)]
    (> cost-of-recommendation cost-of-next-pull)))

(defn new-governor
  "Create a bare empty governor."
  []
  { :recs-amount 5
    :score-weakness-tolerance 0.02 :pull-strategy :target-top-heavy })

(defn diagnose-columns-from-source
  "Get column diagnostics.

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
