(ns gmrs.governor
  (:require
    [clojure.set :as set]
    [gmrs.wrangle :as wrangle]))

(defn new-governor
  "Create a bare empty governor."
  []
  { :recs-amount 5 })

(defn probe-sources
  "Get a (hopefully representative) amount of records from the source."
  [sources amount]
  (wrangle/records-as-cols
    (take amount
          (reduce into sources))))

(defn diagnose-columns-from-source
  "Get column diagnostics.

  For possible column attributes see docs/column-attibutes.md."
  [govern sources]
  (let [sample-size (:recs-amount govern),
        sample (probe-sources sources sample-size)]
    (reduce into
            (map (fn [col-name col]
                   {col-name
                    (filter some?
                            [(if (every? number? col) :num)
                             (if (every? string? col) :tags)])})
                 (keys sample)
                 (vals sample)))))

(defn update-columns-diagnostics
  "Add columns diagnostic info to the governor."
  [old-govern option-gives case-gives inter-gives]
  (merge old-govern
         {:option-columns (diagnose-columns-from-source old-govern option-gives),
          :case-columns (diagnose-columns-from-source old-govern case-gives),
          :inter-columns (diagnose-columns-from-source old-govern inter-gives)}))

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
  [old-govern option-gives case-gives inter-gives]
  (choose-and-prepare-mill
    (update-columns-diagnostics old-govern option-gives case-gives inter-gives)))
