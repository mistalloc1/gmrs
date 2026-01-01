(ns gmrs.mills.informed-popularity
  (:require [clojure.math :refer [floor]]
            [gmrs.math :refer [*gmrs-random*]]
            [gmrs.wrangle :as wrangle]))

(defn random-option-mill
  "Recommend random options to every case. Note that it returns infinite lazy
  sequence of recommendations for each case!"
  [cases cases-getter-partial options-getter-partial inters-getter-partial
   decs-getter-partial pull-strategy]
  (assert (:io-settings (meta cases)))
  (let [case-id-col (-> cases meta :io-settings :case-id),
        option-id-col (-> cases meta :io-settings :option-id),
        gettable-options (options-getter-partial),
        opts-pool (option-id-col
                    (apply wrangle/stack
                         (take (wrangle/cols-row-count cases)
                               gettable-options))),
        random-opt-id (fn []
                        (let [idx (floor
                                    ;; We don't +1 the count because the indices
                                    ;; have to be zero-based.
                                    (* (*gmrs-random*) (count opts-pool)))]
                          (nth opts-pool idx)))]
    (reduce (fn [rec-acc case-id]
              (assoc rec-acc case-id
                     (repeatedly (fn []
                                   {option-id-col (random-opt-id),
                                    :score 0.25}))))
            {}
            (case-id-col cases))))

(defn informed-popularity-exploit-mill
  "Recommend most popular options for case audience segments."
  [cases cases-getter-partial options-getter-partial inters-getter-partial
   decs-getter-partial pull-strategy])
