(ns gmrs.mills.informed-popularity
  (:require [gmrs.wrangle :as wrangle]))

(defn informed-popularity-recommend
  "Recommend random options to every case."
  [cases options ; with :io-settings metadata
   & {:keys [recs-amount] :or { recs-amount 5 }}]
  (assert (:option-id (:io-settings (meta options))))
  (let [option-id-col (:option-id (:io-settings (meta options))),
        option-rows (wrangle/cols-as-rows options)]
    (map #(map (fn [option-row]
                 {(keyword option-id-col) (option-id-col option-row)})
               (repeatedly recs-amount
                           (rand-nth option-rows)))
         cases)))
