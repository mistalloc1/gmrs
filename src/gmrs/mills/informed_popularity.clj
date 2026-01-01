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

(defn merge-ctrs
  "Collate info from ctr-1 and ctr-2. Return a map with :result of the merge
  and :mean-diff of CTRs between the two (computed between different options).
  For this calculation, we assume a default prior of 0.1 CTR.

  For each opt-id, we will add a :ctr entry with the new calculation. The ctr-1
  should have the :ctr's for its entries."
  [ctr-1 ctr-2]
  (let [result (reduce
                 (fn [ctr-accum [opt-id new-entry]]
                   (update-in
                     ctr-accum [opt-id]
                     (fn [old-entry]
                       (let [merge-entry
                             { :dec-ids (into (or (:dec-ids old-entry) #{})
                                              (:dec-ids new-entry)),
                               :unseen-dec-ids
                               (into (or (:unseen-dec-ids old-entry) #{})
                                     (:unseen-dec-ids new-entry)),
                               :inter-count (+ (or (:inter-count old-entry) 0)
                                               (:inter-count new-entry)) }]
                         (assoc merge-entry :ctr
                                (/
                                 (:inter-count merge-entry)
                                 (count (:dec-ids merge-entry))))))))
                   ctr-1
                   ctr-2),
        diff-accum (reduce (fn [acc opt-id]
                             (let [ctr-at-1 (-> ctr-1 (get opt-id)
                                                    (get :ctr 0.1)),
                                   ctr-at-result (-> result (get opt-id)
                                                    (get :ctr 0.1))]
                               (+ acc (abs (- ctr-at-1 ctr-at-result)))))
                           0.0 (into (set (keys ctr-1))
                                     (keys ctr-2)))]
    { :result result,
      :mean-diff (if-let [cnt (when (seq result) (count (keys result)))]
                   (/ diff-accum cnt)
                   0.0) } ))

(defn top-click-through-rate
  "One round of updating CTRs.

  Returns a map with the :result (new CTRs map), the :confidence, dependent
  on the mean change of individual CTRs, and :unpaired-inters-count, which is the
  number of interactions that were seen but not their linked decisions.

  Start-ctr may be a map of option id -> <a map of :dec-ids :inter-count :ctr
  :unseen-dec-ids> to update with the new data. :ctr = :inter-count /
  (count :dec-ids). :Dec-ids were already seen and counted. :Unseen-dec-ids are
  decisions for which we already saw the interaction, i.e. the click,
  but we cannot count them before seeing and counting the decision itself.

  Inters and decs should be the already obtained items in the columnar format.
  They are assumed to be relevant and usable in calculations but won't be
  deduplicated against what we get from getters. After these are consumed once,
  don't provide them again.

  The getters should be already set to the desired timeframe and to having
  non-nil decision IDs on inters. Pages amount is how many pages to take, which
  would also the number of 'steps' taken for pull strategies."
  [start-ctr unpaired-inters-count
   inters decs
   inters-getter decs-getter pages-amount]
  (let [new-unpaired-inters-count (atom unpaired-inters-count),
        new-inters (apply wrangle/stack
                          (if (seq inters) inters {})
                          (take pages-amount inters-getter)),
        new-decs (apply wrangle/stack
                        (if (seq decs) decs {})
                        (take pages-amount decs-getter)),
        io-settings (:io-settings (meta new-inters)),
        inter-option (io-settings :inter-option),
        inter-dec-id (io-settings :inter-dec-id),
        dec-id (io-settings :dec-id),
        dec-options (io-settings :dec-options),
        ctr-opts-clicks (reduce
                          (fn [ctr-acc inter]
                            (update-in ctr-acc [(inter-option inter)]
                                       (fn [opt-entry]
                                         (swap! new-unpaired-inters-count inc)
                                         (update-in opt-entry [:unseen-dec-ids]
                                                    conj (inter-dec-id inter)))))
                          {} new-inters),
        new-ctr-with-decs
        (reduce
          (fn [ctr-acc dec]
            (reduce (fn [dec-ctr-acc opt-id]
                      (let [old-inter-count (-> dec-ctr-acc (get opt-id)
                                                (get :inter-count 0)),
                            old-unseen-decs (-> dec-ctr-acc (get opt-id)
                                                (get :unseen-dec-ids #{})),
                            new-unseen-decs (remove #{(dec-id dec)}
                                                    old-unseen-decs),
                            new-inter-count (if
                                              (< (count new-unseen-decs)
                                                 (count old-unseen-decs))
                                              (do
                                                (swap!
                                                  new-unpaired-inters-count dec)
                                                (inc old-inter-count))
                                              old-inter-count)]
                        (update-in dec-ctr-acc [opt-id]
                                   (fn [entry]
                                     {:dec-ids (conj (set (:dec-ids entry))
                                                     (dec-id dec))
                                      :unseen-dec-ids new-unseen-decs
                                      :inter-count new-inter-count}))))
                    ctr-acc (dec-options dec)))
          ctr-opts-clicks
          new-decs),
        ctr-merge (merge-ctrs start-ctr new-ctr-with-decs)]
    { :result (:result ctr-merge),
      :confidence (- 1.0 (:mean-diff ctr-merge)),
      :unpaired-inters-count @new-unpaired-inters-count }))

(defn informed-popularity-exploit-mill
  "Recommend most popular options for case audience segments."
  [cases cases-getter-partial options-getter-partial inters-getter-partial
   decs-getter-partial pull-strategy
   time-end time-window]
  ;; TODO: audience segments are for later
  )
