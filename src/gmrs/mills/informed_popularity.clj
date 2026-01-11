(ns gmrs.mills.informed-popularity
  (:require [clojure.math :refer [floor]]
            [tick.core :as t]
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

(defn ctr-mean-diff
  "Compute mean absolute CTR difference between ctr-old and ctr-new.
  Uses a default prior of 0.1 when CTR is missing."
  [ctr-old ctr-new]
  (let [opt-ids (into (set (keys ctr-old)) (keys ctr-new))
        diff-accum
        (reduce (fn [acc opt-id]
                  (let [ctr-at-old (-> ctr-old (get opt-id) (get :ctr 0.1)),
                        ctr-at-new (-> ctr-new (get opt-id) (get :ctr 0.1))]
                    (+ acc (abs (- ctr-at-old ctr-at-new)))))
                0.0 opt-ids)]
    (if (seq opt-ids)
      (/ diff-accum (count opt-ids))
      0.0)))

(defn click-through-rate-update
  "One round of updating CTRs.

  Returns a map with the :result (new CTRs map), the :confidence, dependent
  on the mean change of individual CTRs, and :unpaired-inters-count, which is the
  number of interactions that were seen but not their linked decisions.

  Start-ctr may be a map of option id -> <a map of :dec-ids :inter-count :ctr
  :unseen-dec-ids> to update with the new data. :ctr = :inter-count /
  (count :dec-ids). :Dec-ids were already seen and counted. :Unseen-dec-ids are
  decisions for which we already saw the interaction, i.e. the click,
  but we cannot count them before seeing and counting the decision itself.
  :Dec-ids and :inter-count are optional: may not occur if only decisions but
  not inters were seen for the option.

  Inters and decs should be the already obtained items in the columnar format.
  They are assumed to be relevant and usable in calculations but won't be
  deduplicated against what we get from getters. After these are consumed once,
  don't provide them again.

  The getters should be already set to the desired timeframe and to having
  non-nil decision IDs on inters. Pages amount is how many pages to take, which
  would also the number of 'steps' taken for pull strategies."
  [start-ctr unpaired-inters-count
   inters decs
   gettable-inters gettable-decs pages-amount]
  (let [new-unpaired-inters-count (atom unpaired-inters-count),
        ;; Note that these are multiple pages and need to be stacked before
        ;; using, getting their Clojure metadata etc.
        [new-inters left-inters] (split-at pages-amount gettable-inters),
        [new-decs left-decs] (split-at pages-amount gettable-decs),
        inters-to-use (apply wrangle/stack
                             (if (seq inters) inters {})
                             new-inters),
        decs-to-use (apply wrangle/stack
                           (if (seq decs) decs {})
                           new-decs),
        io-settings (:io-settings (meta inters-to-use)),
        inter-option (io-settings :inter-option),
        inter-dec-id (io-settings :inter-dec-id),
        dec-id (io-settings :dec-id),
        dec-options (io-settings :dec-options),
        ctr-after-inters
        (reduce
          (fn [ctr-acc inter]
            (if-let [dec-id (inter-dec-id inter)]
              (update-in ctr-acc [(inter-option inter)]
                         (fn [opt-entry]
                           (if (some #{dec-id} (:dec-ids opt-entry))
                             ;; Interaction for already seen decision.
                             (update opt-entry :inter-count inc)
                             ;; Interaction for yet unseen decision.
                             (do
                               (swap! new-unpaired-inters-count inc)
                               (merge opt-entry
                                      {:unseen-dec-ids
                                       (conj
                                         (or (:unseen-dec-ids opt-entry) #{})
                                         dec-id)
                                       ;; Make sure at least a CTR is present.
                                       :ctr (or (:ctr opt-entry) 0.0)})))))
              ctr-acc))
          start-ctr
          (wrangle/cols-as-rows inters-to-use)),
        ;; Add information from decisions for the final CTR from this update.
        final-ctr (reduce
                    (fn [ctr-acc decision]
                      ;; Update for the options recommended in the decision,
                      ;; especially when we saw inters associated with that
                      ;; decision and haven't counted them yet.
                      (reduce
                        (fn [dec-ctr-acc opt-id]
                          (update-in
                            dec-ctr-acc [opt-id]
                            (fn [entry]
                              (let [entry (or entry {}),
                                    old-unseen (:unseen-dec-ids entry #{}),
                                    new-unseen (disj old-unseen
                                                     (dec-id decision)),
                                    dec-marked-unseen? (< (count new-unseen)
                                                          (count old-unseen)),
                                    new-inter-count
                                    (if dec-marked-unseen?
                                      (do (swap! new-unpaired-inters-count dec)
                                          (inc (or (:inter-count entry) 0)))
                                      (or (:inter-count entry) 0)),
                                    new-dec-ids (conj (set (:dec-ids entry))
                                                      (dec-id decision))]
                                {:dec-ids new-dec-ids
                                 :unseen-dec-ids new-unseen
                                 :inter-count new-inter-count
                                 :ctr (if
                                        ;; Zero can happen when the opt is only
                                        ;; known from inters, but no decisions.
                                        (zero? (count new-dec-ids))
                                        0.0
                                        (/ new-inter-count (count new-dec-ids)))}))))
                        ctr-acc (dec-options decision)))
                    ctr-after-inters
                    (wrangle/cols-as-rows decs-to-use)),
        mean-diff (ctr-mean-diff start-ctr final-ctr)]
    { :result final-ctr,
      :confidence (- 1.0 mean-diff
                     (* 0.3 (/ @new-unpaired-inters-count
                               (if (pos? (count inters-to-use))
                                 (count inters-to-use)
                                 1)))),
      :unpaired-inters-count @new-unpaired-inters-count
      :left-inters left-inters :left-decs left-decs }))

(defn recommendations-from-ctr
  "From CTR from click-through-rate-update and confidence factor, create a map of
  recommendations in the standard format - mapping case IDs to lists of maps of
  :score and opt ID."
  [case-ids ctr confidence opt-id-name]
  (let [common-ranking
        (map
          (fn [[opt-id opt-entry]]
            {opt-id-name opt-id :score (* (:ctr opt-entry) confidence)})
          ctr)]
    (reduce (fn [acc case-id]
              (assoc acc case-id common-ranking))
            {}
            case-ids)))

(defn ctr-with-decisions-mill
  "Recommend based on bulding CTRs from available decisions and inters."
  ;; TODO: canonical mill arglist
  ([cases decs inters
    gettable-decs gettable-inters partial-pull-strat step-number
    ctr unpaired-inters-count
    recommendations]
   (let [continue? (partial-pull-strat recommendations step-number)]
     (if (not continue?)
       recommendations
       (let [pages-amount 5,
             case-id-col (-> cases meta :io-settings :case-id),
             new-ctr-info (click-through-rate-update
                            ctr unpaired-inters-count
                            inters decs
                            gettable-inters gettable-decs pages-amount)
             new-recs (recommendations-from-ctr
                        (case-id-col cases)
                        (:result new-ctr-info)
                        (:confidence new-ctr-info)
                        (-> cases meta :io-settings :option-id))]
         (recur cases [] [] ;; passed decs and inters are consumed
                (:left-decs new-ctr-info) (:left-inters new-ctr-info)
                partial-pull-strat (+ step-number pages-amount)
                new-recs
                (:result new-ctr-info) (:unpaired-inters-count new-ctr-info)))))))

(defn informed-popularity-exploit-mill
  "Recommend most popular options for case audience segments.

  Mill specific options are:
    time-end - a Tick datetime object, by default now
    time-window - a Tick duration object, by default 24 hours

  The data will be taken from until the time-end, going back the time-window."
  ;; The basic mill call with no config.
  ([cases cases-getter-partial options-getter-partial inters-getter-partial
   decs-getter-partial pull-strategy]
   (let [time-end (t/date-time)
         time-window (t/of-hours 24)]
     (informed-popularity-exploit-mill
       cases cases-getter-partial options-getter-partial inters-getter-partial
       decs-getter-partial pull-strategy
       time-end time-window)))
  ;; External mill call with config.
  ([cases cases-getter-partial options-getter-partial inters-getter-partial
   decs-getter-partial pull-strategy
   time-end time-window])
  ;; TODO: audience segments are for later
  ;; Decide to use either ctr-with-decisions-mill or interactions.
  )
