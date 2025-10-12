(ns gmrs.mills.nearest-options
  (:require
    [clojure.spec.alpha :as s]
    [gmrs.math :as math]
    [gmrs.wrangle :as wrangle]))


(s/def ::no-nils (s/coll-of some?))

; TODO: handle nils, no fields supplied
(defn nearest-options-scoring
  "Return a scoring table. It needs to be supplied useful col sets for both
  cases and options, and the separate vectors (columns) of their ids."
  [cases options case-ids option-ids]
  (assert (= (wrangle/cols-row-count cases) (count case-ids)))
  (assert (= (wrangle/cols-row-count options) (count option-ids)))
  (let [option-row-vecs (wrangle/cols-as-row-vecs options)]
    ;; Iterate through cases and then options for computing scores
    ;; Create a scoring table with the appropriate metadata.
    (with-meta
      (reduce
        into {}
        (map (fn [case-id case-vec]
               (when (not (s/valid? ::no-nils case-vec))
                 (throw (ex-info "bad case row"
                                 {:row case-vec :cases cases})))
                 (map (fn [option-id option-vec]
                         { [case-id option-id]
                           (math/pearson-correlation case-vec option-vec) })
                       option-ids
                       option-row-vecs))
             case-ids
             (wrangle/cols-as-row-vecs cases)))
      { :cases case-ids :options option-ids
        :io-settings (:io-settings (meta cases)) })))

(defn interacted-cases-mask
  [cases inters]
  (let [io-settings (:io-settings (meta cases))]
    (map (set ((:inter-case io-settings) inters))
         ((:case-id io-settings) cases))))

(defn map-case-inters
  "Get a map of case IDs to IDs of inters that are associated."
  [cases inters]
  (let [io-settings (:io-settings (meta cases))
        inter-case-col (:inter-case io-settings)]
    (reduce
      (fn [case-inters inter]
        ;; If the interaction's case is one of the targets, update its entry
        ;; in case-inters.
        (if (get case-inters (inter-case-col inter))
          (update case-inters (inter-case-col inter)
                  conj inter)
          case-inters))
      (zipmap ((:case-id io-settings) cases)
              (repeat []))
      (wrangle/cols-as-rows inters))))

(defn nearest-options-from-interactions-mill
  "The mill that will recommend options for cases, assuming that the gettable
  interactions with these cases will provide enough interacted options so that
  similar options to them can be suggested. This means that the cases should
  have some interactions.

  It's best to pass the *interactions* already loaded for the assessment to the
  mill. But NOTE if so, all the interactions must be with one of the cases!

  The *partial-pull-strat* is a function that takes only the current-scores and step
  number. It can be a 'raw' pull strategy function partialled with the guvna.

  Repeating the same *step* is assumed to mean that we received empty data, which
  means that we have to bail with the current recommendations."
  ; TODO:consider the scenario of getting the same loose-options multiple times
  ; TODO:when do we want to retake more interactions?
  ; TODO:inspection or logging
  ([cases options inters gettable-options gettable-inters partial-pull-strat]
   (nearest-options-from-interactions-mill
     cases options inters
     gettable-options gettable-inters
     (map-case-inters cases inters)
     partial-pull-strat 1 nil
     {}))
  ([cases options inters
    gettable-options gettable-inters
    ;; Case inters map case id -> interaction IDs. The opts are only and all the
    ;; ones in the options arg, "inter" have interacted with the cases, the "loose"
    ;; ones not.
    case-inters
    partial-pull-strat step-number last-step
    recommendations]
  (assert (:io-settings (meta cases)))
  (let [io-settings (:io-settings (meta cases)),
        option-id (io-settings :option-id)
        inter-id (io-settings :inter-id),
        inter-option (io-settings :inter-option),
        inter-case (io-settings :inter-case),
        continue? (partial-pull-strat
                    (wrangle/sorted-with-culled-already-interacted
                      recommendations case-inters)
                    step-number)]
    (cond
      ;; Not enough inters to assess the cases.
      (and continue? (not= last-step :more-inters)
           (not= (count case-inters)
                 (wrangle/cols-row-count cases))) ; TODO: always 1 enough?
      (let [new-inters (wrangle/cols-as-rows
                         (first gettable-inters)), ; expected to be to cases
            new-case-inters (reduce
                              (fn [m inter]
                                (if (get m (inter-case inter))
                                  (update m (inter-case inter)
                                          (fn [old] (conj old inter)))
                                  m))
                              case-inters new-inters),
            only-relevant-inters
            (wrangle/records-as-cols
              (filter (fn [inter] (get case-inters (inter-case inter)))
                      new-inters))]
        (tap> {:last-step last-step, :current-step :more-inters,
               :new-data only-relevant-inters})
        (recur cases options (wrangle/stack inters
                                            only-relevant-inters)
               gettable-options (rest gettable-inters)
               new-case-inters
               partial-pull-strat (inc step-number) :more-inters
               recommendations))

      (and continue? (not= last-step :more-options)
           ;; More options needed - either 0 or all used for recommendations
           (= (count (:options (meta recommendations)))
              (wrangle/cols-row-count options)))
      (let [more-opts (first gettable-options)]
        (tap> {:last-step last-step, :current-step :more-options,
               :new-data more-opts})
        (recur cases (wrangle/stack options more-opts) inters
               (rest gettable-options) gettable-inters
               case-inters
               partial-pull-strat (inc step-number) :more-options
               recommendations))

      ;; Can recommend more
      (and continue? (not= last-step :more-recs)
           (< (count (:options (meta recommendations)))
              (wrangle/cols-row-count options)))
      (recur cases options inters
             gettable-options gettable-inters
             case-inters
             partial-pull-strat (inc step-number) :more-recs
             ;; The score for an option is always its mean score against
             ;; the known target (already interacted) options. This
             ;; approximation should get more reliable with retries and
             ;; pulling more interactions and options for each case (by
             ;; law of large numbers). (NOTE this remark makes sense if loose
             ;; opts reappear)
             (let [opt-recs (nearest-options-scoring
                              (dissoc options option-id)
                              (dissoc options option-id)
                              (option-id options)
                              (option-id options)),
                   case-opts (reduce (fn [case-to-opts inter]
                                         (update case-to-opts (inter-case inter)
                                                 conj (inter-option inter)))
                                       {}
                                       (wrangle/cols-as-rows inters)),
                   case-recs (wrangle/options-to-cases-scoring-table
                               opt-recs case-opts)]
               (tap> {:last-step last-step, :current-step :more-recs,
                      :new-data case-recs})
               (with-meta
                 (merge recommendations case-recs)
                 { :cases (vec (set (into (:cases (meta recommendations))
                                          (:cases (meta case-recs)))))
                   :options (vec (set (into (:options (meta recommendations))
                                           (:options (meta case-recs)))))
                   :io-settings (:io-settings (meta options)) })))

      ;; Recommendations OK or a repeated step
      :else (do (tap> {:last-step last-step, :current-step :return-recs*})
                (wrangle/sorted-with-culled-already-interacted
                  recommendations case-inters))))))

(defn nearest-options-from-cases-mill
  ([cases options inters gettable-options gettable-inters pull-strategy]
   (assert (:io-settings (meta cases)))
   (let [case-id-col (:case-id (:io-settings (meta cases)))]
     ;; TODO: for now only mock some return values
     (map (fn [case-id] { [case-id :case-near-opt-marker] 1.0 })
        (case-id-col cases)))))

(defn nearest-options-type-mill
  "Look at the cases and determine which ones can get recommendations from
  similar options to their interactions, and which (with little interactions)
  have to get recommended options from hopefully similar cases."
  [cases options inters gettable-options gettable-inters pull-strategy]
  ;; TODO: heuristic of getting two pages of inters, kinda weak
  (assert (:io-settings (meta cases)))
  (let [more-inters (wrangle/stack inters (first gettable-inters)),
        case-ids-with-inters (interacted-cases-mask
                               cases
                               (wrangle/stack inters more-inters))]
    (println "Not interacted:" (wrangle/cols-from-row-mask cases (map not case-ids-with-inters)))
    (merge
      (nearest-options-from-cases-mill
        (wrangle/cols-from-row-mask cases (map not case-ids-with-inters))
        options more-inters gettable-options gettable-inters
        pull-strategy)
      (nearest-options-from-interactions-mill
        (wrangle/cols-from-row-mask cases case-ids-with-inters)
        options more-inters gettable-options gettable-inters
        pull-strategy))))
