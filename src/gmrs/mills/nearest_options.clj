(ns gmrs.mills.nearest-options
  (:require
    [clojure.spec.alpha :as s]
    [clojure.set :refer [subset?]]
    [gmrs.math :as math]
    [gmrs.wrangle :as wrangle]))


(s/def ::no-nils (s/coll-of some?))

; TODO: handle nils, no fields supplied
(defn nearest-options-scoring
  "Return a scoring table. It needs to be supplied useful col sets for both
  cases and options, and the separate vectors (columns) of their ids."
  [cases options case-ids option-ids]
  ;(println "CS" (keys cases))
  ;(println "CSI" case-ids)
  ;(println "OP" (keys options))
  ;(println "OPI" option-ids)
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
                           (let [score (math/pearson-correlation case-vec
                                                                 option-vec)]
                             (if (NaN? score) -1.0 score)) })
                       option-ids
                       option-row-vecs))
             case-ids
             (wrangle/cols-as-row-vecs cases)))
      { :cases case-ids :options option-ids
        :io-settings (:io-settings (meta cases)) })))

(defn interacted-cases-mask
  "Get a boolean mask of case IDs indicating whether they appear in the
  interactions."
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
  ([cases options inters gettable-cases gettable-options gettable-inters
    partial-pull-strat]
   (if (pos? (wrangle/cols-row-count cases))
     (nearest-options-from-interactions-mill
       cases options inters ; FIXME: only relevant inters...
       gettable-options gettable-inters
       (map-case-inters cases inters)
       partial-pull-strat 1 nil
       {})
     {}))
  ([cases options inters
    gettable-options gettable-inters
    ;; Case inters map case id -> interactions.
    case-inters
    partial-pull-strat step-number last-step
    recommendations]
  (assert (:io-settings (meta cases)))
  (let [io-settings (:io-settings (meta cases)),
        option-id (io-settings :option-id)
        inter-option (io-settings :inter-option),
        inter-case (io-settings :inter-case),
        recs-excluding-existing-inters
        (wrangle/sorted-with-culled-already-interacted recommendations
                                                       case-inters),
        continue? (partial-pull-strat
                    recs-excluding-existing-inters
                    step-number)]
    (cond
      ;; Not enough inters to assess the cases.
      (and continue? (not= last-step :more-inters)
           (not= (count (filter #(some any? %) (vals case-inters)))
                 (wrangle/cols-row-count cases))) ; TODO: always 1 enough?
      (let [new-inters (wrangle/cols-as-rows
                         (first gettable-inters)), ; expected to be to cases
            new-case-inters (reduce
                              (fn [cs-int-acc inter]
                                (if (get cs-int-acc (inter-case inter))
                                  ;; Add for the case if it's one of the target
                                  ;; cases.
                                  (update cs-int-acc (inter-case inter)
                                          (fn [old] (conj old inter)))
                                  cs-int-acc))
                              case-inters new-inters),
            only-relevant-inters
            (wrangle/records-as-cols
              (filter (fn [inter] (get case-inters (inter-case inter)))
                      new-inters))]
        (tap> {:last-step last-step, :current-step :more-inters,
               :new-data only-relevant-inters :place :nn-from-inters})
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
      (let [more-opts (first gettable-options),
            all-opts (wrangle/stack options more-opts)]
        (if (empty? (dissoc all-opts option-id))
          (do
            (tap> {:last-step last-step, :current-step :more-options,
                   :new-data more-opts :place :nn-from-inters
                   :end-reason "no meaningful options features"})
            recs-excluding-existing-inters)
          (do
            (tap> {:last-step last-step, :current-step :more-options,
                 :new-data more-opts :place :nn-from-inters})
            (recur cases all-opts inters
                   (rest gettable-options) gettable-inters
                   case-inters
                   partial-pull-strat (inc step-number) :more-options
                   recommendations))))

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
                      :new-data case-recs :place :nn-from-inters})
               (with-meta
                 (merge recommendations case-recs)
                 { :cases (vec (set (into (:cases (meta recommendations))
                                          (:cases (meta case-recs)))))
                   :options (vec (set (into (:options (meta recommendations))
                                           (:options (meta case-recs)))))
                   :io-settings (:io-settings (meta options)) })))

      ;; Recommendations OK or a repeated step
      :else (do (tap> {:last-step last-step, :current-step :return-recs
                        :place :nn-from-inters})
                recs-excluding-existing-inters)))))

(defn select-max [reference new-val]
  (max (or reference -1.0) new-val))

(defn nearest-options-from-cases-mill
  "Mill recommending options from cases (aux-cases) that are found and are
  similar to the target ones.

  Case-similarities is a scoring table comparing target cases to aux-cases."
  ([cases options inters gettable-cases gettable-options gettable-inters
    partial-pull-strat]
   (if (pos? (wrangle/cols-row-count cases))
     (nearest-options-from-cases-mill
       cases options inters
       gettable-cases gettable-options gettable-inters
       {} {}
       (map-case-inters cases inters)
       partial-pull-strat 1 nil
       {})
     {}))
   ([cases options inters
     gettable-cases gettable-options gettable-inters
     aux-cases case-similarities
     case-inters ; should be almost none but keep just in case, for target cases
     partial-pull-strat step-number last-step
     recommendations]
  (assert (:io-settings (meta cases)))
  (let [io-settings (:io-settings (meta cases)),
        case-id (io-settings :case-id),
        inter-option (io-settings :inter-option),
        inter-case (io-settings :inter-case),
        recs-excluding-existing-inters
        (wrangle/sorted-with-culled-already-interacted recommendations
                                                       case-inters),
        continue? (partial-pull-strat
                    recs-excluding-existing-inters
                    step-number)]
    (cond
      (and continue? (not= last-step :more-cases)
           (or
             (and
               ;; More cases needed - either 0 or all used for ranking...
               (= (count (:options (meta case-similarities)))
                  (wrangle/cols-row-count aux-cases))
               ;; ...and there is equal or more final recs than aux cases
               (<= (count (:options (meta case-similarities)))
                   (count (:options (meta recommendations)))))
             ;; some target cases only have negative matches
             ;; TODO: trim case-similarities to the top ones per case?
             ;; (to make top-scorings less costly)
             (and (seq recommendations)
                  (zero? (mod step-number 5))
                  (some neg?
                        (vals
                          (wrangle/top-scorings case-similarities))))))
      (let [more-cases (first gettable-cases)]
        (tap> {:last-step last-step, :current-step :more-cases,
               :new-data more-cases :place :nn-from-cases})
        (recur cases options inters
               (rest gettable-cases) gettable-options gettable-inters
               (wrangle/stack aux-cases more-cases) case-similarities
               case-inters
               partial-pull-strat (inc step-number) :more-cases
               recommendations))

      ;; Rank the aux-cases according to their usability.
      (and continue? (not= last-step :rank-cases)
           (< (count (:options (meta case-similarities)))
              (wrangle/cols-row-count aux-cases)))
      (let [more-sims (nearest-options-scoring
                        (dissoc cases case-id) (dissoc aux-cases case-id)
                        (case-id cases) (case-id aux-cases))]
        (tap> {:last-step last-step, :current-step :rank-cases,
               :new-data more-sims :place :nn-from-cases})
        (recur cases options inters
               (rest gettable-cases) gettable-options gettable-inters
               aux-cases
               (with-meta
                 (merge case-similarities more-sims)
                 { :cases (vec (set (into (:cases (meta case-similarities))
                                          (:cases (meta more-sims)))))
                   :options (vec (set (into (:options (meta case-similarities))
                                           (:options (meta more-sims)))))
                   :io-settings (:io-settings (meta cases)) })
               case-inters
               partial-pull-strat (inc step-number) :rank-cases
               recommendations))

      ;; Get more inters - options from aux-cases-relevant inters already used.
      (and continue? (not= last-step :more-inters)
           (let [aux-case-ids-set (set (case-id aux-cases))]
             (subset? (set (inter-option
                             ;; Get the inters relevant to the aux-cases.
                             (wrangle/cols-from-row-mask
                               inters
                               (map aux-case-ids-set (inter-case inters)))))
                      (set (:options (meta recommendations))))))
      (let [new-inters (first gettable-inters),
            all-inters (wrangle/stack inters new-inters)]
        (tap> {:last-step last-step, :current-step :more-inters,
               :new-data new-inters :place :nn-from-cases})
        (recur cases options all-inters
               gettable-cases gettable-options (rest gettable-inters)
               aux-cases case-similarities (map-case-inters cases all-inters)
               partial-pull-strat (inc step-number) :more-inters
               recommendations))

      ;; Create recommendations.
      (and continue? (not= last-step :more-recs))
      (let [usable-inters (wrangle/cols-from-row-mask
                            inters
                            (map #(some #{%}
                                     (:options (meta case-similarities)))
                                 (inter-case inters))),
            new-recs
            (with-meta (reduce
                         (fn [collected-recs inter]
                           (if (some #{(inter-case inter)}
                                     (:options (meta case-similarities)))
                             ;; For the interaction, associate its option with
                             ;; the target cases according to their similarity to
                             ;; the interaction's case.
                             (reduce
                               (fn [recs-for-option target-case-id]
                                 (assoc
                                   recs-for-option
                                   [target-case-id (inter-option inter)]
                                   ;; Update only with a higher value.
                                   (select-max
                                     (get recs-for-option
                                          [target-case-id (inter-option inter)])
                                     (get case-similarities
                                          [target-case-id (inter-case inter)]))))
                               collected-recs (case-id cases))
                             collected-recs))
                         {} (wrangle/cols-as-rows usable-inters))
                       { :cases (case-id cases)
                         :options (inter-option usable-inters)
                         :io-settings (:io-settings (meta cases)) })]
        (tap> {:last-step last-step, :current-step :more-recs,
               :new-data new-recs :place :nn-from-cases})
        (recur cases options inters
               gettable-cases gettable-options gettable-inters
               aux-cases case-similarities case-inters
               partial-pull-strat (inc step-number) :more-recs
               new-recs))

      ;; Recommendations OK or a repeated step
      :else (do (tap> {:last-step last-step, :current-step :return-recs
                       :place :nn-from-cases})
                recs-excluding-existing-inters)))))

(defn nearest-options-type-mill
  "Look at the cases and determine which ones can get recommendations from
  similar options to their interactions, and which (with little interactions)
  have to get recommended options from hopefully similar cases."
  [cases cases-getter-partial options-getter-partial inters-getter-partial
   decs-getter-partial pull-strategy]
  ;; TODO: heuristic of getting five pages of inters, kinda weak
  (assert (:io-settings (meta cases)))
  (let [gettable-cases (cases-getter-partial),
        gettable-options (options-getter-partial),
        gettable-inters (inters-getter-partial),
        test-inters (apply wrangle/stack (take 5 gettable-inters)),
        case-ids-with-inters (interacted-cases-mask cases test-inters)]
    (merge
      (nearest-options-from-cases-mill
        (wrangle/cols-from-row-mask cases (map not case-ids-with-inters))
        {} test-inters
        gettable-cases gettable-options gettable-inters
        pull-strategy)
      (nearest-options-from-interactions-mill
        (wrangle/cols-from-row-mask cases case-ids-with-inters)
        {} test-inters
        gettable-cases gettable-options gettable-inters
        pull-strategy))))
