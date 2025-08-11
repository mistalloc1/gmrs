(ns gmrs.mills.nearest-options
  (:require
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [gmrs.math :as math]
    [gmrs.wrangle :as wrangle]
    [gmrs.preprocess :as preprocess]))


(s/def ::no-nils (s/coll-of some?))

(defn encoded-columns
  "Encode multihot tag columns and scale number fields. number-fields-to-transfs
  should be a hashmap of column names to transformations from z-logistic-scale.

  Get the map of column names to vectors."
  [cols tag-fields number-fields-to-transfs]
  (apply merge (concat
                 (map
                   (fn [field]
                     (preprocess/multihot-from-tags
                       (cols field) (str (name field) "-")))
                   tag-fields)
                 (map
                   (fn [field]
                     { field
                      (preprocess/apply-z-logistic-scale
                        (cols field)
                        (number-fields-to-transfs field)) })
                   (keys number-fields-to-transfs)))))

; TODO: allow for excluding some fields from tag multihot chopping
; TODO: handle nils, no fields supplied
; TODO: allow for numeric columns where zero is meaningful (and shouldn't
; disappear in scaling)
; TODO: binning
; TODO: optionally reconstruct some case representation from interactions
(defn nearest-options-recommend
  [cases options ; with :io-settings metadata
   ; those we expect from the governor
   & {:keys [tag-fields number-fields]
      :or { tag-fields [], number-fields [] }}]
  (assert (:option-id (:io-settings (meta options))))
  (assert (:case-id (:io-settings (meta cases))))
  (let [option-id-col (:option-id (:io-settings (meta options))),
        case-id-col (:case-id (:io-settings (meta cases))),
        number-fields-to-transfs
        (reduce into (map (fn [field-name]
                            { field-name
                             (preprocess/z-logistic-scale
                               (concat (field-name cases)
                                       (field-name options))) })
                          number-fields)),
        encoded-option-cols (encoded-columns options tag-fields
                                             number-fields-to-transfs),
        encoded-case-cols (encoded-columns cases tag-fields
                                           number-fields-to-transfs),
        ; a consistent order of multihot-encoded fields
        encoded-fields (set/union (set (keys encoded-case-cols))
                                  (set (keys encoded-option-cols))),
        option-cols
        (into options
              (wrangle/fill-missing-cols encoded-option-cols encoded-fields)),
        case-cols
        (into cases
              (wrangle/fill-missing-cols encoded-case-cols encoded-fields)),
        option-rows (wrangle/cols-as-rows option-cols)]
    ; Validation
    (when (not (wrangle/all-same-length? option-cols case-cols))
      (throw (ex-info "not the same number of cases and option encoded cols"
                      {:option-cols (keys option-cols)
                       :case-cols (keys case-cols)})))
    (run! (fn [row] (when (not (s/valid? ::no-nils row))
                      (throw (ex-info "bad option row"
                                      {:row row :options option-cols}))))
          (wrangle/cols-as-vecs (map option-cols encoded-fields)))
    ;; Iterate through cases and then options for computing scores
    ;; Create a scoring table with the appropriate metadata.
    (with-meta
      (reduce
        into {}
        (map (fn [case-id case-vec]
               (when (not (s/valid? ::no-nils case-vec))
                 (throw (ex-info "bad case row"
                                 {:row case-vec :cases case-cols})))
               (map (fn [option-row]
                       { [case-id (option-id-col option-row)]
                         (math/pearson-correlation
                           case-vec
                           (map option-row encoded-fields)) })
                     option-rows))
             (case-id-col cases)
             (wrangle/cols-as-vecs (map case-cols encoded-fields))))
      { :cases (case-id-col cases) :options (option-id-col options)
        :io-settings (:io-settings (meta cases)) })))

(defn nearest-options-from-interactions-mill
  "The mill that will recommend options for cases, assuming that the gettable
  interactions will provide enough interacted options so that similar options
  to them can be suggested.

  It's best to pass the *interactions* already loaded for the assessment to the
  mill. But NOTE if so, all the interactions must be with one of the cases!

  The *pull-strategy* is a function that takes only the current-scores and step
  number. It can be a 'raw' pull strategy function partialled with the guvna.

  Repeating the same *step* is assumed to mean that we received empty data, which
  means that we have to bail with the current recommendations."
  ; TODO:consider the scenario of getting the same loose-options multiple times
  ; TODO:when do we want to retake more interactions?
  ; TODO:inspection or logging
  ([cases inters gettable-inters gettable-options pull-strategy]
   (nearest-options-from-interactions-mill
     cases inters []
     gettable-inters gettable-options
     {} [] []
     pull-strategy 1 nil
     {}))
  ([cases inters options
   gettable-inters gettable-options
   ;; Case inters map case id -> interaction IDs. The opts are only and all the
   ;; ones in the options arg.
   case-inters inter-opt-ids loose-opt-ids
   pull-strategy step-number last-step
   recommendations]
  (let [io-settings (:io-settings (meta options)),
        option-id (io-settings :option-id)
        inter-option (io-settings :inter-option),
        inter-case (io-settings :inter-case),
        continue? (pull-strategy recommendations step-number)]
    (cond
      ;; Not enough inters to assess the cases.
      (and continue? (not= last-step :more-inters)
           (not= (count case-inters) (count cases))) ; TODO: always 1 enough?
      (let [new-inters (first gettable-inters), ; expected to be to cases
            new-case-inters (reduce
                              (fn [m inter]
                                (update m (inter-case inter)
                                        (fn [old] (conj old inter))))
                              case-inters new-inters),
            only-relevant-inters
            (filter (fn [inter] (get case-inters (inter-case inter)))
                    new-case-inters)]
        (recur cases (into inters only-relevant-inters) options
               (rest gettable-inters) gettable-options
               case-inters inter-opt-ids loose-opt-ids
               pull-strategy (inc step-number) :more-inters
               recommendations))

      (and continue? (not= last-step :more-options)
           (or
             ;; More interacted options needed
             (some (complement inter-opt-ids) ; see if their details are unknown
                   ;; set of known inter options:
                   (reduce into #{} (map inter-option inters)))
             ;; More loose recommendable options needed
             (empty? loose-opt-ids)))
      (let [new-opts (first gettable-options),
            new-inter-opt-ids
            (into inter-opt-ids
                  (filter (fn [opt-id]
                            (some #(= % opt-id)
                                  (map inter-option inters)))
                          (map (io-settings :option-id) new-opts))),
            new-loose-opt-ids (filter (complement new-inter-opt-ids)
                                      (map option-id new-opts))]
        (recur cases inters (into options new-opts)
               gettable-inters (rest gettable-options)
               case-inters inter-opt-ids
               (vec (set (into loose-opt-ids new-loose-opt-ids)))
               pull-strategy (inc step-number) :more-options
               recommendations))

      ;; Can recommend more
      (and continue? (not= last-step :more-recs)
           (seq loose-opt-ids))
      (recur cases inters options
             gettable-inters gettable-options
             case-inters inter-opt-ids #{}
             pull-strategy (inc step-number) :more-recs
             ;; The score for an option is always its mean score against
             ;; the known target (already interacted) options. This
             ;; approximation should get more reliable with retries and
             ;; pulling more interactions and options for each case (by
             ;; law of large numbers). (NOTE this remark makes sense if loose
             ;; opts reappear)
             (let [opt-recs (nearest-options-recommend
                              (filter (fn [opt] (inter-opt-ids (option-id opt)))
                                        options)
                              (filter (fn [opt] (loose-opt-ids (option-id opt)))
                                                       options))
                   case-recs (wrangle/options-to-cases-scoring-table
                               opt-recs (zipmap (keys case-inters)
                                                (map #(map inter-option %)
                                                     (vals case-inters))))]
               (with-meta
                 (merge recommendations case-recs)
                  { :cases (vec (set (into (:cases (meta recommendations))
                                           (:cases (meta opt-recs)))))
                    :options (vec (set (into (:options (meta recommendations))
                                             (:options (meta opt-recs)))))
                    :io-settings (:io-settings recommendations) })))

      ;; Recommendations OK or a repeated step
      :else recommendations))))
