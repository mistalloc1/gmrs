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
   & {:keys [tag-fields number-fields recs-amount]
      :or { tag-fields [], number-fields [], recs-amount 5 }}]
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
    (if (not (wrangle/all-same-length? option-cols case-cols))
      (throw (ex-info "not the same number of cases and option encoded cols"
                      {:option-cols (keys option-cols)
                       :case-cols (keys case-cols)})))
    (run! (fn [row] (if (not (s/valid? ::no-nils row))
                      (throw (ex-info "bad option row"
                                      {:row row :options option-cols}))))
          (wrangle/cols-as-vecs (map option-cols encoded-fields)))
    ; Iterate through cases and then options for computing scores
    (reduce
      into {}
      (map (fn [case-id case-vec]
             (if (not (s/valid? ::no-nils case-vec))
               (throw (ex-info "bad case row"
                               {:row case-vec :cases case-cols})))
             { case-id
               (map (fn [option-row]
                     {
                      (keyword option-id-col)
                      (option-id-col option-row),
                      :score (math/pearson-correlation
                                case-vec
                                (map option-row encoded-fields))
                      })
                   option-rows) })
           (case-id-col cases)
           (wrangle/cols-as-vecs (map case-cols encoded-fields))))))

(defn nearest-options-from-interactions-mill
  "The mill that will recommend options for cases, assuming that the gettable
  interactions will provide enough interacted options so that similar options
  to them can be suggested.

  It's best to pass the *interactions* already loaded for the assessment to the
  mill.

  The *pull-strategy* is a function that takes only the current-scores and step
  number. It can be a 'raw' pull strategy function partialled with the guvna.

  Repeating the same *step* is assumed to mean that we received empty data, which
  means that we have to bail with the current recommendations."
  ; TODO:consider the scenario of getting the same loose-options multiple times
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
   case-inters inter-opt-ids loose-opt-ids
   pull-strategy step-number last-step
   recommendations]
  (let [io-settings (:io-settings (meta options)),
        inter-option (io-settings :inter-option),
        inter-case (io-settings :inter-case)
        continue? (pull-strategy recommendations step-number)]
    (cond
      ;; Not enough inters to assess the cases.
      (and continue? (not= last-step :more-inters)
           (not= (count case-inters) (count cases)))
      (let [new-inters (first gettable-inters), ; expected to be to cases
            new-case-inters (reduce
                              (fn [m inter-n]
                                (update m (inter-case (nth inter-n new-inters))
                                        (fn [old] (conj old inter-n))))
                              case-inters
                              (range (count new-inters)))]
        (recur cases (into inters new-inters) options
               (rest gettable-inters) gettable-options
               case-inters inter-opt-ids loose-opt-ids
               pull-strategy (inc step-number) :more-inters
               recommendations))

      (and continue? (not= last-step :more-options)
           (or
             ;; More interacted options needed
             (some (complement inter-opt-ids) ; see if their details are unknown
                   ;; set of known inter options:
                   (reduce into #{} (map (fn [[_ inters]]
                                           (map inter-option inters))
                                         case-inters)))
             ;; More loose recommendable options needed
             (empty? loose-options)))
      (let [new-opts (first gettable-options),
            new-inter-opt-ids
            (into inter-opt-ids
                  (filter (fn [opt-id]
                            (some = (map inter-option
                                         (filter (vec (vals case-inters))
                                                 inters))))
                          (map (io-settings :option-id) new-opts))),
            new-loose-opt-ids (filter (complement new-inter-opt-ids)
                                      (map (io-settings :option-id) new-opts))]
        (recur cases inters (into options new-opts)
               gettable-inters (rest gettable-options)
               case-inters inter-opt-ids loose-opt-ids
               pull-strategy (inc step-number) :more-options
               recommendations))

      ;; Can recommend more
      (and continue? (not= last-step :more-recs)
           (not (empty? loose-options)))
      (recur cases inters options
             gettable-inters gettable-options
             case-inters inter-opt-ids #{}
             pull-strategy (inc step-number) :more-recs
             (into recommendations
                   (let [opt-recs (nearest-options-recommend interacted-options
                                                             loose-options)]
                     (reduce
                       into {}
                       ; FIXME: update/concat!
                       (map (fn [[case-id inters]]
                              { case-id
                                (filter (fn [rec] (some #(= (:target-id rec)
                                                            (inter-option %))
                                                        inters))
                                       opt-recs) })
                            case-inters)))))

      ;; Recommendations OK or a repeated step
      :else recommendations))))
