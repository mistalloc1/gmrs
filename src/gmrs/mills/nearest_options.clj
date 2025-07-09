(ns gmrs.mills.nearest-options
  (:require
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [fastmath.stats :as stats]
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
  (let [option-id-col (:option-id (:io-settings (meta options))),
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
    (map (fn [case-row]
           (if (not (s/valid? ::no-nils case-row))
             (throw (ex-info "bad case row"
                             {:row case-row :cases case-cols})))
           (let [scores (reduce into
                                (map (fn [option-row]
                                       {option-row
                                        (stats/pearson-r
                                          case-row
                                          (map option-row encoded-fields))})
                                     option-rows))]
             (take
               recs-amount
               (sort-by :score >
                        (map (fn [option-row]
                               {(keyword option-id-col) (option-id-col option-row),
                                :score (get scores option-row)})
                             option-rows)))))
           (wrangle/cols-as-vecs (map case-cols encoded-fields)))))
