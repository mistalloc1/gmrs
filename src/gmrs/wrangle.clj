"Change formats of raw data so it is suitable for different operations. As soon
as it cares about the meaning of the data, it should go into preprocess."

(ns gmrs.wrangle
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn all-same-length? [& xs]
  (= 1 (count (set (map count xs)))))

(defn average-score
  "Compute the average score, ignoring nils."
  [coll]
  (let [coll (filter true? coll)]
    (if (empty? coll) 0.0
      (/ (reduce + 0.0 coll) (count coll)))))

(defn cols-row-count
  "Get the length of the column, guaranteeing it's the same everywhere."
  [cols]
  (let [cols (if (map? cols) (vals cols) cols)]
    (if (all-same-length? cols)
      (count (first cols))
      (throw (ex-info "unequal or bad column set"
                      {:cols cols})))))

(defn fill-missing-cols
  "Add aligned columns of zeros for the col-names if missing. Preserve the
  original columns."
  [cols col-names]
  (let [row-count (cols-row-count cols),
        zeros (repeat row-count 0)]
    (reduce into
            (map (fn [col-name]
                   {col-name
                    (or (cols col-name) zeros)})
                 (set/union (keys cols) col-names)))))

(defn cols-as-vecs
  "A seq of individual rows as vectors from the cols seq."
  [cols]
 (let [col-length (cols-row-count cols)]
    (map (fn [idx] (map (fn [col] (nth col idx)) cols))
         (range col-length))))

(defn cols-as-rows
  "A seq of individual rows from the cols map"
  [cols]
 (let [col-length (cols-row-count cols)]
   (map (fn [idx] (reduce into
                          (map
                            (fn [[col-name col]] {col-name (nth col idx)})
                            cols)))
        (range col-length))))

(defn slice
  "Get elements from start until (not including) end. Works on column sets and
  native collections."
  [coll start end]
  (condp apply [coll]
    vector? (subvec coll start end),
    #(and (map? %) (every? vector? (vals %)))
    (reduce into {} (map (fn [[col-name col]]
                           { col-name (subvec col start end) })
                         coll)),
    any? (->> coll
               (drop-last (- (count coll) end))
               (drop start))))

(defn stack
  "Stack the added columnar data on the bottom of the orig data."
  [orig & added]
  (let [first-stacked
        (if (= (set (keys orig)) (set (keys (first added))))
          (reduce into {}
            (map (fn [[col-name orig-objs]]
                  { col-name
                    (concat orig-objs (col-name (first added))) })
                orig))
          (ex-info
            "Cannot merge columnar data"
            { :orig-keys (keys orig)
              :add-keys (keys (first added)) }))]
    (if (= 1 (count added))
      first-stacked
      (recur first-stacked (rest added)))))


; TODO: we could detect calls on data which already columnar
(defn records-as-cols
  "Get data from records in a columnar format. Missing values will be nils."
  ([records] (records-as-cols records {}))
  ([records existing-cols] (records-as-cols records existing-cols
                                            (cols-row-count existing-cols)))
  ([records existing-cols col-length]
   (if (empty? records) existing-cols
     (let [rec (first records),
           new-cols
           (reduce into
                   (map
                     (fn [field-name]
                       { field-name
                        (conj (or (existing-cols field-name)
                                  (vec (repeat col-length nil)))
                              (rec field-name)) })
                     (keys rec))),
           missing-value-cols
           (reduce into
                   (concat [{}] ; ensure we get a map from no cols
                           (map (fn [col-name]
                                  { col-name
                                    (conj (existing-cols col-name) nil) })
                                (filter #(not-any? #{%} (keys new-cols))
                                        (keys existing-cols)))))]
       (recur (rest records)
              (merge new-cols missing-value-cols)
              (inc col-length))))))

(defn sorted-rec-options
  "Given a scoring table, return a map of cases to vectors of maps { (options id)
  :score } sorted by :score descending."
  [scoring-table]
  (reduce into {}
          (map (fn [case-id]
                 { case-id
                   (sort-by
                     :score >
                     (map (fn [opt-id] { (:option-id (:io-settings
                                                       (meta scoring-table)))
                                         opt-id,
                                         :score (get scoring-table
                                                     [case-id opt-id]) })
                          (get (meta scoring-table) :options))) })
               (get (meta scoring-table) :cases))))

(defn options-to-cases-scoring-table
  "Given a scoring table made option-to-option, derive scores for the recommended
  options applicable when recommending them for the cases; do this by averaging
  the scores for the options associated with the case."
  [scoring-table cases-options]
  (with-meta
    (reduce
      into {}
      (map (fn [[case-id case-assoc-options]]
             (reduce
               into {}
               (map (fn [rec-opt-id]
                      { [case-id rec-opt-id]
                        (average-score
                          (map (fn [ass-opt-id]
                                 (if-let [scoring (get scoring-table
                                                       [ass-opt-id rec-opt-id])]
                                   (:score scoring)
                                   nil))
                               case-assoc-options)) })
                    (:options (meta scoring-table)))))
           cases-options))
    { :cases (keys cases-options) :options (:options (meta scoring-table))
      :io-settings (:io-settings (meta scoring-table)) }))

(defn keywordify
  "Get list of keywords corresponding to the names (strings). They correspond to
  the strings exactly if possible, but non-alphanumeric characters outside of
  -_\"'?<>=!+* will be replaced with dashes (-). If the would be collisions, the
  keywords will get suffixes like -001, -002 etc."
  [names]
  (letfn [(get-base-form [string]
            (str/replace string
                         #"[^\d\p{IsAlphabetic}-_\"'?<>=!+*]"
                         "-"))
          (keywordify-step [remaining-names keywords keyword-map]
            (if (empty? remaining-names)
              keywords
              (let [base-form (get-base-form (first remaining-names))]
                (if (get keyword-map base-form)
                  (recur (rest remaining-names)
                         (conj keywords
                               (keyword
                                 (str base-form "-"
                                      (format "%03d"
                                              (get keyword-map base-form)))))
                         (update keyword-map base-form inc))
                  (recur (rest remaining-names)
                         (conj keywords (keyword base-form))
                         (assoc keyword-map base-form 1))))))]
    (keywordify-step names [] {})))
