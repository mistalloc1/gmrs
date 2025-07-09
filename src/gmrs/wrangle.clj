"Change formats of raw data so it is suitable for different operations. As soon
as it cares about the meaning of the data, it should go into preprocess."

(ns gmrs.wrangle
  (:require
    [clojure.set :as set])
  (:use [clojure.test :only [is]]))

(defn all-same-length? [& xs]
  (= 1 (count (set (map count xs)))))

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
