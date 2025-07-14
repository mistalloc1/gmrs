(ns gmrs.io.csv
  (:require [clojure.data.csv :as csv]
            [gmrs.wrangle :as wrangle]))

(defn csv-give
  "Get a give function for the CSV file reader, the wrap? arg sets whether the
  file contents should wrap, i.e. repeat infinitely."
  ([reader] (csv-give reader true))
  ([reader wrap?]
   (let [csv (csv/read-csv reader),
         field-names (wrangle/keywordify (first csv))]
     (fn [io-settings]
       ((if wrap? cycle identity)
        (partition-all (io-settings :page-size)
                       (map
                         (fn [row]
                           (apply assoc {} (interleave field-names row)))
                         (rest csv))))))))
