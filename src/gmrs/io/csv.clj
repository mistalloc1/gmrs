(ns gmrs.io.csv
  (:require [clojure.data.csv :as csv]
            [gmrs.wrangle :as wrangle]))

(defn csv-give
  "Get a give function for the CSV file reader, the wrap? arg sets whether the
  file contents should wrap, i.e. repeat infinitely.

  If ident-column? is set to a keyword, a sequence of consecutive numbers will
  be added as a potential ID column"
  ([reader] (csv-give reader true false))
  ([reader wrap? ident-column?]
   (let [csv (csv/read-csv reader),
         field-names (wrangle/keywordify (first csv)),
         counter (atom 1)]
     (fn [io-settings]
       ((if wrap? cycle identity)
        (partition-all (io-settings :page-size)
                       (map
                         (fn [row]
                           (apply assoc
                                  (if ident-column?
                                    {ident-column? (swap! counter inc)}
                                    {})
                                  (interleave field-names row)))
                         (rest csv))))))))
