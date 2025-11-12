(ns gmrs.io.csv
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [gmrs.wrangle :as wrangle]))

(defn csv-give
  "Get a give function for the CSV file path, the wrap? arg sets whether the
  file contents should wrap, i.e. repeat infinitely.

  If ident-column? is set to a keyword, a sequence of consecutive numbers will
  be added as a potential ID column."
  ([path] (csv-give path true false))
  ([path wrap? ident-column?]
   (fn [io-settings]
     (with-open [reader (io/reader path)]
       (let [csv (csv/read-csv reader),
             field-names (wrangle/keywordify (first csv)),
             counter (atom 1)
             realized-rows (doall
                            (map
                              (fn [row]
                                (apply assoc
                                       (if ident-column?
                                         {ident-column? (swap! counter inc)}
                                         {})
                                       (interleave field-names row)))
                              (rest csv)))]
         ((if wrap? cycle identity)
          (partition-all (io-settings :page-size)
                         realized-rows)))))))
