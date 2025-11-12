(ns gmrs.io.csv
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [gmrs.wrangle :as wrangle]))

(defn csv-give
  "Get a give function for the CSV file path.

  If ident-column? is set to a keyword, a sequence of consecutive numbers will
  be added as a potential ID column."
  ([path] (csv-give path false))
  ([path ident-column?]
   (fn [io-settings]
     (let [page-size (io-settings :page-size)
           pages-per-chunk 20
           rows-per-chunk (* page-size pages-per-chunk)]
       ;; Re-open the file and load the next chunk of pages every 20 pages.
       (letfn [(read-chunk [start-row]
                 (with-open [reader (io/reader path)]
                   (let [csv (csv/read-csv reader)
                         field-names (wrangle/keywordify (first csv))
                         rows-to-skip (inc start-row)
                         chunk-data (take rows-per-chunk (drop rows-to-skip csv))]
                     (when (seq chunk-data)
                       (doall
                         (map-indexed
                           (fn [idx row]
                             (let [global-row-num (+ start-row idx 1)]
                               (apply assoc
                                      (if ident-column?
                                        {ident-column? global-row-num}
                                        {})
                                      (interleave field-names row))))
                           chunk-data))))))
               (lazy-chunks [start-row]
                 (lazy-seq
                   (when-let [chunk (read-chunk start-row)]
                     (let [pages (partition-all page-size chunk)]
                       (concat pages (lazy-chunks (+ start-row (count chunk))))))))]
         (lazy-chunks 0))))))
