(ns gmrs.io.getters
  (:require [gmrs.wrangle :as wrangle]))

(defn get-governor [io-settings io-setup govern-name]
  (some identity
        (map (fn [give-fun] (give-fun io-settings govern-name))
             (:govern-gives io-setup))))

(defn pages
  "Lazy sequence of concatenated subsequent pages of each give."
  [io-settings gives cols-subset]
  (lazy-seq (cons
              (with-meta
                ((if cols-subset #(select-keys % cols-subset) identity)
                 (wrangle/records-as-cols (apply concat (map first gives))))
                {:io-settings io-settings})
              (pages io-settings (map rest gives) cols-subset))))

;; TODO: getters as exposed downstream should be able to receive dataprefs but
;; also wrap preprocessing transformations
(defn getter
  ([io-settings gives] (getter io-settings gives nil))
  ([io-settings gives cols-subset]
  (let [realized-gives (map #(apply % [io-settings])
                            gives)]
    (pages io-settings realized-gives cols-subset))))

(defn get-options
  "Lazy sequence of option pages (combining a page from each give)."
  [io-settings io-setup]
  (getter io-settings (:option-gives io-setup) (:option-columns io-settings)))

(defn get-cases
  "Lazy sequence of case pages (combining a page from each give)."
  [io-settings io-setup]
  (getter io-settings (:case-gives io-setup) (:case-columns io-settings)))

(defn get-inters
  "Lazy sequence of interaction pages (combining a page from each give)."
  [io-settings io-setup]
  (getter io-settings (:inter-gives io-setup)))
