(ns gmrs.io.getters
  (:require [gmrs.wrangle :as wrangle]))

(defn get-governor [io-settings io-setup govern-name]
  (some (map (fn [give-fun] (give-fun io-settings govern-name))
             (:govern-gives io-setup))))

(defn pages
  "Lazy sequence of concatenated subsequent pages of each give."
  [io-settings gives]
  (lazy-seq (cons
              (with-meta
                (wrangle/records-as-cols (apply concat (map first gives)))
                {:io-settings io-settings})
              (pages io-settings (map rest gives)))))

(defn getter
  [io-settings gives]
  (let [realized-gives (map #(apply % [io-settings])
                            gives)]
    (pages io-settings realized-gives)))

(defn get-options
  "Lazy sequence of option pages (combining a page from each give)."
  [io-settings io-setup]
  (getter io-settings (:option-gives io-setup)))

(defn get-cases
  "Lazy sequence of case pages (combining a page from each give)."
  [io-settings io-setup]
  (getter io-settings (:case-gives io-setup)))
