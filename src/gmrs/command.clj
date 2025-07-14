(ns gmrs.command
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [gmrs.governor :as gv]
            [gmrs.wrangle :as wrangle]
            [gmrs.io.baseless :as bs]
            [gmrs.mills.informed-popularity
             :refer [informed-popularity-recommend]]
            [gmrs.mills.nearest-options :refer [nearest-options-recommend]]))

(def ^:dynamic *GlobalIOSettings* (bs/toy-temp-baseless-io-settings))
(def ^:dynamic *GlobalIOSetup* (bs/toy-temp-baseless-io-setup))
(def ^:dynamic *EnabledMills*
  { :informed-popularity informed-popularity-recommend 
    :nearest-options nearest-options-recommend })

;
; Helper functions.

(defn _get-governor [govern-name]
  (some (map (fn [give-fun] (give-fun *GlobalIOSettings* govern-name))
             (:govern-gives *GlobalIOSetup*))))

(defn _pages [gives]
  "Lazy sequence of concatenated subsequent pages of each give."
  (lazy-seq (cons
              (with-meta
                (wrangle/records-as-cols (apply concat (map first gives)))
                {:io-settings *GlobalIOSettings*})
              (_pages (map rest gives)))))

(defn _get-options []
  (let [gives (map #(apply % [*GlobalIOSettings*])
                   (:option-gives *GlobalIOSetup*))]
    (_pages gives)))

(defn _get-cases []
  (let [gives (map #(apply % [*GlobalIOSettings*])
                   (:case-gives *GlobalIOSetup*))]
    (_pages gives)))

;
; API functions.

(defn new-governor! [govern-name]
  (run! (fn [send-fun]
          (send-fun *GlobalIOSettings* govern-name (gv/new-governor)))
        (:govern-sends *GlobalIOSetup*)))

(defn send-options!
  "Add or replace recommendable options (given as hash maps)."
  [options]
  (run! (fn [send-fun] (send-fun *GlobalIOSettings* options))
        (:option-sends *GlobalIOSetup*)))

(defn send-cases!
  "Add or replace recommendable cases (given as hash maps)."
  [cases]
  (run! (fn [send-fun] (send-fun *GlobalIOSettings* cases))
        (:case-sends *GlobalIOSetup*)))

(defn send-options-csv!
  [path]
  (with-open [reader (io/reader path)]
    (let [contents (csv/read-csv reader),
          header (map keyword (first contents))]
      (send-options! (map (fn [row]
                            (apply assoc {}
                                   (interleave header row)))
                          (rest contents))))))

(defn send-cases-csv!
  [path]
  (with-open [reader (io/reader path)]
    (let [contents (csv/read-csv reader),
          header (map keyword (first contents))]
      (send-cases! (map (fn [row]
                            (apply assoc {}
                                   (interleave header row)))
                          (rest contents))))))

(defn autogovern! [govern-name]
  (let [old-govern (_get-governor govern-name),
        new-govern (gv/autogovern old-govern
                                  (:option-gives *GlobalIOSetup*)
                                  (:case-gives *GlobalIOSetup*)
                                  (:inter-gives *GlobalIOSetup*))]
    (run! (fn [send-fun] (send-fun *GlobalIOSetup* govern-name
                                   new-govern))
          (:govern-sends *GlobalIOSetup*))))

(defn recommend-to
  ([govern-name cases-filter options-filter]
   (recommend-to govern-name cases-filter options-filter
                 (first (_get-cases))))
  ; NOTE: this is intended so you could send in new cases without saving them
  ; to the storage
  ([govern-name cases-filter options-filter case-cols]
   ; TODO: actually apply cases-filter and options-filter, some filters and
   ; guidance should come from the guvna/mill
   (let [governor (_get-governor govern-name)]
     (apply ((governor :mill) *EnabledMills*)
            case-cols
            (first (_get-options)) ; FIXME: apply strategy here and for cases
            governor))))
