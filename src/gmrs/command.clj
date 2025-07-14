(ns gmrs.command
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [gmrs.governor :as gv]
            [gmrs.wrangle :as wrangle]
            [gmrs.io.baseless :as bs]
            [gmrs.io.getters :as get]
            [gmrs.mills.informed-popularity
             :refer [informed-popularity-recommend]]
            [gmrs.mills.nearest-options :refer [nearest-options-recommend]]))

(def ^:dynamic *GlobalIOSettings* (bs/toy-temp-baseless-io-settings))
(def ^:dynamic *GlobalIOSetup* (bs/toy-temp-baseless-io-setup))
(def ^:dynamic *EnabledMills*
  { :informed-popularity informed-popularity-recommend 
    :nearest-options nearest-options-recommend })

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
  (let [old-govern (get/get-governor *GlobalIOSettings*
                                     *GlobalIOSetup*
                                     govern-name),
        new-govern (gv/autogovern old-govern
                                  *GlobalIOSettings*
                                  (:option-gives *GlobalIOSetup*)
                                  (:case-gives *GlobalIOSetup*)
                                  (:inter-gives *GlobalIOSetup*))]
    (run! (fn [send-fun] (send-fun *GlobalIOSetup* govern-name
                                   new-govern))
          (:govern-sends *GlobalIOSetup*))))

(defn recommend-to
  ([govern-name cases-filter options-filter]
   (recommend-to govern-name cases-filter options-filter
                 (first (get/get-cases  *GlobalIOSettings*
                                       *GlobalIOSetup*))))
  ; NOTE: this is intended so you could send in new cases without saving them
  ; to the storage
  ([govern-name cases-filter options-filter case-cols]
   ; TODO: actually apply cases-filter and options-filter, some filters and
   ; guidance should come from the guvna/mill
   (let [governor (get/get-governor  *GlobalIOSettings*
                                    *GlobalIOSetup*
                                    govern-name)]
     (apply ((governor :mill) *EnabledMills*)
            case-cols
            ; FIXME: apply strategy here and for cases
            (first (get/get-options *GlobalIOSettings*
                                    *GlobalIOSetup*))
            governor))))
