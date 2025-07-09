(ns gmrs.command
  (:require
    [gmrs.governor :as gv]
    [gmrs.wrangle :as wrangle]
    [gmrs.io.baseless :as bs]
    [gmrs.mills.informed-popularity :refer [informed-popularity-recommend]]
    [gmrs.mills.nearest-options :refer [nearest-options-recommend]]))

(def ^:dynamic *GlobalIOSetup* (bs/toy-temp-baseless-io-setup))
(def ^:dynamic *EnabledMills*
  { :informed-popularity informed-popularity-recommend 
    :nearest-options nearest-options-recommend })

;
; Helper functions.

(defn _get-governor [govern-name]
  (some (map (fn [give-fun] (give-fun govern-name))
             (:govern-gives *GlobalIOSetup*))))

(defn _get-options []
  (with-meta
    (wrangle/records-as-cols
      (reduce into (:option-gives *GlobalIOSetup*)))
    {:io-settings (:settings *GlobalIOSetup*)}))

(defn _get-cases []
  (with-meta
    (wrangle/records-as-cols
      (reduce into (:case-gives *GlobalIOSetup*)))
    {:io-settings (:settings *GlobalIOSetup*)}))

;
; API functions.

(defn new-governor! [govern-name]
  (run! (fn [send-fun] (send-fun govern-name (gv/new-governor)))
        (:govern-sends *GlobalIOSetup*)))

(defn send-options!
  "Add or replace recommendable options (given as hash maps)."
  [options]
  (let [options-by-id
        (reduce into
                ; TODO: handle nils here
                (fn [option] {(-> *GlobalIOSetup* :settings :option-id option)
                            option})
                options)]
    (run! (fn [send-fun])
          (:option-sends *GlobalIOSetup*))))

(defn autogovern! [govern-name]
  (let [old-govern (_get-governor govern-name),
        new-govern (gv/autogovern old-govern
                                  (:option-gives *GlobalIOSetup*)
                                  (:case-gives *GlobalIOSetup*)
                                  (:inter-gives *GlobalIOSetup*))]
    (run! (fn [send-fun] (send-fun govern-name
                                   new-govern))
          (:govern-sends *GlobalIOSetup*))))

(defn recommend-to
  ([govern-name cases-filter options-filter]
   (recommend-to govern-name cases-filter options-filter
                 (_get-cases)))
  ([govern-name cases-filter options-filter case-cols]
   ; TODO: actually apply cases-filter and options-filter
   (let [governor (_get-governor govern-name)]
     (apply ((governor :mill) *EnabledMills*)
            case-cols
            (_get-options)
            governor))))
