(ns gmrs.command
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [gmrs.governor :as gv]
            [gmrs.wrangle :as wrangle]
            [gmrs.io.baseless :as bs]
            [gmrs.io.getters :as get]
            [gmrs.preprocess :as preproc]
            [gmrs.mills.informed-popularity
             :refer [informed-popularity-recommend]]
            [gmrs.mills.nearest-options :refer [nearest-options-type-mill]])
  (:gen-class))

(def ^:dynamic *GlobalIOSettings* (bs/toy-temp-baseless-io-settings))
(def ^:dynamic *GlobalIOSetup* (bs/toy-temp-baseless-io-setup))

(def ^:dynamic *EnabledMills*
  { :informed-popularity informed-popularity-recommend
    :nearest-options nearest-options-type-mill })

(def ^:dynamic *MillAcceptedColumnAttrs*
  { :informed-popularity #{}
    :nearest-options #{:integer :integer-needs-conv :float :float-needs-conv
                       :tags} })

(def ^:dynamic *EnabledPullStrategies*
  { :target-top-heavy gv/target-top-heavy-pull-strategy })

(defn set-db-settings! [& specs]
  (set! *GlobalIOSettings*
        (apply assoc *GlobalIOSettings* specs)))

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

(defn send-interactions!
  "Add or replace recommendable cases (given as hash maps)."
  [inters]
  (run! (fn [send-fun] (send-fun *GlobalIOSettings* inters))
        (:inter-sends *GlobalIOSetup*)))

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
                                  *GlobalIOSetup*)]
    (run! (fn [send-fun] (send-fun *GlobalIOSetup* govern-name
                                   new-govern))
          (:govern-sends *GlobalIOSetup*))))

(defn force-mill! [govern-name mill]
  (let [old-govern (get/get-governor *GlobalIOSettings*
                                     *GlobalIOSetup*
                                     govern-name)]
    (run! (fn [send-fun] (send-fun *GlobalIOSetup* govern-name
                                   (assoc old-govern :mill mill)))
          (:govern-sends *GlobalIOSetup*))))

(defn peek-governor [govern-name]
  (get/get-governor *GlobalIOSettings* *GlobalIOSetup* govern-name))

(defn mill-args
  "Prepare args to calling a mill with the target cases, creating partial
  functions from getters. The mill can call them as they are or adding
  data-prefs.

  The mill gets the preprocessed cases, the getters, and partial pull strategy
  curried with the governor."
  [govern cases]
  ;; TODO: what if the samples are not enough?
  (let [accepted-col-attrs ((govern :mill) *MillAcceptedColumnAttrs*),
        pull-strat ((govern :pull-strategy) *EnabledPullStrategies*),
        case-id-col (*GlobalIOSettings* :case-id),
        opt-id-col (*GlobalIOSettings* :option-id),
        raw-options-sample (first (get/options-getter *GlobalIOSettings*
                                                      *GlobalIOSetup*)),
        tags-and-transfs (preproc/retag-with-preproc-transforms
                           (select-keys (:tags-preprocessing govern)
                                        accepted-col-attrs)
                           (map govern [:case-columns :option-columns])
                           [cases raw-options-sample]
                           [case-id-col opt-id-col])
        set-taggings (:set-taggings tags-and-transfs),
        preprocess-exec (partial preproc/execute-preprocessing-instructions
                                 (:tags-table tags-and-transfs)),
        preprocessed-cases (first (preprocess-exec (take 1 set-taggings) [cases]
                                                   [case-id-col]))]
    [preprocessed-cases
     (partial get/cases-getter *GlobalIOSettings* *GlobalIOSetup*
              #(first (preprocess-exec (take 1 set-taggings) [%] [case-id-col])))
     (partial get/options-getter *GlobalIOSettings* *GlobalIOSetup*
              #(first (preprocess-exec (drop 1 set-taggings) [%] [opt-id-col])))
     (partial get/inters-getter *GlobalIOSettings* *GlobalIOSetup*
              identity)
     (partial pull-strat govern)]))

(defn recommend-to
  ([govern-name cases-filter options-filter]
   (recommend-to govern-name cases-filter options-filter
                 (first (get/cases-getter *GlobalIOSettings*
                                          *GlobalIOSetup*))))
  ; NOTE: this is intended so you could send in new cases without saving them
  ; to the storage
  ([govern-name cases-filter options-filter cases]
   ; TODO: actually apply cases-filter and options-filter thru dataprefs
   (let [cases (with-meta
                 (wrangle/records-as-cols cases)
                 { :io-settings *GlobalIOSettings* }), ; adapt from the input form
         gov (get/get-governor *GlobalIOSettings*
                               *GlobalIOSetup*
                               govern-name)
         mill ((gov :mill) *EnabledMills*)]
     (assert (:mill gov))
     (assert (:tags-preprocessing gov))
     ;; TODO:require at least some of :case-columns etc. to be present
     (reduce-kv
       (fn [recs-map case-id case-recs]
         ;; Observe the recs-amount.
         (assoc recs-map case-id (take (gov :recs-amount) case-recs)))
       {}
       (apply mill (mill-args gov cases))))))

(defn -main [& args]
  (println "Running GMRS"))
