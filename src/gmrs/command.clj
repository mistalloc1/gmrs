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
    :nearest-options nearest-options-type-mill})

(def ^:dynamic *EnabledPullStrategies*
  { :target-top-heavy gv/target-top-heavy-pull-strategy })

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
                                  (:option-gives *GlobalIOSetup*)
                                  (:case-gives *GlobalIOSetup*)
                                  (:inter-gives *GlobalIOSetup*))]
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

(defn recommend-to
  ([govern-name cases-filter options-filter]
   (recommend-to govern-name cases-filter options-filter
                 (first (get/get-cases  *GlobalIOSettings*
                                       *GlobalIOSetup*))))
  ; NOTE: this is intended so you could send in new cases without saving them
  ; to the storage
  ([govern-name cases-filter options-filter cases]
   ; TODO: actually apply cases-filter and options-filter, some filters
   ; guidance should come from the guvna/mil
   (let [gov (get/get-governor *GlobalIOSettings*
                               *GlobalIOSetup*
                               govern-name)
         mill ((gov :mill) *EnabledMills*),
         pull-strat ((gov :pull-strategy) *EnabledPullStrategies*),
         options-getter (get/get-options *GlobalIOSettings*
                                         *GlobalIOSetup*),
         inters-getter (get/get-inters *GlobalIOSettings*
                                       *GlobalIOSetup*),
         ;; TODO: what if the samples are not enough?
         raw-options-sample (first options-getter),
         raw-inters-sample (first inters-getter),
         tags-and-transfs (preproc/retag-with-preproc-transforms
                            (:tags-preprocessing gov)
                            (map gov [:case-columns :option-columns
                                      :inter-columns])
                            [cases raw-options-sample raw-inters-sample])
         set-taggings (:set-taggings tags-and-transfs),
         preprocess-exec (partial preproc/execute-preprocessing-instructions
                                  (:tags-table tags-and-transfs))]
     (assert (:mill gov))
     (assert (:tags-preprocessing gov))
     ;; TODO:require at least some of :case-columns etc. to be present
     (println "TAGS" tags-and-transfs)
     (println "PREPR" (preprocess-exec set-taggings
                                  [cases raw-options-sample raw-inters-sample]))
     (reduce-kv
       (fn [recs-map case-id case-recs]
         (assoc recs-map case-id (take (gov :recs-amount) case-recs)))
       {}
       (apply
         mill
         (concat []
                 ;; preprocess the already gotten data as the starts. wrap
                 ;; the getters for more; nothing below will ever see the raw
                 ;; data
                 (preprocess-exec set-taggings
                                  [cases raw-options-sample raw-inters-sample])
                 [(map #(first (preprocess-exec (take 1 (drop 1 set-taggings))
                                                [%]))
                       (rest options-getter))
                  (map #(first (preprocess-exec (drop 2 set-taggings) [%]))
                       (rest inters-getter))
                  (partial pull-strat gov)]))))))

(defn -main [& args]
  (println "Running GMRS"))
