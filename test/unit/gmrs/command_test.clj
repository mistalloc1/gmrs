(ns gmrs.command-test
  (:require [clojure.test :refer :all]
            [gmrs.command :refer :all]
            [gmrs.wrangle :as wrangle]
            [gmrs.io.getters :as get]
            [gmrs.preprocess :as preproc]
            [gmrs.io.baseless :as bs]))

(def example-governor
  { :recs-amount 2 ;:mill :nearest-options
    :score-weakness-tolerance 0.02
    :pull-strategy :target-top-heavy
    :tags-preprocessing
    { :tags preproc/MultihotFromTags
      :number-scale preproc/ZLogisticScale } })

(def example-options
  [{:id 90 :name "Alligator" :danger "high" :time "2004-03-04" :temperature 25}
   {:id 130 :name "Cat" :danger "high" :time "1999-12-09" :temperature 38}
   {:id 140 :name "Snail" :danger "low" :time "2020-02-30" :temperature 28}
   {:id 175 :name "Pigeon" :danger "medium" :time "2011-07-22" :temperature 42}
   {:id 200 :name "Octopus" :danger "medium" :time "2017-11-05" :temperature 12}
   {:id 245 :name "Squirrel" :danger "medium" :time "2008-05-19" :temperature 38}
   {:id 310 :name "Ferret" :danger "low" :time "2014-09-13" :temperature 39}
   {:id 360 :name "Moose" :danger "high" :time "1995-01-27" :temperature 38}])

(def example-cases
  [{:id 1190 :name "Laszlo" :danger "high" :city "Eger"}
   {:id 2130 :name "Ilona" :danger "high" :city "Budapest"}
   {:id 2140 :name "Zoltan" :danger "low" :city "Budapest"}
   {:id 2200 :name "Csilla" :danger "low" :city "Debrecen"}])

(def example-inters
  [{:id "inter1", :case-id 1190 :option-id 130}
   {:id "inter2", :case-id 1190, :option-id 175}
   {:id "inter3", :case-id 1190, :option-id 175}
   {:id "inter4", :case-id 1190, :option-id 245}
   {:id "inter5", :case-id 1190, :option-id 310}
   {:id "inter6", :case-id 2130, :option-id 310}
   {:id "inter7", :case-id 2140, :option-id 175}
   {:id "inter8", :case-id 2130, :option-id 140}
   {:id "inter9", :case-id 1190, :option-id 310}
   {:id "inter10", :case-id 2130, :option-id 310}
   {:id "inter11", :case-id 2140, :option-id 360}
   {:id "inter12", :case-id 2140, :option-id 175}])

(deftest test-force-mill
  (binding [*GlobalIOSetup* (bs/toy-temp-baseless-io-setup),
            *GlobalIOSettings* (assoc (bs/toy-temp-baseless-io-settings)
                                      :option-id :id)]
    (run! (fn [send-fun]
            (send-fun *GlobalIOSettings* "test-guvna"
                      (assoc example-governor :mill :informed-popularity)))
        (:govern-sends *GlobalIOSetup*))
    (is (:mill (get/get-governor *GlobalIOSettings* *GlobalIOSetup*
                                 "test-guvna"))
        :informed-popularity)
    (force-mill! "test-guvna" :nearest-options)
    (is (:mill (get/get-governor *GlobalIOSettings* *GlobalIOSetup*
                                 "test-guvna"))
        :nearest-options)))

(deftest test-integr-send-and-options-getter
  (binding [*GlobalIOSetup* (bs/toy-temp-baseless-io-setup),
            *GlobalIOSettings* (assoc (bs/toy-temp-baseless-io-settings)
                                      :option-id :id)]
    (send-options! example-options)
    (is (= (sort-by :id example-options)
           (sort-by :id (wrangle/cols-as-rows
                          (first (get/options-getter *GlobalIOSettings*
                                                  *GlobalIOSetup*)))))
        "getting previously sent options")))

(deftest test-integr-send-and-cases-getter
  (binding [*GlobalIOSetup* (bs/toy-temp-baseless-io-setup),
            *GlobalIOSettings* (assoc (bs/toy-temp-baseless-io-settings)
                                      :case-id :id)]
    (send-cases! example-cases)
    (is (= (sort-by :id example-cases)
           (sort-by :id (wrangle/cols-as-rows
                          (first (get/cases-getter *GlobalIOSettings*
                                                *GlobalIOSetup*)))))
        "getting previously sent cases")))

(deftest test-integr-send-and-inters-getter
  (binding [*GlobalIOSetup* (bs/toy-temp-baseless-io-setup),
            *GlobalIOSettings* (assoc (bs/toy-temp-baseless-io-settings)
                                      :inter-id :id)]
    (send-interactions! example-inters)
    (is (= (sort-by :id example-inters)
           (sort-by :id (wrangle/cols-as-rows
                          (first (get/inters-getter *GlobalIOSettings*
                                                 *GlobalIOSetup*)))))
        "getting previously sent inters")))

(deftest test-integr-recommend-to
  (binding [*GlobalIOSetup* (bs/toy-temp-baseless-io-setup),
            *GlobalIOSettings* (assoc (bs/toy-temp-baseless-io-settings)
                                      :case-id :id, :option-id :id,
                                      :inter-id :id,
                                      :inter-case :case-id,
                                      :inter-option :option-id)]
    (send-cases! example-cases) ; for autogovern
    (send-options! example-options)
    (send-interactions! example-inters)
    ;; NOTE: this assumes setting a custom guvna isn't handled by API, maybe it
    ;; should
    (run! (fn [send-fun]
            (send-fun *GlobalIOSettings* "test-guvna" example-governor))
        (:govern-sends *GlobalIOSetup*))
    ;; Among other work, annotate the column attributes.
    (autogovern! "test-guvna")
    (force-mill! "test-guvna" :nearest-options)
    (let [recommendations
          (recommend-to "test-guvna" nil nil
                        (with-meta
                          example-cases
                          { :io-settings
                            (assoc *GlobalIOSettings*
                                   :case-id :id :option-id :id
                                   :inter-id :id :inter-case :case-id
                                   :inter-option :option-id) }))]
      (is (= #{1190 2130 2140 2200} (set (keys recommendations)))
          "recommendations keyed by cases")
      (is (= 2 (count (get recommendations 1190)))
          "guvna recs-amount observed")
      (is (some #{90 130 360} (map :id (get recommendations 2130)))
          "some 'high danger' options should be for the 'high danger' case")
      (is (number? (:score (first (get recommendations 2140))))))))

; (run-tests `gmrs.command-test)
