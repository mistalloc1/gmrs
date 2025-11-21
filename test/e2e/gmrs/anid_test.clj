(ns gmrs.anid-test
  (:require [clojure.test :refer :all]
            [gmrs.command :as cmd]
            [gmrs.wrangle :as wrangle]
            [gmrs.preprocess :as preproc]
            [gmrs.io.csv :as csv]
            [gmrs.io.getters :as get]
            [gmrs.io.baseless :as bs]))

(def test-page-size 120)

(def example-cases
  [
   {:Mal-ID 222
    :Username "Notrik"
    :Gender "Male"
    :Birthday "1987-02-18T00:00:00+00:00"
    :Location "Drøbak, Norway"
    :Joined "2005-10-18T00:00:00+00:00"
    :Days-Watched 93.2
    :Mean-Score 8.44
    :Watching 15.0
    :Completed 179.0
    :On-Hold 0.0
    :Dropped 1.0
    :Plan-to-Watch 0.0
    :Total-Entries 195.0
    :Rewatched 0.0
    :Episodes-Watched 5538.0}

   {:Mal-ID 224
    :Username "modious"
    :Gender "Male"
    :Birthday nil
    :Location nil
    :Joined "2005-10-26T00:00:00+00:00"
    :Days-Watched 56.5
    :Mean-Score 8.18
    :Watching 8.0
    :Completed 65.0
    :On-Hold 0.0
    :Dropped 1.0
    :Plan-to-Watch 85.0
    :Total-Entries 159.0
    :Rewatched 0.0
    :Episodes-Watched 3371.0}

   {:Mal-ID 228
    :Username "Okkult"
    :Gender nil
    :Birthday nil
    :Location nil
    :Joined "2005-11-10T00:00:00+00:00"
    :Days-Watched 849.0
    :Mean-Score 7.26
    :Watching 133.0
    :Completed 4730.0
    :On-Hold 93.0
    :Dropped 0.0
    :Plan-to-Watch 190.0
    :Total-Entries 5146.0
    :Rewatched 28.0
    :Episodes-Watched 55968.0}
   ])

(defn prepared-io-setup []
  (let [setup (atom (bs/toy-temp-baseless-io-setup))]
    (swap! setup
           update-in
           [:option-gives]
           conj (csv/csv-give "dev/anid/anime-filtered.csv"))
    (swap! setup
           update-in
           [:case-gives]
           conj (csv/csv-give "dev/anid/users-details-2023.csv"))
    (swap! setup
           update-in
           [:inter-gives]
           conj (csv/csv-give "dev/anid/user-filtered.csv" :inter-id))
    @setup))

(deftest test-anid-loading
  (binding [cmd/*GlobalIOSetup* (prepared-io-setup),
            cmd/*GlobalIOSettings*
            (assoc (bs/toy-temp-baseless-io-settings)
                   :page-size test-page-size
                   :option-id :anime_id
                   :case-id :Mal-ID
                   :option-columns #{:anime_id :Name :Score :Genres
                                     :Episodes :Producers}
                   :case-columns #{:Mal-ID :Gender :Completed
                                   :Location :Dropped})]
    (testing "getting the data without preprocessing"
      (let [options (first (get/get-options cmd/*GlobalIOSettings*
                                            cmd/*GlobalIOSetup*))]
        ; We expect the columnar format.
        (is (= 6 (count (keys options))))
        (is (= test-page-size (wrangle/cols-row-count options)))))
    (testing "preprocessing options, cases"
      (cmd/new-governor! "anime-recs")
      (cmd/autogovern! "anime-recs")
      (let [guvna (get/get-governor cmd/*GlobalIOSettings* cmd/*GlobalIOSetup*
                                    "anime-recs"),
            case-getter (get/get-cases cmd/*GlobalIOSettings*
                                       cmd/*GlobalIOSetup*),
            opt-getter (get/get-options cmd/*GlobalIOSettings*
                                        cmd/*GlobalIOSetup*),
            tags-and-transfs (preproc/retag-with-preproc-transforms
                               (:tags-preprocessing guvna)
                               (map guvna [:case-columns :option-columns])
                               [(first case-getter) (first opt-getter)]),
            both-prepr (preproc/execute-preprocessing-instructions
                          (:tags-table tags-and-transfs)
                          (:set-taggings tags-and-transfs)
                          [(second case-getter) (second opt-getter)]),
            cases-prepr (first both-prepr), opts-prepr (second both-prepr)]
        (is (= test-page-size (wrangle/cols-row-count cases-prepr))
            "full cases page preprocessed")
        (is (< 3 (count (filter (fn [k] (.startsWith (name k) "Location"))
                                (keys cases-prepr))))
            "multiple columns of encoded Location")
        (is (< 1 (count (filter (fn [k] (.startsWith (name k) "Gender"))
                                (keys cases-prepr))))
            "multiple columns of encoded Gender")
        (is (= [:Mal-ID :Gender :Completed :Location :Dropped]
               (keys cases-prepr))
            "all case columns preprocessed")))))

;(run-test test-anid-loading)

(deftest test-anid-recommend-to-some-features
  (binding [cmd/*GlobalIOSetup* (prepared-io-setup),
            cmd/*GlobalIOSettings* (bs/toy-temp-baseless-io-settings)]
    (cmd/set-db-settings! :option-id :anime_id
                          :case-id :Mal-ID
                          :page-size test-page-size
                          :inter-id :inter-id
                          :inter-case :user_id
                          :inter-option :anime_id
                          :option-columns #{:anime_id :Name :Score :Genres
                                            :Episodes :Producers}
                          :case-columns #{:Mal-ID :Gender :Completed
                                          :Location :Dropped})
    (cmd/new-governor! "anime-recs")
    (cmd/autogovern! "anime-recs")
    (cmd/force-mill! "anime-recs" :nearest-options)
    (let [recs (cmd/recommend-to "anime-recs" nil nil example-cases)]
      (is (= #{222 224 228} (set (keys recs)))
          "recommendations keyed by cases")
      (is (= 5 (count (get recs 222)))
          "recs gotten, guvna recs-amount observed"))))

;(run-test test-anid-recommend-to-some-features)

#_(add-tap (fn [inp] (when (some #{(:place inp)}
                                  ;[:preprocess-execute]
                                  [:nn-from-inters :nn-from-cases]
                                  )
                       (println inp))))

; (run-test test-anid-loading)
