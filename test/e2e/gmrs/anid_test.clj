(ns gmrs.anid-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [gmrs.command :as cmd]
            [gmrs.wrangle :as wrangle]
            [gmrs.io.csv :as csv]
            [gmrs.io.getters :as get]
            [gmrs.io.baseless :as bs]))

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

(defn prepared-io-setup-selection []
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
           conj (csv/csv-give "dev/anid/user-filtered.csv" false :inter-id))
    @setup))

(deftest test-anid-loading
  (binding [cmd/*GlobalIOSetup* (prepared-io-setup-selection),
            cmd/*GlobalIOSettings*
            (assoc (bs/toy-temp-baseless-io-settings)
                   :option-id :anime_id
                   :case-id :Mal-ID
                   :option-columns #{:anime_id :Name :Score :Genres
                                     :Episodes :Producers}
                   :case-columns #{:Mal-ID})]
      (let [options (first (get/get-options cmd/*GlobalIOSettings*
                                            cmd/*GlobalIOSetup*))]
        ; We expect the columnar format.
        (is (= 6 (count (keys options))))
        (is (= 32 (wrangle/cols-row-count options))))))

(deftest test-anid-recommend-to-some-features
  (binding [cmd/*GlobalIOSetup* (prepared-io-setup-selection),
            cmd/*GlobalIOSettings* (bs/toy-temp-baseless-io-settings)]
    (cmd/set-db-settings! :option-id :anime_id
                          :case-id :Mal-ID
                          :inter-id :inter-id
                          :inter-case :user_id
                          :inter-option :anime_id
                          :option-columns #{:anime_id :Name :Score :Genres
                                            :Episodes :Producers}
                          :case-columns #{:Mal-ID})
    (cmd/new-governor! "anime-recs")
    (cmd/autogovern! "anime-recs")
    (cmd/force-mill! "anime-recs" :nearest-options)
    (cmd/recommend-to "anime-recs" nil nil example-cases)))

(run-test test-anid-recommend-to-some-features)

#_(add-tap (fn [inp] (when (some #{(:place inp)}
                                  [:groups-to-ready-transfs])
                       (println inp))))

; (run-test test-anid-loading)
