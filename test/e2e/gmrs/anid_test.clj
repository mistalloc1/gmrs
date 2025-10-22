(ns gmrs.anid-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [gmrs.command :as cmd]
            [gmrs.wrangle :as wrangle]
            [gmrs.io.csv :as csv]
            [gmrs.io.getters :as get]
            [gmrs.io.baseless :as bs]))

(deftest test-anid-loading
  (binding [cmd/*GlobalIOSetup* (bs/toy-temp-baseless-io-setup),
            cmd/*GlobalIOSettings*
            (assoc (bs/toy-temp-baseless-io-settings)
                   :option-id :anime_id
                   :case-id :Mal-ID)]
    (with-open [anime-reader (io/reader "dev/anid/anime-filtered.csv"),
                user-reader (io/reader "dev/anid/users-details-2023.csv")
                watched-reader (io/reader "dev/anid/user-filtered.csv")]
      (set! cmd/*GlobalIOSetup*
            (update-in cmd/*GlobalIOSetup*
                       [:option-gives]
                       conj (csv/csv-give anime-reader)))
      (set! cmd/*GlobalIOSetup*
            (update-in cmd/*GlobalIOSetup*
                       [:case-gives]
                       conj (csv/csv-give user-reader)))
      (set! cmd/*GlobalIOSetup*
            (update-in cmd/*GlobalIOSetup*
                       [:inter-gives]
                       conj (csv/csv-give watched-reader false :inter-id)))
      (let [options (first (get/get-options cmd/*GlobalIOSettings*
                                            cmd/*GlobalIOSetup*))]
        ; We expect the columnar format.
        (is (= 25 (count (keys options))))
        (is (= 32 (wrangle/cols-row-count options))))
      (cmd/set-db-settings! :option-id :anime_id
                            :case-id :Mal-ID
                            :inter-id :inter-id
                            :inter-case :user_id
                            :inter-option :anime_id)
      (cmd/new-governor! "anime-recs")
      (cmd/autogovern! "anime-recs")
      (cmd/force-mill! "anime-recs" :nearest-options)
      (println "GOV")
      (run! (fn [[key val]] (println key val))
            (cmd/peek-governor "anime-recs")))))

; (run-test test-anid-loading)
