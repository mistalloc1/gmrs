(ns gmrs.anid-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [gmrs.command :refer :all :as cmd]
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
                user-reader (io/reader "dev/anid/users-details-2023.csv")]
      (set! cmd/*GlobalIOSetup*
            (update-in cmd/*GlobalIOSetup*
                       [:option-gives]
                       conj (csv/csv-give anime-reader)))
      (set! cmd/*GlobalIOSetup*
            (update-in cmd/*GlobalIOSetup*
                       [:case-gives]
                       conj (csv/csv-give user-reader)))
      (let [options (first (get/get-options cmd/*GlobalIOSettings*
                                            cmd/*GlobalIOSetup*))]
        ; We expect the columnar format.
        (is (= 25 (count (keys options))))
        (is (= 32 (wrangle/cols-row-count options)))))))
