(ns gmrs.command-test
  (:require [clojure.test :refer :all]
            [gmrs.command :refer :all :as cmd]
            [gmrs.wrangle :as wrangle]
            [gmrs.io.baseless :as bs]))

(def example-options
  [{:id 90 :name "Alligator" :danger "high" :time "2004-03-04"}
   {:id 130 :name "Cat" :danger "high" :time "1999-12-09"}
   {:id 140 :name "Snail" :danger "low" :time "2020-02-30"}])

(def example-cases
  [{:id 1190 :name "Laszlo" :danger "high" :city "Eger"}
   {:id 2130 :name "Ilona" :danger "high" :city "Budapest"}
   {:id 2140 :name "Zoltan" :danger "low" :city "Budapest"}])

(deftest test-integr-send-and-get-options
  (binding [cmd/*GlobalIOSetup* (bs/toy-temp-baseless-io-setup),
            cmd/*GlobalIOSettings* (assoc (bs/toy-temp-baseless-io-settings)
                                          :option-id :id)]
    (send-options! example-options)
    (is (= example-options (wrangle/cols-as-rows (_get-options)))
        "getting previously sent options")))

(deftest test-integr-send-and-get-cases
  (binding [cmd/*GlobalIOSetup* (bs/toy-temp-baseless-io-setup),
            cmd/*GlobalIOSettings* (assoc (bs/toy-temp-baseless-io-settings)
                                          :case-id :id)]
    (send-cases! example-cases)
    (is (= example-cases (wrangle/cols-as-rows (_get-cases)))
        "getting previously sent cases")))

; (run-tests `gmrs.command-test)
