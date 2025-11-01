(ns gmrs.governor-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [gmrs.command :as cmd :refer [*EnabledMills*]]
            [gmrs.governor :refer :all])
  (:import [java.time ZonedDateTime]))

(def hotel-governor
  { :recs-amount 2
    :score-weakness-tolerance 0.02
    :pull-strategy :target-top-heavy })

(deftest test-target-top-heavy-pull-strategy
  (is (true?
        (target-top-heavy-pull-strategy
          hotel-governor
          {:a [{:score 0.98 :option "go"} {:score 0.1 :option "stay"}]
           :b [{:score 0.25 :option "go"} {:score 0.01 :option "stay"}]}
          1))
      "high recommendation cost")
  (is (false?
        (target-top-heavy-pull-strategy
          hotel-governor
          {:a [{:score 0.98 :option "go"} {:score 0.1 :option "stay"}]
           :b [{:score 0.25 :option "go"} {:score 0.01 :option "stay"}]}
          1000))
      "high recommendation cost but after many pulls"))

(deftest test-safe-parse
  (is (= 10 (:priority (safe-parse #(Float/parseFloat %)))) "correct priority")
  (testing "integers safe-parse"
    (let [transf (safe-parse #(Integer/parseInt (str/trim %)))]
      (is (= [1 2 3] ((:execute transf) ["1" "  2  " "3"]
                      ((:prepare transf) ["1" "  2  " "3"]))))
      (is (= [1 nil 3 nil 5] ((:execute transf) ["1" "invalid" "3" "bad" "5"]
                              ((:prepare transf) ["1" "invalid" "3" "bad" "5"]))))
      (is (nil? ((:execute transf) nil ((:prepare transf) nil))))))
  (testing "Parsing datetime with invalid values returns nil for bad entries"
    (let [transf (safe-parse #(ZonedDateTime/parse %))
          mixed-dates ["2024-01-01T10:00:00Z" "invalid-date" "not-a-datetime"]
          result ((:execute transf) mixed-dates ((:prepare transf) mixed-dates))]
      (is (instance? ZonedDateTime (first result)))
      (is (nil? (second result)))
      (is (nil? (nth result 2))))))

(def hotel-option-gives
  (map (fn [source] (fn [io-settings]
                      (partition-all (io-settings :page-size) source)))
       (list [{:name "Dump Hotel" :country "USA" :checkin-until "20:00"
               :avg-price 20 :amenities "vending machine" :row-id 5}
              {:name "Hilton Hotel" :country "USA" :checkin-until "24:00"
               :avg-price 200 :amenities "pool|gym|wifi|breakfast" :row-id 15}
              {:name "Budget Inn" :country "USA" :checkin-until "22:00"
               :avg-price 80 :amenities "parking|laundry|concierge" :row-id 25}]
             [{:name "Hotel Reims" :country "France" :checkin-until "22:00"
               :avg-price 150 :amenities "breakfast|wifi" :row-id 6}
              {:name "Château Resort" :country "France" :checkin-until "23:00"
               :avg-price 300 :amenities "spa|restaurant|room-service|balcony"
               :row-id 16}])))

(def hotel-case-gives
  (map (fn [source] (fn [io-settings]
                      (partition-all (io-settings :page-size) source)))
       (list [{:name "John Smith" :country "USA" :checkin-until "22:00"
               :avg-price 180 :amenities "pool|wifi|pet-friendly" :age 34
               :travel-purpose "business"}
              {:name "Sarah Johnson" :country "USA" :checkin-until "20:00"
               :avg-price 50 :amenities "wifi|electric-car-charging" :age 78
               :travel-purpose "leisure"}]
             [{:name "Pierre Dubois" :country "France" :checkin-until "23:00"
               :avg-price 120 :amenities "breakfast|wifi|bicycle-rental" :age 45
               :travel-purpose "business"}
              {:name "Marie Leroy" :country "France" :checkin-until "21:00"
               :avg-price 90 :amenities "wifi|kitchenette" :age 21
               :travel-purpose "leisure"}]
             [{:name "Erik Andersson" :country "Sweden" :checkin-until "24:00"
               :avg-price 160 :amenities "gym|wifi|airport-shuttle" :age 29
               :travel-purpose "business"}
              {:name "Anna Lindqvist" :country "Sweden" :checkin-until "22:00"
               :avg-price 140 :amenities "breakfast|pool|wifi|babysitting"
               :age 38 :travel-purpose "leisure"}])))


(def hotel-inter-gives [])

; TODO: test/deal with nils, mixed columns
(deftest test-diagnose-columns-from-source
  (is (= {:name #{:str},
          :country #{:tags :str},
          :checkin-until #{:str :time-local-needs-conv :time-local},
          :avg-price #{:int},
          :amenities #{:str},
          :row-id #{:int}}
         (diagnose-columns-from-source hotel-governor
                                       cmd/*GlobalIOSettings*
                                       hotel-option-gives))
      "options")
  (is (= {:name #{:str},
          :country #{:tags :str},
          :checkin-until #{:str :time-local-needs-conv :time-local},
          :avg-price #{:int},
          :amenities #{:str},
          :age #{:int},
          :travel-purpose #{:tags :str}}
         (diagnose-columns-from-source hotel-governor
                                       cmd/*GlobalIOSettings*
                                       hotel-case-gives))
      "cases")
  (is (= ; TODO: {}? check case where it's important in integration
         []
         (diagnose-columns-from-source hotel-governor
                                       cmd/*GlobalIOSettings*
                                       hotel-inter-gives))
      "inters (empty)"))

(deftest test-update-columns-diagnostics
  (is (= { :recs-amount 2
          :score-weakness-tolerance 0.02 :pull-strategy :target-top-heavy
          :option-columns
          {:name #{:str},
           :country #{:tags :str},
           :checkin-until #{:str :time-local-needs-conv :time-local},
           :avg-price #{:int},
           :amenities #{:str},
           :row-id #{:int}}
          :case-columns
          {:name #{:str},
           :country #{:tags :str},
           :checkin-until #{:str :time-local-needs-conv :time-local},
           :avg-price #{:int},
           :amenities #{:str},
           :age #{:int},
           :travel-purpose #{:tags :str}}
          :inter-columns [] }
         (update-columns-diagnostics hotel-governor
                                     cmd/*GlobalIOSettings*
                                     hotel-option-gives hotel-case-gives
                                     hotel-inter-gives))))

(deftest test-choose-and-prepare-mill
  (is (some #{(:mill (choose-and-prepare-mill
                     (update-columns-diagnostics
                       hotel-governor cmd/*GlobalIOSettings*
                       hotel-option-gives hotel-case-gives
                       hotel-inter-gives)))}
            (keys *EnabledMills*))
      "derive a mill from provided data")
  (is (some #{(:mill (choose-and-prepare-mill hotel-governor))}
            (keys *EnabledMills*))
      "assign a mill when empty data"))

(deftest test-autogovern
  (is (some #{(:mill (autogovern
                       hotel-governor cmd/*GlobalIOSettings*
                       hotel-option-gives hotel-case-gives
                       hotel-inter-gives))}
            (keys *EnabledMills*))
      "derive a mill from provided data")
  (is (some #{(:mill (choose-and-prepare-mill hotel-governor))}
            (keys *EnabledMills*))
      "assign a mill when empty data"))

;; (run-tests 'gmrs.governor-test)
