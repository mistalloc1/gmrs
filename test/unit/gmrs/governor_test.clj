(ns gmrs.governor-test
  (:require [clojure.test :refer :all]
            [gmrs.command :refer [*EnabledMills*]]
            [gmrs.wrangle :as wrangle]
            [gmrs.governor :refer :all]
            [gmrs.command :as cmd]))

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
          :checkin-until #{:str :needs-conv :time-local},
          :avg-price #{:int},
          :amenities #{:str},
          :row-id #{:int}}
         (diagnose-columns-from-source hotel-governor
                                       cmd/*GlobalIOSettings*
                                       hotel-option-gives))
      "options")
  (is (= {:name #{:str},
          :country #{:tags :str},
          :checkin-until #{:str :needs-conv :time-local},
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
           :checkin-until #{:str :needs-conv :time-local},
           :avg-price #{:int},
           :amenities #{:str},
           :row-id #{:int}}
          :case-columns
          {:name #{:str},
          :country #{:tags :str},
          :checkin-until #{:str :needs-conv :time-local},
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

; (run-tests 'gmrs.governor-test)
