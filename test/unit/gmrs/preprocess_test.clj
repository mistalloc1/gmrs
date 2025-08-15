(ns gmrs.preprocess-test
  (:require [clojure.test :refer :all]
            [gmrs.preprocess :refer :all]
            [gmrs.io.getters :refer [getter]]
            [gmrs.wrangle :as wrangle]))

(defn close? [tolerance x y]
  (< (Math/abs (double (- x y))) tolerance))

(deftest test-z-logistic-scale
  (let [transf (z-logistic-scale [0 1 2 5])]
    (is (= 2.0 (:mean transf)) "mean present")
    (is (close? 0.001 (:sd transf) 2.160246899469287)
        "standard deviation present")))

(deftest test-apply-z-logistic-scale
  ; the intermediate Z-scaled stage before sigmoid is roughly
  ; [0.26080, -0.51857, -1.01786, 1.27562]
  (is (every? true? (map (partial close? 0.001)
                         (apply-z-logistic-scale [43 4.6 -20 93]
                                                 {:mean 30.15 :sd 49.27})
                         [0.56483486, 0.37318641, 0.26544431, 0.78170398]))
      "basic case")
  (is (every? true? (map = (double-array (repeat 4.0 0))
                         (apply-z-logistic-scale [43 4.6 -20 93]
                                                 {:mean 30.15 :sd 0.0})))
      "0 standard dev case"))

(def example-cases
  (wrangle/records-as-cols
    [{:name "ferdek"
      :genres "jazz|dub"
      :coolness-rating 45
      :city "warsaw"}
     {:name "ela"
      :genres "rock"
      :coolness-rating 50
      :city "warsaw"}
     {:name "sara"
      :genres "jazz|rap"
      :coolness-rating 60
      :city "gdańsk"}]))

(deftest test-multihot-from-tags
  (is (= {:proc-dub [1.0 0.0 0.0],
          :proc-jazz [1.0 0.0 1.0],
          :proc-rock [0.0 1.0 0.0],
          :proc-rap [0.0 0.0 1.0]}
         (multihot-from-tags (example-cases :genres)))
      "base case")
  (is (= {:genre/dub [1.0 0.0 0.0],
          :genre/jazz [1.0 0.0 1.0],
          :genre/rock [0.0 1.0 0.0],
          :genre/rap [0.0 0.0 1.0]}
         (multihot-from-tags (example-cases :genres) "genre/"))
      "custom prefix"))

(def hotel-governor
  { :recs-amount 2
    :score-weakness-tolerance 0.02
    :pull-strategy :target-top-heavy,
    :tags-preprocessing
      { :tags MultihotFromTags
        :number-scale ZLogisticScale }})

(def hotel-options
  (wrangle/records-as-cols
    [{:name "Dump Hotel" :country "USA" :checkin-until "20:00"
      :avg-price 20 :amenities "vending machine" :row-id 5}
     {:name "Hilton Hotel" :country "USA" :checkin-until "24:00"
      :avg-price 200 :amenities "pool|gym|wifi|breakfast" :row-id 15}
     {:name "Budget Inn" :country "USA" :checkin-until "22:00"
      :avg-price 80 :amenities "parking|laundry|concierge" :row-id 25}
     {:name "Hotel Reims" :country "France" :checkin-until "22:00"
      :avg-price 150 :amenities "breakfast|wifi" :row-id 6}
     {:name "Château Resort" :country "France" :checkin-until "23:00"
      :avg-price 300 :amenities "spa|restaurant|room-service|balcony"
      :row-id 16}]))

(def hotel-cases
  (wrangle/records-as-cols
    [{:name "John Smith" :country "USA" :checkin-until "22:00"
      :avg-price 180 :amenities "pool|wifi|pet-friendly" :age 34
      :travel-purpose "business"}
     {:name "Sarah Johnson" :country "USA" :checkin-until "20:00"
      :avg-price 50 :amenities "wifi|electric-car-charging" :age 78
      :travel-purpose "leisure"}
     {:name "Pierre Dubois" :country "France" :checkin-until "23:00"
      :avg-price 120 :amenities "breakfast|wifi|bicycle-rental" :age 45
      :travel-purpose "business"}
     {:name "Marie Leroy" :country "France" :checkin-until "21:00"
      :avg-price 90 :amenities "wifi|kitchenette" :age 21
      :travel-purpose "leisure"}
     {:name "Erik Andersson" :country "Sweden" :checkin-until "24:00"
      :avg-price 160 :amenities "gym|wifi|airport-shuttle" :age 29
      :travel-purpose "business"}
     {:name "Anna Lindqvist" :country "Sweden" :checkin-until "22:00"
      :avg-price 140 :amenities "breakfast|pool|wifi|babysitting"
      :age 38 :travel-purpose "leisure"}]))

(deftest test-get-col-groups
  (is (= { :group-123 [{ :col-name :country, :set-n 1 },
                       { :col-name :country, :set-n 0 }],
           :ungroup:0:avg-price [{:col-name :avg-price, :set-n 0}],
           :ungroup:1:checkin-until [{:col-name :checkin-until, :set-n 1}] }
         (get-col-groups
           {}
           [{ :country #{:tags :str :group-123},
              :avg-price #{:int :number-scale} }
            { :country #{:tags :str :group-123},
              :checkin-until #{:str :needs-conv :time-local} }]
           0))))

(deftest test-execute-preprocessing-instructions
  (let [preprocessed
        (execute-preprocessing-instructions
          (:tags-preprocessing hotel-governor)
          [{ :country #{:tags :str :group-123},
             :avg-price #{:int :number-scale} }
           { :country #{:tags :str :group-123},
             :checkin-until #{:str :needs-conv :time-local} }]
          [(with-meta
             hotel-cases
             { :io-settings { :iid "item id" :uid "user id"} }),
           (with-meta
             hotel-options
             { :hello "goodbye" })]),
        prepr-cases (first preprocessed),
        prepr-options (second preprocessed)]
    (testing "metadata preservation"
      (is (= (meta prepr-cases)
             { :io-settings { :iid "item id" :uid "user id" } })
          "preprocessed cases metadata")
      (is (= (meta prepr-options)
             { :hello "goodbye" })
          "preprocessed options metadata"))
    (testing "number columns"
      (is (every? float? (:avg-price prepr-cases))
          "Number column mapped into a float scale when requested")
      (is (= (vec ((.execute ZLogisticScale)
                   (:avg-price hotel-cases)
                   ((.prepare ZLogisticScale) (:avg-price hotel-cases))))
             (:avg-price prepr-cases))
          "The correct scaling function applied"))
    (testing "passthrough columns"
      (is (= (:checkin-until hotel-options)
             (:checkin-until prepr-options))
          "A column with no preprocessing piped through as needed"))
    (testing "grouped columns"
      (is (= [true true true]
             (mapv #(boolean (get prepr-cases %))
                   [:country-USA :country-France :country-Sweden]))
          "All countries encoded for cases")
      (is (= [true true true]
             (mapv #(boolean (get prepr-options %))
                   [:country-USA :country-France :country-Sweden]))
          "All countries encoded for options"))
    (testing "multihot tag encoding"
      (is (= [0.0 0.0 1.0 1.0 0.0 0.0]
             (:country-France prepr-cases)))
      (is (= [1.0 1.0 1.0 0.0 0.0]
             (:country-USA prepr-options)))
      (is (= [0.0 0.0 0.0 0.0 0.0]
             (:country-Sweden prepr-options))))))

; (run-tests 'gmrs.preprocess-test)
