(ns gmrs.test-commons
  (:require [gmrs.wrangle :as wrangle]))

;;
;; These are the simple versions of "hotel" data, meant for testing mills
;; but not deeper data handling.
;;

(def hotel-io-settings
  { :case-id :name :option-id :name :inter-id :inter-id
    :inter-case :person-name :inter-option :hotel-name })

(def hotel-cases
  (with-meta
    (wrangle/records-as-cols
      [{:name "John Smith" :country "USA" :checkin-until "22:00"
        :avg-price 180 :amenities "pool|wifi|pet-friendly" :age 34
        :travel-purpose "business" :pets-amount "0"}
       {:name "Sarah Johnson" :country "USA" :checkin-until "20:00"
        :avg-price 50 :amenities "wifi|electric-car-charging" :age 78
        :travel-purpose "leisure" :pets-amount "3"}
       {:name "Pierre Dubois" :country "France" :checkin-until "23:00"
        :avg-price 120 :amenities "breakfast|wifi|bicycle-rental" :age 45
        :travel-purpose "business" :pets-amount "1"}
       {:name "Marie Leroy" :country "France" :checkin-until "21:00"
        :avg-price 90 :amenities "wifi|kitchenette" :age 21
        :travel-purpose "leisure" :pets-amount "2"}
       {:name "Erik Andersson" :country "Sweden" :checkin-until "24:00"
        :avg-price 160 :amenities "gym|wifi|airport-shuttle" :age 29
        :travel-purpose "business" :pets-amount "0"}
       {:name "Anna Lindqvist" :country "Sweden" :checkin-until "22:00"
        :avg-price 140 :amenities "breakfast|pool|wifi|babysitting"
        :age 38 :travel-purpose "leisure" :pets-amount "1"}])
    { :io-settings hotel-io-settings }))

(def hotel-options
  (with-meta
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
        :row-id 16}])
    { :io-settings hotel-io-settings }))

(def hotel-inters
  (with-meta
    (wrangle/records-as-cols
      [{:inter-id 0 :person-name "Sarah Johnson" :hotel-name "Budget Inn"}
       {:inter-id 1 :person-name "Erik Andersson" :hotel-name "Hilton Hotel"}
       {:inter-id 2 :person-name "Erik Andersson" :hotel-name "Chateau Resort"}])
    { :io-settings hotel-io-settings }))

(defn mock-pull-strat [_ step-n] (< step-n 15))

(defn make-getter
  "Make a mock getter of 2-item pages from prepared data in columnar format."
  [item-cols]
  (map (fn [row-page]
         (with-meta
           (wrangle/records-as-cols row-page)
           (meta item-cols)))
       (partition-all 2 (wrangle/cols-as-rows item-cols))))
