(ns gmrs.preprocess-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [gmrs.preprocess :refer :all]
            [gmrs.wrangle :as wrangle])
  (:import [gmrs.preprocess PreprocessingTransform]))

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
  (is (every? true? (map = (double-array (repeat 4 0))
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

(deftest test-get-multihot-values
  (is (= #{:jazz :rock :dub :rap}
         (get-multihot-values (example-cases :genres)))))

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
         (multihot-from-tags (example-cases :genres) nil "genre/"))
      "custom prefix")
  (is (= {:genre/punk [0.0 0.0 0.0],
          :genre/rock [0.0 1.0 0.0],
          :genre/rap [0.0 0.0 1.0]}
         (multihot-from-tags (example-cases :genres)
                             #{:rock :rap :punk}
                             "genre/"))
      "using a predetermined values-set"))

(deftest test-MultihotFromTags
  (let [prep ((.prepare MultihotFromTags)
              (:genres
                (wrangle/cols-from-row-mask example-cases [true true false])))]
    ;; rap will be skipped because it wasn't seen
    (is (= {:proc-dub [1.0 0.0 0.0],
            :proc-jazz [1.0 0.0 1.0],
            :proc-rock [0.0 1.0 0.0]}
           ((.execute MultihotFromTags) (example-cases :genres) prep)))))

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

;; Test PreprocessingTransforms.
(defn append-init-count-prepare [coll]
  { :init-count (count coll) })
(defn append-init-count-execute [coll transf]
  (map (fn [elem] (str (:init-count transf) elem))
       coll))
(def AppendInitialCount (->PreprocessingTransform append-init-count-prepare
                                                  append-init-count-execute
                                                  5))

(defn to-upper-prepare [coll] "Upper preparation")
(defn to-upper-execute [coll transf] (map str/upper-case coll))
(def ToUpper (->PreprocessingTransform to-upper-prepare to-upper-execute 5))

(deftest test-get-col-groups
  (is (= { :group-123 [{ :col-name :country, :set-n 1 },
                       { :col-name :country, :set-n 0 }],
           :ungroup:0:avg-price [{:col-name :avg-price, :set-n 0}],
           :ungroup:1:checkin-until [{:col-name :checkin-until, :set-n 1}] }
         (get-col-groups
           [{ :country #{:tags :str :group-123},
              :avg-price #{:int :number-scale} }
            { :country #{:tags :str :group-123},
              :checkin-until #{:str :needs-conv :time-local} }]))
      "base case")
  (is (= { :ungroup:0:avg-price [{:col-name :avg-price, :set-n 0}],
           :ungroup:0:country [{:col-name :country, :set-n 0}],
           :ungroup:1:country [{:col-name :country, :set-n 1}],
           :ungroup:1:checkin-until [{:col-name :checkin-until, :set-n 1}] }
         (get-col-groups
           [{ :country #{:tags :str},
              :avg-price #{:int :number-scale} }
            { :country #{:int},
              :checkin-until #{:str :needs-conv :time-local} }]))
      "don't group same names unless instructed"))

(deftest test-prepared-transf-for-tag
  (let [tags-table { :upper ToUpper :append-count AppendInitialCount
                    :tags MultihotFromTags :number-scale ZLogisticScale }]
    (is (= ["3a" "3b" "3c"]
           ((prepared-transf (get tags-table :append-count) ["a" "b" "c"])
            ["a" "b" "c"])))))

(deftest test-get-groups-to-ready-transfs
  (let [groups-to-transfs
        (get-groups-to-ready-transfs
          { :upper ToUpper :append-count AppendInitialCount
            :tags MultihotFromTags :number-scale ZLogisticScale }
          [{ :country [:str :append-count :group-1]
             :name [:upper]
             :checkin-until #{:str} }
           { :country [:str :append-count :group-1]
             :avg-price [:number-scale :int] }]
          [hotel-cases hotel-options]
          (get-col-groups [{ :country [:str :append-count :group-1]
                             :name [:upper]
                             :checkin-until #{:str} }
                           { :country [:str :append-count :group-1]
                             :avg-price [:number-scale :int] }]))]
    (is (= #{:group-1 :ungroup:0:name :ungroup:1:avg-price}
           (set (keys groups-to-transfs)))
        "correct number of groups")
    (is (= ["JOHN SMITH" "SARAH JOHNSON" "PIERRE DUBOIS"
            "MARIE LEROY" "ERIK ANDERSSON" "ANNA LINDQVIST"]
           ((:ungroup:0:name groups-to-transfs)
            (:name hotel-cases)))
        "simple uppercase transform applied")
    (is (map (partial close? 0.001)
             (vec ((.execute ZLogisticScale)
                   (:avg-price hotel-options)
                   ((.prepare ZLogisticScale) (:avg-price hotel-options))))
             ((:ungroup:1:avg-price groups-to-transfs)
              (:avg-price hotel-options)))
        "the same scaling as expected is applied for :avg-price")
    (is (= ["11USA" "11USA" "11France" "11France" "11Sweden" "11Sweden"]
           ((:group-1 groups-to-transfs) (:country hotel-cases)))
        "transform for :group-1 was prepared for the lumped column"))
  (testing "tolerating preprocess failure"
    ;; TODO: test for the tap
    (let [groups-to-transfs
          (get-groups-to-ready-transfs
            { :upper ToUpper :append-count AppendInitialCount
              :tags MultihotFromTags :number-scale ZLogisticScale }
            [{ :name [:number-scale] }
             { :avg-price [:tags] }]
            [hotel-cases hotel-options]
            (get-col-groups [{ :name [:number-scale] }
                             { :avg-price [:tags] }]))]
      (is (empty? (keys groups-to-transfs))))))

(deftest test-retag-with-preproc-transforms
  (let [tags-and-transfs
        (retag-with-preproc-transforms
            (:tags-preprocessing hotel-governor)
            [{ :country #{:tags :str :group-123},
               :avg-price #{:int :number-scale}
               :checkin-until #{:str} }
             { :country #{:tags :str :group-123},
               :checkin-until #{:str :needs-conv :time-local} }]
            [hotel-cases hotel-options])
        grouped-taggings (:set-taggings tags-and-transfs),
        tags-table (:tags-table tags-and-transfs)]
    (is (= #{:tags :str :group-123}
           (:country (nth grouped-taggings 0)))
        "should be left the same 1")
    (is (= #{:tags :str :group-123}
           (:country (nth grouped-taggings 1)))
        "should be left the same 2")
    (is (= #{:ungroup:0:avg-price :int :number-scale}
           (:avg-price (nth grouped-taggings 0)))
        "add the ungroup tag")
    (is (= #{:group-123 :ungroup:0:avg-price}
           (set (keys tags-table)))
        "only leave group entries in prepr table, don't include empty transform")
    (is (= [0.0 0.0 1.0 1.0 0.0 0.0]
           (:proc-France ((:group-123 tags-table)
                          (:country hotel-cases))))
        "a grouped transform performs correctly")
    (is  (map (partial close? 0.001)
             (vec ((.execute ZLogisticScale)
                   (:avg-price hotel-cases)
                   ((.prepare ZLogisticScale) (:avg-price hotel-cases))))
             ((:ungroup:0:avg-price tags-table)
              (:avg-price hotel-cases)))
        "an ugrouped transform performs correctly")))

(deftest test-execute-preprocessing-instructions-base
  (let [preprocessed
        (execute-preprocessing-instructions
          (:tags-preprocessing hotel-governor)
          [{ :country #{:tags :str :group-123},
             :avg-price #{:int :number-scale}
             :checkin-until () }
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
    (testing "skipping columns with no preprocessing"
      (is (not (get prepr-cases :amenities)))
      (is (not (get prepr-options :amenities)))
      (is (not (get prepr-cases :checkin-until)))
      (is (not (get prepr-options :name))))
    (testing "number columns"
      (is (every? float? (:avg-price prepr-cases))
          "Number column mapped into a float scale when requested")
      (is (= (vec ((.execute ZLogisticScale)
                   (:avg-price hotel-cases)
                   ((.prepare ZLogisticScale) (:avg-price hotel-cases))))
             (:avg-price prepr-cases))
          "The correct scaling function applied"))
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

(deftest test-execute-preprocessing-instructions-retagged
  (let [tags-and-transfs
        (retag-with-preproc-transforms
          (:tags-preprocessing hotel-governor)
          [{ :country #{:tags :str :group-123},
             :avg-price #{:int :number-scale} }
           { :country #{:tags :str :group-123},
             :checkin-until #{:str :needs-conv :time-local} }]
          [hotel-cases hotel-options]),
        grouped-taggings (:set-taggings tags-and-transfs),
        grouped-tags-table (:tags-table tags-and-transfs),
        preprocessed (execute-preprocessing-instructions
                       grouped-tags-table grouped-taggings
                       [hotel-cases hotel-options]),
        prepr-cases (first preprocessed),
        prepr-options (second preprocessed)]
    (is (= (vec ((.execute ZLogisticScale)
                 (:avg-price hotel-cases)
                 ((.prepare ZLogisticScale) (:avg-price hotel-cases))))
           (:avg-price prepr-cases))
        "The correct scaling function applied")
    (is (= [0.0 0.0 0.0 0.0 0.0]
           (:country-Sweden prepr-options)))
    (testing "skipping columns with no preprocessing"
      (is (not (get prepr-cases :amenities)))
      (is (not (get prepr-options :amenities)))
      (is (not (get prepr-cases :checkin-until)))
      (is (not (get prepr-options :name))))))

; (run-tests 'gmrs.preprocess-test)
