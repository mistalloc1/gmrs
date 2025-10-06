(ns gmrs.mills.nearest-options-test
  (:require [clojure.test :refer :all]
            [gmrs.mills.nearest-options :refer :all]
            [gmrs.preprocess :as preproc]
            [gmrs.wrangle :as wrangle]
            [gmrs.command :refer [prepr-with-ids]]))

(defn close? [tolerance x y]
  (< (Math/abs (double (- x y))) tolerance))

(def tag-processing
  { :tags preproc/multihot-from-tags
    :number-scale (fn [coll]
                    (preproc/apply-z-logistic-scale
                      coll
                      (preproc/z-logistic-scale coll))) })

(def example-meta
  {:io-settings { :option-id :venue-name
                  :case-id :name
                  :inter-case :case-id
                  :inter-option :opt-id
                  :inter-id :inter-id }})

(def example-cases
  (with-meta
    (wrangle/records-as-cols
      [{:name "ferdek/warsaw"
        :genres "jazz|dub"
        :volume 45
        :city "Warsaw"}
       {:name "ela/warsaw"
        :genres "rock"
        :volume 50
        :city "Warsaw"}
       {:name "sara/kraków"
        :genres "rap"
        :volume 60
        :city "Kraków"}
       {:name "alojzy/sandomierz"
        :genres "oldies"
        :volume 70
        :city "Sandomierz"}])
    example-meta))

(def example-options
  (with-meta
    (wrangle/records-as-cols
      [{:venue-name "Warsaw Jazz"
        :genres "jazz"
        :volume 40
        :city "Warsaw"}
       {:venue-name "Kraków Rock"
        :genres "goth|rock"
        :volume -696
        :city "Kraków"}])
    example-meta))

(deftest test-nearest-options-scoring
  (testing "one feature (genres)"
    (let [cases-and-options
          (preproc/execute-preprocessing-instructions
            tag-processing [{:genres [:tags :str :group-g],
                             :name [:str]},
                            {:genres [:tags :str :group-g],
                             :venue-name [:str]}]
            [example-cases example-options]),
          recs
          (wrangle/sorted-rec-options
            (nearest-options-scoring
              (select-keys (first cases-and-options)
                           (wrangle/derived-col-names (first cases-and-options)
                                                      [:genres]))
              (select-keys (second cases-and-options)
                           (wrangle/derived-col-names (second cases-and-options)
                                                      [:genres]))
              (:name example-cases)
              (:venue-name example-options)))]
      (is (= "Warsaw Jazz" (:venue-name (first (recs "ferdek/warsaw"))))
          "top for ferdek")
      (is (pos? (:score (first (recs "ferdek/warsaw"))))
          "Warsaw Jazz for ferdek")
      (is (neg? (:score (second (recs "ferdek/warsaw"))))
          "Kraków Rock for ferdek")
      (is (= "Kraków Rock" (:venue-name (first (recs "ela/warsaw")))))
      (is (pos? (:score (first (recs "ela/warsaw"))))
          "Kraków Rock for ferdek")
      (is (neg? (:score (second (recs "ela/warsaw"))))
          "Warsaw Jazz for ferdek")
      (is (every? neg? (map :score (recs "sara/kraków")))
          "no matches and negative correlation for sara")
      (is (every? neg? (map :score (recs "alojzy/sandomierz")))
          "no matches and negative correlation for alojzy")))

  (testing "two features (genres, city)"
    (let [cases-and-options
          (preproc/execute-preprocessing-instructions
            tag-processing [{:genres [:tags :str :group-g],
                             :city [:tags :str :group-c],
                             :name [:str]},
                            {:genres [:tags :str :group-g],
                             :city [:tags :str :group-c],
                             :venue-name [:str]}]
            [example-cases example-options]),
          recs
          (wrangle/sorted-rec-options
            (nearest-options-scoring
              (select-keys (first cases-and-options)
                           (wrangle/derived-col-names (first cases-and-options)
                                                      [:genres :city]))
              (select-keys (second cases-and-options)
                           (wrangle/derived-col-names (second cases-and-options)
                                                      [:genres :city]))
              (:name example-cases)
              (:venue-name example-options)))]
      (is (= "Warsaw Jazz" (:venue-name (first (recs "ferdek/warsaw"))))
          "top for ferdek")
      (is (pos? (:score (first (recs "ferdek/warsaw"))))
          "Warsaw Jazz for ferdek")
      (is (neg? (:score (second (recs "ferdek/warsaw"))))
          "Kraków Rock for ferdek")
      (is (every? pos? (map :score (recs "ela/warsaw")))
          "all options match somewhat for ela")
      (is (= "Kraków Rock" (:venue-name (first (recs "sara/kraków"))))
          "top for sara (city match) ")
      (is (pos? (:score (first (recs "sara/kraków"))))
          "Kraków Rock for sara")
      (is (neg? (:score (second (recs "sara/kraków"))))
          "Warsaw Jazz for sara")))

  (testing "three features (genres, city, volume)"
    (let [cases-and-options
          (preproc/execute-preprocessing-instructions
            tag-processing [{:genres [:tags :str :group-g],
                             :city [:tags :str :group-c],
                             :volume [:int :number-scale :group-v],
                             :name [:str]},
                            {:genres [:tags :str :group-g],
                             :city [:tags :str :group-c]
                             :volume [:int :number-scale :group-v],
                             :venue-name [:str]}]
            [example-cases example-options]),
          recs
          (wrangle/sorted-rec-options
            (nearest-options-scoring
              (select-keys (first cases-and-options)
                           (wrangle/derived-col-names (first cases-and-options)
                                                      [:genres :city :volume]))
              (select-keys (second cases-and-options)
                           (wrangle/derived-col-names (second cases-and-options)
                                                      [:genres :city :volume]))
              (:name example-cases)
              (:venue-name example-options)))]
      (is (= "Warsaw Jazz" (:venue-name (first (recs "ferdek/warsaw"))))
          "top for ferdek")
      (is (pos? (:score (first (recs "ferdek/warsaw"))))
          "Warsaw Jazz for ferdek")
      (is (neg? (:score (second (recs "ferdek/warsaw"))))
          "Kraków Rock for ferdek")
      (is (every? pos? (map :score (recs "ela/warsaw")))
          "all options match somewhat for ela")
      (is (= "Kraków Rock" (:venue-name (first (recs "sara/kraków"))))
          "top for sara (city match) ")
      (is (pos? (:score (first (recs "sara/kraków"))))
          "Kraków Rock for sara")
      (is (neg? (:score (second (recs "sara/kraków"))))
          "Warsaw Jazz for sara")
      (is (= "Warsaw Jazz" (:venue-name (first (recs "alojzy/sandomierz"))))
          "top for alojzy")
      (is (> (:score (first (recs "alojzy/sandomierz"))) -0.5)
          "Warsaw Jazz for alojzy")
      (is (neg? (:score (second (recs "alojzy/sandomierz"))))
          "Kraków Rock for alojzy"))))

(deftest test-interacted-cases-mask
  (is (= ["ferdek/warsaw" nil nil nil]
         (interacted-cases-mask
           example-cases
           (wrangle/records-as-cols [{:case-id "ferdek/warsaw"
                                      :opt-id "Warsaw Jazz"
                                      :inter-id 0}])))))

(deftest test-map-case-inters
  (is (= {"ferdek/warsaw" [0 2], "sara/kraków" [1],
          "ela/warsaw" [], "alojzy/sandomierz" []}
         (map-case-inters
           example-cases
           (wrangle/records-as-cols [{:case-id "ferdek/warsaw"
                                      :opt-id "Warsaw Jazz"
                                      :inter-id 0}
                                     {:case-id "sara/kraków"
                                      :opt-id "Warsaw Jazz"
                                      :inter-id 1}
                                     {:case-id "ferdek/warsaw"
                                      :opt-id "Kraków Rock"
                                      :inter-id 2}])))))

;;;
;; Testing mills
;;;

(def hotel-io-settings
  { :case-id :name :option-id :name :inter-id :inter-id
    :inter-case :person-name :inter-option :hotel-name })

(def hotel-cases
  (with-meta
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
        :age 38 :travel-purpose "leisure"}])
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

(defn mock-pull-strat [_ step-n] (< step-n 5))

(deftest test-nearest-options-from-interactions-mill
  (let [cases-and-options
        (preproc/execute-preprocessing-instructions
          tag-processing [{:amenities [:tags :str],
                           :avg-price [:number-scale]},
                          {:amenities [:tags :str],
                           :country [:tags :str],
                           :avg-price [:number-scale]}]
          [(wrangle/cols-from-row-mask
             hotel-cases
             ;; select only the ones to which we gave interactions
             [false true false true true false])
           hotel-options]),
        cases (prepr-with-ids
                (first cases-and-options)
                :name hotel-cases),
        options (prepr-with-ids
                  (second cases-and-options)
                  :name hotel-options),
        recs (nearest-options-from-interactions-mill
               cases {}
               {:inter-id [3], :person-name ["Marie Leroy"],
                 :hotel-name ["Dump Hotel"]}
               ;; construct getters for options and inters
               (map (fn [row-page page-ids]
                      (with-meta
                        (prepr-with-ids
                          (wrangle/records-as-cols row-page)
                          :name {:name page-ids})
                        (meta options)))
                    (partition-all 2 (wrangle/cols-as-rows options))
                    (partition-all 2 (:name hotel-options)))
               (map wrangle/records-as-cols
                    (partition-all 2 (wrangle/cols-as-rows hotel-inters)))
               mock-pull-strat)]
    (is (< 0 (count (get recs "Sarah Johnson")))
        "case 1, Sarah Johnson gets recommendations")
    (is (< 0 (count (get recs "Marie Leroy")))
        "case 2, Marie Leroy gets recommendations")
    (is (< 0 (count (get recs "Erik Andersson")))
        "case 3, Erik Andersson gets recommendations")))
(run-test test-nearest-options-from-interactions-mill)

; (run-tests 'gmrs.mills.nearest-options-test)
