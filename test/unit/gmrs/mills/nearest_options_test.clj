(ns gmrs.mills.nearest-options-test
  (:require [clojure.test :refer :all]
            [gmrs.mills.nearest-options :refer :all]
            [gmrs.preprocess :as preprocess]
            [gmrs.wrangle :as wrangle]))

(defn close? [tolerance x y]
  (< (Math/abs (double (- x y))) tolerance))

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
    {:io-settings {:option-id :venue-name
                   :case-id :name}}))
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
    {:io-settings {:option-id :venue-name
                   :case-id :name}}))
(def volume-transf { :volume
                    (preprocess/z-logistic-scale
                      (concat (:volume example-cases)
                              (:volume example-options))) })

(deftest test-encoded-columns
  (let [encoded-options
        (encoded-columns example-options '(:genres :city) volume-transf),
        encoded-cases
        (encoded-columns example-cases '(:genres :city) volume-transf)]
    (testing "encoded options"
      (is (= [0.0 1.0]
             (:genres-rock encoded-options))
          "basic multi hot")
      (is (every? #(and (pos? %) (< % 1))
                  (:volume encoded-options))
          "volume in expected range for scaling")
      (is (> (first (:volume encoded-options)) (second (:volume encoded-options)))
          "correlation preserves the correct greater-than")
      (is (every? true? (map (fn [x y] (close? 0.001 x y))
                             (:volume encoded-options)
                             [0.5903728960666603, 0.11506913446634476]))
          "options number encoding - regression"))
    (testing "encoded cases"
      (is (= [0.0 0.0 1.0 0.0]
             (:genres-rap encoded-cases))
          "basic multi hot")
      (is (every? true? (map (fn [x y] (close? 0.001 x y))
                             (:volume encoded-cases)
                             [0.5943189145352167, 0.5982527878236933,
                              0.6060822379880739, 0.613857589450869]))
          "cases number encoding - regression"))))

(deftest test-nearest-options-recommend
  (testing "one feature (genres)"
    (let [recs
          (wrangle/sort-rec-options
            (nearest-options-recommend example-cases example-options
                                       {:tag-fields '(:genres)
                                        :number-fields ()}))]
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
6
  (testing "two features (genres, city)"
    (let [recs
          (wrangle/sort-rec-options
            (nearest-options-recommend example-cases example-options
                                       {:tag-fields '(:genres :city)
                                        :number-fields ()
                                        :recs-amount 3}))]
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
    (let [recs
          (wrangle/sort-rec-options
            (nearest-options-recommend example-cases example-options
                                       {:tag-fields '(:genres :city)
                                        :number-fields '(:volume)
                                        :recs-amount 3}))]
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
(run-test test-nearest-options-recommend)

; (run-tests 'gmrs.mills.nearest-options-test)
