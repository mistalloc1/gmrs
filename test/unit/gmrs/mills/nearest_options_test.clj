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
    {:io-settings {:option-id :venue-name}}))
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
    {:io-settings {:option-id :venue-name}}))
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
    (let [recs (nearest-options-recommend example-cases example-options
                                        {:tag-fields '(:genres)
                                         :number-fields ()
                                         :recs-amount 3})]
      ; ferdek/warsaw
      (is (= "Warsaw Jazz" (:venue-name (first (first recs))))
          "top for ferdek")
      (is (pos? (:score (first (first recs))))
          "Warsaw Jazz for ferdek")
      (is (neg? (:score (second (first recs))))
          "Kraków Rock for ferdek")
      ; ela/warsaw
      (is (= "Kraków Rock" (:venue-name (first (second recs)))))
      (is (pos? (:score (first (second recs))))
          "Kraków Rock for ferdek")
      (is (neg? (:score (second (second recs))))
          "Warsaw Jazz for ferdek")
      ; sara/kraków
      (is (every? neg? (map :score (nth recs 2)))
          "no matches and negative correlation for sara")
      ; alojzy/sandomierz
      (is (every? neg? (map :score (nth recs 3)))
          "no matches and negative correlation for alojzy")))
6
  (testing "two features (genres, city)"
    (let [recs (nearest-options-recommend example-cases example-options
                                        {:tag-fields '(:genres :city)
                                         :number-fields ()
                                         :recs-amount 3})]
      ; ferdek/warsaw
      (is (= "Warsaw Jazz" (:venue-name (first (first recs))))
          "top for ferdek")
      (is (pos? (:score (first (first recs))))
          "Warsaw Jazz for ferdek")
      (is (neg? (:score (second (first recs))))
          "Kraków Rock for ferdek")
      ; ela/warsaw
      (is (every? pos? (map :score (second recs)))
          "all options match somewhat for ela")
      ; sara/kraków
      (is (= "Kraków Rock" (:venue-name (first (nth recs 2))))
          "top for sara (city match) ")
      (is (pos? (:score (first (nth recs 2))))
          "Kraków Rock for sara")
      (is (neg? (:score (second (nth recs 2))))
          "Warsaw Jazz for sara")))
 
  (testing "three features (genres, city, volume)"
    (let [recs (nearest-options-recommend example-cases example-options
                                        {:tag-fields '(:genres :city)
                                         :number-fields '(:volume)
                                         :recs-amount 3})]
      ; ferdek/warsaw
      (is (= "Warsaw Jazz" (:venue-name (first (first recs))))
          "top for ferdek")
      (is (pos? (:score (first (first recs))))
          "Warsaw Jazz for ferdek")
      (is (neg? (:score (second (first recs))))
          "Kraków Rock for ferdek")
      ; ela/warsaw
      (is (every? pos? (map :score (second recs)))
          "all options match somewhat for ela")
      ; sara/kraków
      (is (= "Kraków Rock" (:venue-name (first (nth recs 2))))
          "top for sara (city match) ")
      (is (pos? (:score (first (nth recs 2))))
          "Kraków Rock for sara")
      (is (neg? (:score (second (nth recs 2))))
          "Warsaw Jazz for sara")
      ; alojzy/sandomierz
      (is (= "Warsaw Jazz" (:venue-name (first (nth recs 3))))
          "top for alojzy")
      (is (> (:score (first (nth recs 3))) -0.5)
          "Warsaw Jazz for alojzy")
      (is (neg? (:score (second (nth recs 3))))
          "Kraków Rock for alojzy"))))

; (run-tests 'gmrs.mills.nearest-options-test)
