(ns gmrs.mills.nearest-options-test
  (:require [clojure.test :refer :all]
            [gmrs.mills.nearest-options :refer :all]
            [gmrs.preprocess :as preproc]
            [gmrs.wrangle :as wrangle]))

(defn close? [tolerance x y]
  (< (Math/abs (double (- x y))) tolerance))

(def tag-processing
  { :tags preproc/multihot-from-tags
    :number-scale (fn [coll]
                    (preproc/apply-z-logistic-scale
                      coll
                      (preproc/z-logistic-scale coll))) })

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
              (:name (first cases-and-options))
              (:venue-name (second cases-and-options))))]
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
              (:name (first cases-and-options))
              (:venue-name (second cases-and-options))))]
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
              (:name (first cases-and-options))
              (:venue-name (second cases-and-options))))]
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

; (run-tests 'gmrs.mills.nearest-options-test)
