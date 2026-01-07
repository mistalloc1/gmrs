(ns gmrs.mills.nearest-options-test
  (:require [clojure.test :refer :all]
            [gmrs.mills.nearest-options :refer :all]
            [gmrs.test-commons :refer :all]
            [gmrs.preprocess :as preproc]
            [gmrs.wrangle :as wrangle]
            [gmrs.mills.scoring-tables :as scot]))

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
            [example-cases example-options]
            [:name :venue-name]),
          recs
          (scot/sorted-rec-options
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
            [example-cases example-options]
            [:name :venue-name]),
          recs
          (scot/sorted-rec-options
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
            [example-cases example-options]
            [:name :venue-name]),
          recs
          (scot/sorted-rec-options
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

(deftest nearest-options-scoring-few-cols
  (is (= (nearest-options-scoring
           {:a [0.0 0.0] :b [0.0 0.0]} {:a [0.0 0.0] :b [0.0 0.0]}
           ["e" "g"] ["x" "y"])
         {["e" "x"] -1.0, ["e" "y"] -1.0, ["g" "x"] -1.0, ["g" "y"] -1.0})
      "get -1.0 where we'd get NaNs from correlations of 0.0s ")
  (is (= (nearest-options-scoring
           {:a [0.0 0.0]} {:a [0.0 0.0]}
           ["e" "g"] ["x" "y"])
         {["e" "x"] -1.0, ["e" "y"] -1.0, ["g" "x"] -1.0, ["g" "y"] -1.0})
      "one column - get -1.0 where we'd get NaNs from correlations of 0.0s "))

(deftest test-interacted-cases-mask
  (is (= ["ferdek/warsaw" nil nil nil]
         (interacted-cases-mask
           example-cases
           (wrangle/records-as-cols [{:case-id "ferdek/warsaw"
                                      :opt-id "Warsaw Jazz"
                                      :inter-id 0}])))))

(deftest test-map-case-inters
  (let [inters [{:case-id "ferdek/warsaw"
                 :opt-id "Warsaw Jazz"
                 :inter-id 0}
                {:case-id "sara/kraków"
                 :opt-id "Warsaw Jazz"
                 :inter-id 1}
                {:case-id "ferdek/warsaw"
                 :opt-id "Kraków Rock"
                 :inter-id 2}]]
    (is (= {"ferdek/warsaw" [(nth inters 0) (nth inters 2)],
            "sara/kraków" [(nth inters 1)],
            "ela/warsaw" [], "alojzy/sandomierz" []}
           (map-case-inters
             example-cases
             (wrangle/records-as-cols inters))))))

;;;
;; Testing mills
;;;

(deftest test-nearest-options-from-interactions-mill
  (let [cases-and-options
        (preproc/execute-preprocessing-instructions
          tag-processing [{:amenities [:tags :str],
                           :avg-price [:number-scale]},
                          {:amenities [:tags :str],
                           :country [:tags :str],
                           :avg-price [:number-scale]}]
          [hotel-cases hotel-options]
          [:name :name]),
        cases (wrangle/cols-from-row-mask
                (first cases-and-options)
                ;; select only the ones to which we gave interactions
                [false true false true true false]),
        options (second cases-and-options),
        recs (nearest-options-from-interactions-mill
               cases {}
               { :inter-id [3], :person-name ["Marie Leroy"],
                :hotel-name ["Dump Hotel"] }
               ;; construct getters for cases, options and inters
               (make-getter cases)
               (make-getter options)
               (map wrangle/records-as-cols
                    (partition-all 2 (wrangle/cols-as-rows hotel-inters)))
               mock-pull-strat)]
    (is (< 0 (count (get recs "Sarah Johnson")))
        "case 1, Sarah Johnson gets recommendations")
    (is (< 0 (count (get recs "Marie Leroy")))
        "case 2, Marie Leroy gets recommendations")
    (is (< 0 (count (get recs "Erik Andersson")))
        "case 3, Erik Andersson gets recommendations")
    (is (not (some #{"Budget Inn"} (map :name (get recs "Sarah Johnson"))))
        "don't recommend for already interacted options")
    (is (and (some #{"Hotel Reims"} (map :name (get recs "Erik Andersson")))
             (< 0.0 (:score
                      (first (filter #(= (:name %) "Hotel Reims")
                                     (get recs "Erik Andersson"))))))
        "the option should be matched because of breakfast tag")))

(deftest test-nearest-options-from-cases-mill
  (let [cases-premask
        (first (preproc/execute-preprocessing-instructions
          tag-processing [{:amenities [:tags :str],
                           :avg-price [:number-scale]}]
          [hotel-cases] [:name])),
        ;; For the target cases as well we need to manually do the preprocessing
        ;; which would be handled by gmrs.command/recommend-to.
        cases (wrangle/cols-from-row-mask
                cases-premask
                ;; Select John Smith, Marie Leroy and Anna Lindqvist
                [true false false true false true]),
        options (preproc/execute-preprocessing-instructions
                        tag-processing [{:amenities [:tags :str],
                                         :country [:tags :str],
                                         :avg-price [:number-scale]}]
                        [hotel-options] [:name]),
        recs (nearest-options-from-cases-mill
               cases {}
               { :inter-id [3 4],
                 :person-name ["Marie Leroy" "Sarah Johnson"],
                 :hotel-name ["Dump Hotel" "Hilton Hotel"] }
               ;; construct getters for cases, options and inters
               (make-getter cases-premask)
               (make-getter options)
               (map wrangle/records-as-cols
                    (partition-all 2 (wrangle/cols-as-rows hotel-inters)))
               mock-pull-strat)]
    (is (= 3 (count recs)) "only recommend for requested cases")
    (is (< 0 (count (get recs "John Smith")))
        "case 1, John Smith gets recommendations")
    (is (< 0 (count (get recs "Anna Lindqvist")))
        "case 2, Anna Lindqvist gets recommendations")
    (is (< 0 (count (get recs "Marie Leroy")))
        "case 3, Marie Leroy gets recommendations")
    (is (not (some #{"Dump Hotel"} (map :name (get recs "Marie Leroy"))))
        "don't recommend for already interacted options")
    (is (and (some #{"Chateau Resort"} (map :name (get recs "John Smith")))
             (< 0.0 (:score
                      (first (filter #(= (:name %) "Chateau Resort")
                                     (get recs "John Smith"))))))
        "option should be matched because of similar Andersson's inters")
    (is (and (some #{"Hilton Hotel"} (map :name (get recs "Anna Lindqvist")))
             (< 0.0 (:score
                      (first (filter #(= (:name %) "Hilton Hotel")
                                     (get recs "Anna Lindqvist"))))))
        "recommendation for Johnson - take max despite Andersson also having it")
    (is (not (some #(close? 0.01 1.0 (:score %))
                   (reduce into [] (vals recs))))
        "no recs should get 1.0 score - contamination from the case itself")))

; (run-tests 'gmrs.mills.nearest-options-test)
