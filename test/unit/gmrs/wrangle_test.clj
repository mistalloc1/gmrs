(ns gmrs.wrangle-test
  (:require [clojure.test :refer :all]
            [gmrs.wrangle :refer :all]))

(deftest test-all-same-length?
  (is (= true (all-same-length? [2 3] [3 1])) "base case")
  (is (= false (all-same-length? [2 3] [3])) "unequal vecs false")
  (is (= true (all-same-length? '([1 0] [1 0] [0 1] [1 0] [0 1]))) "longer seq"))

(deftest test-cols-row-count
  (is (= 2 (cols-row-count {:genres-rock [1 0], :genres-dub [1 0],
                             :genres-jazz [0 1], :city-gdańsk [1 0],
                             :city-kraków [0 1]}))))

(deftest test-fill-missing-cols
  (is (= {:genres-rock [1 0], :genres-funk [0 0], :genres-dub [1 0]}
         (fill-missing-cols {:genres-rock [1 0], :genres-dub [1 0]}
                             (list :genres-rock :genres-funk :genres-dub)))
      "adding a column")
  (is (= {:genres-rock [1 0], :genres-funk [0 0], :genres-dub [1 0]
          :genres-rap [4 6]}
         (fill-missing-cols {:genres-rock [1 0], :genres-dub [1 0]
                              :genres-rap [4 6]}
                             (list :genres-rock :genres-funk :genres-dub)))
      "preserving non-specified but present column"))

(deftest test-cols-as-rows
  (is (= {:genres-rock 1,
          :genres-dub 1,
          :genres-rap 0,
          :genres-jazz 0,
          :city-warsaw 0,
          :city-gdańsk 1,
          :city-kraków 0}
         (first (cols-as-rows {:genres-rock [1 0], :genres-dub [1 0],
                                :genres-rap '(0 0), :genres-jazz [0 1],
                                :city-warsaw '(0 0), :city-gdańsk [1 0],
                                :city-kraków [0 1]})))
      "base case")
  (is (= [{:id "inter5", :case-id 1190, :option-id 310},
          {:id "inter11", :case-id 2130, :option-id 360},
          {:id "inter10", :case-id 2130, :option-id 310},
          {:id "inter3", :case-id 1190, :option-id 175}]
         (take
           4
           (cols-as-rows {:id ["inter5" "inter11" "inter10" "inter3" "inter8"
                               "inter9" "inter1" "inter2" "inter6" "inter12"
                               "inter4" "inter7" "inter13"],
                          :case-id [1190 2130 2130 1190 2130 1190 1190 1190 2130
                                    2140 1190 2140 2140],
                          :option-id [310 360 310 175 140 310 130 175 310 360
                                      245 175 175]}))))
  (is (= 0 (count (cols-as-rows {}))) "no columns")
  (is (= 0 (count (cols-as-rows {:a [] :b []}))) "empty columns"))

(deftest test-derived-column-names
  (is (= [:genres-scifi :genres-horror :length]
         (derived-col-names { :genres-scifi [1] :genres-horror [0]
                              :length [26544] :quality [1] }
                            [:genres :length]))))

(deftest test-cols-from-row-mask
  (is (= {:a [1 4] :b ["X1" "X4"]}
         (cols-from-row-mask { :a [1 2 3 4 5]
                               :b ["X1" "X2" "X3" "X4" "X5"] }
                             [true false false true false]))
      "simple case")
  (is (= {:john "lennon" :mary "stuart"}
         (meta
           (cols-from-row-mask (with-meta
                               { :a [1 2 3 4 5]
                                :b ["X1" "X2" "X3" "X4" "X5"] }
                               {:john "lennon" :mary "stuart"})
                             [true false false true false])))
      "preserving metadata"))

(deftest test-cols-as-row-vecs
  (is (= '((1 0 1) (1 0 0) (1 0 2))
         (cols-as-row-vecs '([1 1 1] [0 0 0] [1 0 2])))
      "base case")
  (is (= (list 1 1 0 0 0 1 0)
         (first (cols-as-row-vecs (vals
                                 {:genres-rock [1 0], :genres-dub [1 0],
                                  :genres-rap '(0 0), :genres-jazz [0 1],
                                  :city-warsaw '(0 0), :city-gdańsk [1 0],
                                  :city-kraków [0 1]}))))
      "mixed lists and vecs in input")
  (is (= (list 1 1 0 0 0 1 0)
         (first (cols-as-row-vecs {:genres-rock [1 0], :genres-dub [1 0],
                                  :genres-rap '(0 0), :genres-jazz [0 1],
                                  :city-warsaw '(0 0), :city-gdańsk [1 0],
                                  :city-kraków [0 1]}
                                 )))
      "a full column map as input"))

(deftest test-records-as-cols
  (is (= { :kind ["fish" "tiger"]
          :color ["silver" "orange"]
          :sound [nil "roar"] }
         (records-as-cols [{:kind "fish" :color "silver"}
                           {:kind "tiger" :color "orange" :sound "roar"}]))
      "simple case with nils")
  (is (= { :kind ["fish" "tiger" "ant"]
          :color ["silver" "orange" nil]
          :sound [nil "roar" nil] }
         (records-as-cols [{:kind "fish" :color "silver"}
                           {:kind "tiger" :color "orange" :sound "roar"}
                           {:kind "ant"}]))
      "simple case with nils, add'l partial record")
  (is (= { :kind ["fish" "tiger" "ant"]
          :color ["silver" "orange" nil]
          :sound [nil "roar" nil] }
         (records-as-cols []
                          { :kind ["fish" "tiger" "ant"]
                           :color ["silver" "orange" nil]
                           :sound [nil "roar" nil]}))
      "add empty seq of records to existing columns")
  (is (= 0 (count (records-as-cols [])))
      "empty input")
  (is (= { :meta-stuff "example" }
         (meta (records-as-cols
                 (with-meta
                   [{:kind "fish" :color "silver"}
                    {:kind "tiger" :color "orange" :sound "roar"}
                    {:kind "ant"}]
                   { :meta-stuff "example" }))))
      "preserving metadata from records")
  (is (= { :meta-stuff "example" }
         (meta (records-as-cols
                 [{:kind "fish" :color "silver"}
                  {:kind "tiger" :color "orange" :sound "roar"}
                  {:kind "ant"}]
                 (with-meta
                   { :kind ["fish" "tiger" "ant"]
                    :color ["silver" "orange" nil]
                    :sound [nil "roar" nil] }
                   { :meta-stuff "example" }))))
      "preserving metadata from existing-cols"))

(deftest test-stack
  (let [data1 { :kind ["fish" "tiger"]
                :color ["silver" "orange"]
                :sound [nil "roar"] }
        data2 { :kind ["moose"] :color ["brown"] :sound ["bellow"] }
        data3 { :kind ["fish" "tiger" "moose"]
                :color ["silver" "orange" "brown"]
                :sound [nil "roar" "bellow"] }
        data4 { :kind ["fish" "tiger" "moose" "moose"]
                :color ["silver" "orange" "brown" "brown"]
                :sound [nil "roar" "bellow" "bellow"] }]
    (is (= data3 (stack data1 data2)))
    (is (= data4 (stack data1 data2 data2)))))

(deftest test-sorted-rec-options
  (is { :c1 [{ :opt-id :o1 :score 0.3 }
             { :opt-id :o2 :score 0.4 }]
        :c2 [{ :opt-id :o1 :score 0.8 }
             { :opt-id :o2 :score 0.2 }] }
      (sorted-rec-options
        (with-meta { [:c1 :o1] 0.3, [:c1 :o2] 0.4,
                     [:c2 :o1] 0.8, [:c2 :o2] 0.2 }
                   { :cases [:c1 :c2] :options [:o1 :o2]
                     :io-settings { :option-id :opt-id } }))))

(deftest test-average-score
  (is (= 0.4 (average-score [1.0 0.1 0.2 0.5 0.2]))))

(deftest test-options-to-cases-scoring-table
  (let [options-scoring-table
        (with-meta
          { [:opt-1 :opt-2] 0.7, [:opt-1 :opt-3] 0.3, [:opt-4 :opt-2] 0.7,
            [:opt-4 :opt-3] 0.7 [:opt-2 :opt-2] 1.0 [:opt-2 :opt-3] 0.0 }
          { :cases [:opt-1 :opt-4 :opt-2]
            :options [:opt-2 :opt-3] }),
        cases-to-options { :case-1 [:opt-1] :case-2 [:opt-1 :opt-4] }]
    (is (= { [:case-1 :opt-2] 0.7, [:case-1 :opt-3] 0.3,
             [:case-2 :opt-2] 0.7, [:case-2 :opt-3] 0.5 }
           (options-to-cases-scoring-table options-scoring-table
                                           cases-to-options)))))

(deftest test-sorted-with-culled-already-interacted
  (is { :case-1 [] :case-2 [{ :opt-id :opt-2 :score 0.7 }] }
      (sorted-with-culled-already-interacted
        (with-meta
          { [:case-1 :opt-2] 0.7, [:case-1 :opt-3] 0.3,
            [:case-2 :opt-2] 0.7, [:case-2 :opt-3] 0.5 }
          { :cases [:case-1 :case-2] :options [:opt-2 :opt-3]
            :io-settings { :option-id :opt-id} })
        { :case-1 [:opt-1 :opt-2 :opt-3] :case-2 [:opt-3] })))

(deftest test-keywordify
  (is (= [:dill :dandelion :daisy]
         (keywordify ["dill" "dandelion" "daisy"]))
      "simple case")
  (is (= [:dill :common-dandelion :Lawn-daisy]
         (keywordify ["dill" "common/dandelion" "Lawn daisy"]))
      "extra chars replaced with dashes")
  (is (= [:dill :dandelion :common-daisy :common-daisy-001]
         (keywordify ["dill" "dandelion" "common-daisy" "common daisy"]))
      "collision in simplified names"))

; (run-tests 'gmrs.wrangle-test)
