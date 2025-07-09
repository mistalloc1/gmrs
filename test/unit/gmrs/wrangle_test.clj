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
                                :city-kraków [0 1]})))))

(deftest test-cols-as-vecs
  (is (= '((1 0 1) (1 0 0) (1 0 2))
         (cols-as-vecs '([1 1 1] [0 0 0] [1 0 2])))
      "base case")
  (is (= (list 1 1 0 0 0 1 0)
         (first (cols-as-vecs (vals 
                                 {:genres-rock [1 0], :genres-dub [1 0],
                                  :genres-rap '(0 0), :genres-jazz [0 1],
                                  :city-warsaw '(0 0), :city-gdańsk [1 0],
                                  :city-kraków [0 1]}))))
      "mixed lists and vecs in input"))

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
      "add empty seq of records to existing columns"))

; (run-tests 'gmrs.wrangle-test)
