(ns gmrs.mills.informed-popularity-test
  (:require [clojure.test :refer :all]
            [gmrs.mills.informed-popularity :refer :all]
            [gmrs.test-commons :refer :all]
            [gmrs.wrangle :as wrangle]))

(deftest test-random-option-mill
  (let [cases (wrangle/cols-from-row-mask
                hotel-cases
                ;; Select John Smith, Marie Leroy and Anna Lindqvist
                [true false false true false true]),
        recs (random-option-mill
               cases #(make-getter cases) #(make-getter hotel-options)
               #(map wrangle/records-as-cols
                    (partition-all 2 (wrangle/cols-as-rows hotel-inters)))
               #(make-getter {})
               mock-pull-strat)]
    (is (= 3 (count recs)) "only recommend for requested cases")
    (is (= 3 (count (take 3 (get recs "John Smith"))))
        "case 1, John Smith gets recommendations")
    (is (< 0 (:score (first (get recs "John Smith"))))
        "recommendations with score")
    (is ((set (:name hotel-options))
         (:name (first (get recs "John Smith"))))
        "recommendations get actual option IDs")
    (is (= 3 (count (take 3 (get recs "Anna Lindqvist"))))
        "case 2, Anna Lindqvist gets recommendations")
    (is (= 3 (count (take 3 (get recs "Marie Leroy"))))
        "case 3, Marie Leroy gets recommendations")))

; (run-tests 'gmrs.mills.explore-exploit-test)
