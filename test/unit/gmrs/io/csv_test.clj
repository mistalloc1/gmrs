(ns gmrs.io.csv-test
  (:require [clojure.test :refer :all]
            [gmrs.io.csv :refer :all]))

(def expected-example
  [{:name "Felipe" :city "Madrid" :age "20"}
   {:name "Maria" :city "Aguilas" :age "30"}
   {:name "Ernesto" :city "La Coruña" :age "25"}])

(deftest test-csv-give
  (let [give ((csv-give  "test/_res/example.csv" nil) {:page-size 3})]
    (is (= expected-example
           (first give))
        "reading contents from a CSV file"))
  (let [give ((csv-give  "test/_res/example.csv" :id) {:page-size 3})
        first-page (first give)]
    (is (every? #(contains? % :id) first-page)
        "ident-column? adds ID column")
    (is (= (range 1 (+ 1 (count first-page)))
           (map :id first-page))
        "ident-column? generates consecutive numbers"))
  (testing "getting over 20 pages"
    (let [give ((csv-give  "test/_res/example.csv" :id) {:page-size 3})
          page-21 (nth give 20)]
      (is (= "Ines" (:name (last page-21))))
      (is (= 63 (:id (last page-21)))))))
; (run-tests 'gmrs.io.csv-test)
