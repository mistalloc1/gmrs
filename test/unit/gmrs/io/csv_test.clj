(ns gmrs.io.csv-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [gmrs.io.csv :refer :all]))

(def expected-example
  [{:name "Felipe" :city "Madrid" :age "20"}
   {:name "Maria" :city "Aguilas" :age "30"}
   {:name "Ernesto" :city "La Coruña" :age "25"}])

(deftest test-csv-give
  (with-open [reader (io/reader "test/_res/example.csv")]
    (let [give ((csv-give reader true nil) {:page-size 10})]
      (is (= expected-example
             (first give))
          "reading contents from a CSV file")))
  (with-open [reader (io/reader "test/_res/example.csv")]
    (let [give ((csv-give reader) {:page-size 10})]
      (is (= expected-example
             (first give))
          "default wrap? arg value")))
  (with-open [reader (io/reader "test/_res/example.csv")]
    (let [give ((csv-give reader true :id) {:page-size 10})
          first-page (first give)]
      (is (every? #(contains? % :id) first-page)
          "ident-column? adds ID column")
      (is (= (range 2 (+ 2 (count first-page)))
             (map :id first-page))
          "ident-column? generates consecutive numbers"))))
; (run-tests 'gmrs.io.csv-test)
