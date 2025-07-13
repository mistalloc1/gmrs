(ns gmrs.io.csv-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [gmrs.io.csv :refer :all]))

(deftest test-csv-give
  (with-open [reader (io/reader "test/_res/example.csv")]
    (let [give (csv-give reader true)]
      (is (= [{:name "Felipe" :city "Madrid" :age "20"}
              {:name "Maria" :city "Aguilas" :age "30"}
              {:name "Ernesto" :city "La Coruña" :age "25"}]
             (give {:page-size 10}))
          "reading contents from a CSV file")))
  (with-open [reader (io/reader "test/_res/example.csv")]
    (let [give (csv-give reader)]
      (is (= [{:name "Felipe" :city "Madrid" :age "20"}
              {:name "Maria" :city "Aguilas" :age "30"}
              {:name "Ernesto" :city "La Coruña" :age "25"}]
             (give {:page-size 10}))
          "default wrap? arg value"))))

; (run-tests 'gmrs.io.csv-test)
