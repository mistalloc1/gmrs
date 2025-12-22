(ns gmrs.data-diag-test
  (:require [clojure.test :refer :all]
            [gmrs.data-diag :refer :all]))

(deftest test-tags-map-usable?
  (is (= true
         (tags-map-usable? {:a 3 :b 2 :c 1 :d 4} 16)))
  (is (= false
         (tags-map-usable? {:a 3 :b 2 :c 1 :d 4} 8))))

(def example-attrib-map {:str 4 :date-local 3})

(deftest test-multimodal-dist?
  (is (false? (multimodal-dist?
                (apply concat (map (fn [i] (map #(+ i %)
                                                [72.0 84.0 45.0 88.0 99.0]))
                                   (range 10))))))
  (is (true? (multimodal-dist? (concat (repeat 10 404)
                                       (repeat 40 200)
                                       (repeat 33 503))))))

(deftest test-heuristic-is-datetime?
  (is (= false (heuristic-is-datetime? "dragons")))
  (is (= false (heuristic-is-datetime? "324253356 dragons")))
  ;; could reverse in the future:
  (is (= true (heuristic-is-datetime? "1900 dragons")))
  (is (= true (heuristic-is-datetime? "2023-05-05")))
  (is (= true (heuristic-is-datetime? "13:04")))
  (is (= true (heuristic-is-datetime? "2004-11-13T00:00:00+00:00"))))

(deftest test-is-numtype?
  (is (boolean (is-numtype? "23" #{\- \+} Integer/parseInt)))
  (is (not (boolean (is-numtype? "23.5" #{\- \+} Integer/parseInt))))
  (is (boolean (is-numtype? "23.5" #{\- \+ \e \.} Float/parseFloat)))
  (is (boolean (is-numtype? "23" #{\- \+ \e \.} Float/parseFloat)))
  (is (not (boolean (is-numtype? "pasta23" #{\- \+ \e \.} Float/parseFloat))))
  (is (not (boolean (is-numtype? "pasta23.5" #{\- \+} Integer/parseInt)))))

(deftest test-diag-potential-datetime
  (is (= {:date-local 4 :date-local-needs-conv 1}
         (diag-potential-datetime "2023-05-05" example-attrib-map)))
  (is (= {:time-local 1 :time-local-needs-conv 1}
         (diag-potential-datetime "12:05" example-attrib-map)))
  (is (= {:time-local 1 :time-local-needs-conv 1}
         (diag-potential-datetime "   12:05  " example-attrib-map)))
  (is (= {:date-zoned-with-time 1 :date-zoned-with-time-needs-conv 1}
         (diag-potential-datetime "2004-11-13T00:00:00+00:00"
                                  example-attrib-map)))
  (is (= {:date-zoned-with-time 1 :date-zoned-with-time-needs-conv 1}
         (diag-potential-datetime "     2004-11-13T00:00:00+00:00  "
                                  example-attrib-map))))

(deftest test-diag-string-and-update
  "Correctness of these steps assumes that diag-string only adds correct update
  data for the new string and doesn't really care about what was in the supplied
  attrib-map."
  (is (= {:str 5 :date-local 3 :tags 1
          :maybe-tags-map {"635" 1 "364" 1}}
         (diag-string-and-update "635|364" example-attrib-map 10)))
  (is (= {:str 5 :date-local 3 :integer 1 :float 1 :integer-needs-conv 1 :tags 1
          :float-needs-conv 1 :maybe-tags-map {"635364" 1}}
         (diag-string-and-update "635364" example-attrib-map 10)))
  (is (= {:str 5 :date-local 3 :float 1 :tags 1
          :float-needs-conv 1 :maybe-tags-map {"635.364" 1}}
         (diag-string-and-update "635.364" example-attrib-map 10)))
  (is (= {:str 5 :date-local 4 :tags 1 :date-local-needs-conv 1
          :maybe-tags-map {"2023-05-05" 1}}
         (diag-string-and-update "2023-05-05" example-attrib-map 10)))
  (is (= {:str 5 :date-local 3 :time-local 1 :time-local-needs-conv 1 :tags 1
          :maybe-tags-map {"12:05" 1}}
         (diag-string-and-update "12:05" example-attrib-map 10)))
  (is (= {:str 5 :date-local 3 :date-zoned-with-time 1 :tags 1
          :date-zoned-with-time-needs-conv 1
          :maybe-tags-map {"2004-11-13T00:00:00+00:00" 1}}
         (diag-string-and-update "2004-11-13T00:00:00+00:00" example-attrib-map 10))))

(deftest test-prefer-keyword
  (is #{:foo :baz}
      (prefer-keyword #{:foo :bar :baz} :foo :bar))
  (is #{:bar :baz}
      (prefer-keyword #{:bar :baz} :foo :bar))
  (is #{:foo :baz}
      (prefer-keyword #{:foo :baz} :foo :bar)))

(deftest test-diag-all-values
  (is (= #{:float}
         (diag-all-values [45.3 124.3 25643.43 3655.5 43245.4])))
  (is (= #{:float :str :float-needs-conv}
         (diag-all-values ["45.3" "124.3" "25643.43" "3655.5" "43245.4"])))
  (is (= #{:float :str :float-needs-conv}
         (diag-all-values ["45.3 " "124.3 " "25643.43 " "3655.5 " "43245.4"])))
  (is (= #{:integer :str :integer-needs-conv}
         (diag-all-values [" 45 " "124 " "25643 " "3655 " "43245"]))
      "prefer integer to float"))

#_(diag-all-values
  ["1.0" "23.0" "16.0" "5.0" "1.0" "11.0" "20.0" "27.0" "0.0" "15.0" "0.0" "34.0"
   "30.0" "2.0" "13.0" "4.0" "6.0" "3.0" " 2.0" "4.0" "1.0" "221.0" "14.0" "2.0"
   "2.0" "20.0" "46.0" "7.0" "1.0" "7.0" "34.0" "10.0"])

; (run-tests 'gmrs.data-diag-test)
