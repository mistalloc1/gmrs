(ns gmrs.math-test
  (:require [clojure.test :refer :all]
            [gmrs.math :refer :all]))

(defn close? [tolerance x y]
  (< (Math/abs (double (- x y))) tolerance))

(deftest test-cosine-similarity
  (is (close? 0.001
              0.868282
              (cosine-similarity [0.5 0.4 0.3]
                                 [0.8,0.2,0.1]))))

(deftest test-pearson-correlation
  (is (neg? (pearson-correlation [1.0 0.5] [-2.0 -1.0])))
  (is (pos? (pearson-correlation [1.0 0.5] [2.0 1.0])))
  (is (NaN? (pearson-correlation [1.0 0.0] [0.0 0.0]))))

; (run-tests 'gmrs.math-test)
