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

; (run-tests 'gmrs.math-test)
