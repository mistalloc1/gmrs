(ns gmrs.preprocess-test
  (:require [clojure.test :refer :all]
            [gmrs.preprocess :refer :all]
            [gmrs.wrangle :as wrangle]))

(defn close? [tolerance x y]
  (< (Math/abs (double (- x y))) tolerance))

(deftest test-z-logistic-scale
  (let [transf (z-logistic-scale [0 1 2 5])]
    (is (= 2.0 (:mean transf)) "mean present")
    (is (close? 0.001 (:sd transf) 2.160246899469287)
        "standard deviation present")))

(deftest test-apply-z-logistic-scale
  ; the intermediate Z-scaled stage before sigmoid is roughly
  ; [0.26080, -0.51857, -1.01786, 1.27562]
  (is (every? true? (map (partial close? 0.001)
                         (apply-z-logistic-scale [43 4.6 -20 93]
                                                 {:mean 30.15 :sd 49.27})
                         [0.56483486, 0.37318641, 0.26544431, 0.78170398]))
      "basic case")
  (is (every? true? (map = (double-array (repeat 4.0 0))
                         (apply-z-logistic-scale [43 4.6 -20 93]
                                                 {:mean 30.15 :sd 0.0})))
      "0 standard dev case"))

(def example-cases
  (wrangle/records-as-cols
    [{:name "ferdek"
      :genres "jazz|dub"
      :coolness-rating 45
      :city "warsaw"}
     {:name "ela"
      :genres "rock"
      :coolness-rating 50
      :city "warsaw"}
     {:name "sara"
      :genres "jazz|rap"
      :coolness-rating 60
      :city "gdańsk"}]))

(deftest test-multihot-from-tags 
  (is (= {:tag-dub [1.0 0.0 0.0],
          :tag-jazz [1.0 0.0 1.0],
          :tag-rock [0.0 1.0 0.0],
          :tag-rap [0.0 0.0 1.0]}
         (multihot-from-tags (example-cases :genres)))
      "base case")
  (is (= {:genre/dub [1.0 0.0 0.0],
          :genre/jazz [1.0 0.0 1.0],
          :genre/rock [0.0 1.0 0.0],
          :genre/rap [0.0 0.0 1.0]}
         (multihot-from-tags (example-cases :genres) "genre/"))
      "custom prefix"))


; (run-tests 'gmrs.preprocess-test)
