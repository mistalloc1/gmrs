(ns gmrs.math-test
  (:require [clojure.test :refer :all]
            [clojure.math :as clj-math]
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

(deftest test-gaussian-density-at-x
  (let [standard-dist (->UnimodalGaussian 0.0 1.0 1.0)]
    (is (close? 0.001 0.352 (gaussian-density-at-x standard-dist 0.5)))
    (is (close? 0.001 0.352 (gaussian-density-at-x standard-dist -0.5))
        "negative should be the same"))
  (is (close? 0.001 0.193 (gaussian-density-at-x (->UnimodalGaussian 0.0 4.0 2.0)
                                                0.5))
      "changed variance")
  (is (close? 0.001 0.352 (gaussian-density-at-x (->UnimodalGaussian 4.0 1.0 1.0)
                                                4.5))
      "changed mean"))

;; Check that gaussian-density-at-x connects.
(deftest test-point-likelihood-unimodal
  (is (close? 0.01 0.352 (point-likelihood (->UnimodalGaussian 4.0 1.0 1.0)
                                           4.5))))

(deftest test-mle-unimodal-gaussian
  (let [fit-dist (mle-unimodal-gaussian [42.0 43.0 40.0 44.0 41.0])]
    (is (= (:mean fit-dist) 42.0))
    (is (= (:variance fit-dist) 2.5))
    (is (close? 0.01 (:sd fit-dist) 1.58))))

(deftest test-sequence-log-likelihood-unimodal
  (let [sample [2.1, 2.5, 1.9, 3.0, 2.3],
        fit-dist (mle-unimodal-gaussian sample)]
    (is (close? 0.001 2.36 (:mean fit-dist)) "sanity check of the fit dist")
    (is (close? 0.001 -2.28 (sequence-log-likelihood fit-dist sample)))))

(deftest test-point-likelihood-mixture
  (let [example-dist (->GaussianMixture
                       3
                       [0.2 0.6 0.2]
                       [0.0 100.0 -25.0]
                       [9.0 1.0 25.0]
                       [3.0 1.0 5.0])]
    (is (close? 0.001 (* 0.352 0.6)
                (point-likelihood example-dist 100.5))
        "mainly the scaled contribution of 2nd dist")
    (is (close? 0.001 (* 0.0548 0.2)
                (point-likelihood example-dist 4.0))
        "mainly from the 1st dist")
    (is (close? 0.001 0.0 (point-likelihood example-dist 54.0))
        "far from everything")
    (is (> (point-likelihood example-dist -28.0)
           (point-likelihood example-dist 54.0))
        "the third spread out dist has some influence")))

(deftest test-sequence-log-likelihood-mixture
  (let [example-dist (->GaussianMixture
                       3
                       [0.2 0.6 0.2]
                       [0.0 100.0 -25.0]
                       [9.0 1.0 25.0]
                       [3.0 1.0 5.0])]
    (is (close? 0.01 (+ -1.55 -4.5135 -4.32)
                (sequence-log-likelihood example-dist [4.0 -28.0 100.5]))
        "examples from test-point-likelihood-mixture summed")))

(deftest test-params-count-mixture
  (is (= 8 (params-count (->GaussianMixture
                           3
                           [0.2 0.6 0.2]
                           [0.0 100.0 -25.0]
                           [9.0 1.0 25.0]
                           [3.0 1.0 5.0])))))

(deftest test-mle-gaussian-mixture
  (testing "multimodal case"
    (let [fit-dist (mle-gaussian-mixture [-5.0 100.0 1.0 3.0 99.0 102.5 98.5
                                          101.0]
                                         2),
        lower-subdist-k (if (> (first (:means fit-dist))
                               (second (:means fit-dist)))
                          1 0),
        higher-subdist-k (if (zero? lower-subdist-k) 1 0)]
    (is (< 0.15 (- (nth (:weights fit-dist) higher-subdist-k)
                   (nth (:weights fit-dist) lower-subdist-k)))
        "the higher member distribution has more similar points so higher weight")
    (is (close? 2.0 0.0 (nth (:means fit-dist) lower-subdist-k)))
    (is (close? 2.0 101.0 (nth (:means fit-dist) higher-subdist-k)))
    (is (< (nth (:variances fit-dist) higher-subdist-k)
           (nth (:variances fit-dist) lower-subdist-k)))
    (is (> 4.0
           (nth (:variances fit-dist) higher-subdist-k)
           0.0))
    (is (close? 0.0001 (clj-math/pow (nth (:sds fit-dist) lower-subdist-k) 2)
                (nth (:variances fit-dist) lower-subdist-k)))))
  (testing "trying to fit multimodal on actually unimodal sample"
    (let [fit-dist (mle-gaussian-mixture [72.0 84.0 45.0 88.0 99.0 84.5 89.0]
                                         3)]
      (is (< 0.5 (apply max (:weights fit-dist))))
      (is (> 0.001 (apply min (:variances fit-dist)))
          "one of the models is actually dead with overfit mean"))))

(deftest test-integr-bayesian-inform-criterion
  (let [fit-dist-uni (mle-unimodal-gaussian [50.0 49.99 44.0 55.0]),
        fit-dist-bi (mle-gaussian-mixture [9.0 12.0 15.0 115.4 114.0] 2)]
    (is (= fit-dist-uni (bayesian-inform-criterion
                          [fit-dist-uni fit-dist-bi]
                          [43.0 52.0 51.4])))
    (is (= fit-dist-bi (bayesian-inform-criterion
                          [fit-dist-uni fit-dist-bi]
                          [103.0 10.0 7.0 113.0 17.0])))))

; (run-tests 'gmrs.math-test)
