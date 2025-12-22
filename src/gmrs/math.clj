(ns gmrs.math
  (:require
    [clojure.math :as clj-math]
    [uncomplicate.commons.core :refer [with-release]]
    [uncomplicate.neanderthal.core :as nd]
    [uncomplicate.neanderthal.vect-math :as ndv]
    [uncomplicate.neanderthal.native :refer [dv]]))

(defn cosine-similarity
  [coll1 coll2]
  (let [a (dv coll1),
        b (dv coll2),
        dot-product (nd/dot a b)
        norm-a (nd/nrm2 a)
        norm-b (nd/nrm2 b)
        denom (* norm-a norm-b)]
    (/ dot-product denom)))

; TODO: rewrite to with-release
(defn desc-stats
  "Descriptive statistics map for x (collection)."
  [coll]
  ;; FIXME: one element is also problematic due to /0 with variance
  (when (empty? coll) (throw (Exception. "trying to get stats from empty coll")))
  (let [x (dv coll),
        n (nd/dim x),
        mu (/ (nd/sum x) n),
        subtract-mean (dv (vec (repeat n (- mu)))),
        sum-squares (nd/sum (ndv/sqr (nd/axpy subtract-mean x))),
        variance (/ sum-squares (dec n))]
    { :mean mu
      :variance variance
      :sd (Math/sqrt variance) }))

; TODO: rewrite to with-release
(defn pearson-correlation
  "Get Pearson correlation. Note this gives NaN on vectors with zeros."
  [coll1 coll2]
  (let [a (dv coll1),
        b (dv coll2),
        n (do (assert (= (nd/dim a) (nd/dim b)))
              (nd/dim a)),
        subtract-mean-a (dv (vec (repeat n (- (/ (nd/sum a) n))))),
        subtract-mean-b (dv (vec (repeat n (- (/ (nd/sum b) n))))),
        a-centered (nd/axpy subtract-mean-a a),
        b-centered (nd/axpy subtract-mean-b b),
        numer (nd/sum (ndv/mul a-centered b-centered)),
        denom (* (Math/sqrt (nd/sum (ndv/sqr a-centered)))
                 (Math/sqrt (nd/sum (ndv/sqr b-centered))))]
    (/ numer denom)))

(defn z-logistic-scale
  "Apply sigmoid(z-score), z-score = (x-mean) * (1/sd) on the data (x - coll).
  If sd is 0, map all values to zeros."
  [coll mean sd]
  (let [size (count coll)
        out (double-array size)]
    ; Compute if necessary (otherwise give zeros).
    (if (not (zero? sd))
      (with-release [inp (dv coll)
                     subtract-mean (dv (vec (repeat (count coll) (- mean))))]
        (nd/transfer!
          (ndv/sigmoid (nd/scal! (/ 1.0 sd)
                                  (nd/axpy! subtract-mean inp)))
          out))
      out)))

;;;
;;; Probability distributions.
;;;
;;; NOTE: as these are for individual columns, the current implementation always
;;; assumes 1D data.
;;;

(defprotocol ProbDist
  (sequence-log-likelihood [this xs]
                        "Give the log likelihood of xs given the distribution.")
  (point-likelihood [this x]
                    "Likelihood, or density function value, for the point X.")
  (params-count [this]
                "The number of parameters estimated for the model."))

(defrecord UnimodalGaussian
  [mean variance sd])

(def sqrt-two-pi (clj-math/sqrt (* 2 clj-math/PI)))

(defn gaussian-density-at-x [gaussian x]
  (/ (clj-math/exp (- (/ (clj-math/pow (/ (- x (:mean gaussian))
                                          (:sd gaussian))
                                       2)
                         2)))
     (* sqrt-two-pi (:sd gaussian))))

;; Just took these equations from:
;; https://www.geeksforgeeks.org/machine-learning/maximum-likelihood-estimation-of-gaussian-parameters/

(extend UnimodalGaussian
  ProbDist
  {:sequence-log-likelihood
   (fn [this xs]
     (let [n (count xs)]
       (- (- (* (/ n 2)
                (clj-math/log (* 2 clj-math/PI (:variance this)))))
          (* (/ 1 (* 2 (:variance this)))
             (reduce + (map #(clj-math/pow (- % (:mean this)) 2) xs)))))),
   :point-likelihood gaussian-density-at-x
   :params-count (fn [this] 2)})

(defn mle-unimodal-gaussian
  "Get a unimodal Gaussian distribution obtained by Maximum Likelihood Estimation
  from Xs."
  [xs]
  (let [empirical-stats (desc-stats xs)]
    (->UnimodalGaussian (:mean empirical-stats) (:variance empirical-stats)
                        (:sd empirical-stats))))

(defrecord GaussianMixture
  [k weights means variances sds])

;; TODO: memoize or somethingm (could also rewrite to compose unimodals in objs)
(defn gmm-individual-models
  "Decompose a Gaussian mixture into a UnimodalGaussian for each model."
  [gmm]
  (map
    #(->UnimodalGaussian (nth (:means gmm) %) (nth (:variances gmm) %)
                         (nth (:sds gmm) %))
    (range (:k gmm))))

(extend GaussianMixture
  ProbDist
  ;; TODO: we could find/use some simpler versions?
  ;; based on https://nic.schraudolph.org/teach/ml03/MLmix.pdf
  ;; and https://www.cs.toronto.edu/~jlucas/teaching/csc411/lectures/lec15_16_handout.pdf
  {:sequence-log-likelihood
   (fn [this xs]
     ;; prob-scalers: 1 / <sqrt<2*pi> * sigma_j> terms
     ;; diff-scalers: -1 / <2 * variance> terms, applied to <x-mu>^2
     (let [prob-scalers (map #(/ 1 (* sqrt-two-pi (nth (:sds this) %)))
                             (range (:k this))),
           diff-scalers (map #(/ 1 (* 2 (nth (:variances this) %)))
                             (range (:k this)))]
       (reduce +
               (map
                 (fn [x]
                   (clj-math/log
                     (reduce
                       +
                       (map (fn [k]
                              (* (nth (:weights this) k)
                                 (nth prob-scalers k)
                                 (clj-math/exp
                                   (- (* (nth diff-scalers k)
                                         (clj-math/pow
                                           (- x (nth (:means this) k))
                                           2))))))
                            (range (:k this))))))
                 xs))))
     :point-likelihood (fn [this x]
                        (let [member-models (gmm-individual-models this)]
                          (reduce +
                                  (map #(* (nth (:weights this) %)
                                           (gaussian-density-at-x
                                             (nth member-models %)
                                             x))
                                       (range (:k this))))))
     :params-count (fn [this] (+ (* 2 (:k this)) ; sigmas and means
                                 ;; the last weight is determined by subtracting
                                 ;; the rest from 1.0
                                 (dec (:k this))))})

(def EM-STOP-EPSILON 0.1)
(def EM-MAX-ITER 100)

;; the EM algo equations taken from https://stephens999.github.io/fiveMinuteStats/intro_to_em.html
(declare gmm-e-step)

(defn gmm-m-step
  "The M step of the EM algorithm for Maximum Likelihood Estimation for Gaussian
  Mixture Models. Return a function for trampoline, calling the E step with the
  re-estimated weights for the mixture models.

  As the result of the calculation, we get the weights per each data point
  for each member model, so a vector of vectors (each vector concerning one
  member model)."
  [mixture-model xs iter-n]
  (let [member-models (gmm-individual-models mixture-model),
        point-likelihoods (map (partial point-likelihood mixture-model)
                               xs),
        model-per-point-weights
        (map (fn [k]
              (map (fn [x full-likelihood]
                     (/ (* (nth (:weights mixture-model) k)
                                 (gaussian-density-at-x (nth member-models k)
                                                        x))
                              full-likelihood))
                   xs
                   point-likelihoods))
             (range (:k mixture-model)))]
    #(gmm-e-step mixture-model xs model-per-point-weights iter-n)))

(defn gmm-e-step
  [mixture-model xs model-per-point-weights iter-n]
  (let [point-weight-sums (map (fn [k]
                                 (reduce + (nth model-per-point-weights k)))
                               (range (:k mixture-model))),
        new-means (map (fn [k]
                         (/ (reduce + (map (fn [x loc-weight] (* x loc-weight))
                                           xs (nth model-per-point-weights k)))
                            (nth point-weight-sums k)))
                       (range (:k mixture-model))),
        new-variances (map (fn [k]
                             (/ (reduce
                                  + (map
                                      (fn [x loc-weight]
                                        (max
                                          ;; guard against 0.0 variance
                                          0.0001
                                          (* loc-weight
                                             (clj-math/pow (- x
                                                              (nth new-means
                                                                   k))
                                                           2))))
                                      xs (nth model-per-point-weights k)))
                                (nth point-weight-sums k)))
                           (range (:k mixture-model)))
        new-sds (map #(clj-math/sqrt %) new-variances),
        new-weights (map #(/ % (count xs)) point-weight-sums),
        new-mixture (->GaussianMixture (:k mixture-model)
                                       new-weights new-means new-variances
                                       new-sds)
        old-likelihood (sequence-log-likelihood mixture-model xs),
        new-likelihood (sequence-log-likelihood new-mixture xs)]
    (if (or (< (- new-likelihood old-likelihood) EM-STOP-EPSILON)
            (>= iter-n EM-MAX-ITER))
      new-mixture
      #(gmm-m-step new-mixture xs (inc iter-n)))))

(defn mle-gaussian-mixture
  [xs k]
  (let [empirical-stats (desc-stats xs),
        divided-sds (repeat k (/ (:sd empirical-stats) k))]
    (trampoline gmm-m-step
                (->GaussianMixture k
                                   (repeat k (/ 1 k))
                                   (map (fn [i]
                                          (+ (:mean empirical-stats)
                                             ;; this just-so function gives 0
                                             ;; at 3 and otherwise wobbles
                                             (* (if (even? i) 1.0 -1.0)
                                                (/ (- i 3) i)
                                                (:sd empirical-stats))))
                                        (range 1 (inc k)))
                                   (map #(clj-math/pow % 2) divided-sds)
                                   divided-sds)
                xs 1)))

(defn bayesian-inform-criterion
  "Bayesian Information Criterion, similar to Akaike Information Criterion, is
  a tool for selecting a better distribution for the data based on log likelihood
  and the degrees of freedom. The function returns the best distribution that
  was selected for xs, i.e., the data.

  The param-penalty is used to additionally favor simpler models."
  ([dists xs] (bayesian-inform-criterion dists xs 10))
  ([dists xs param-penalty]
   (first (sort-by (fn [dist] (- (* (params-count dist)
                                    param-penalty
                                    (clj-math/log (count xs)))
                                 (* 2 (sequence-log-likelihood dist xs))))
                   <
                   dists))))
