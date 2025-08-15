(ns gmrs.math
  (:require
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
