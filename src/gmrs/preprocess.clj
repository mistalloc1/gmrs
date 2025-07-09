"Operate on semantic data such as data columns to prepare for mills. The
meaning-agnostic things about reformatting etc. should go into wrangle."

(ns gmrs.preprocess
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [uncomplicate.commons.core :refer [with-release]]
    [uncomplicate.neanderthal.core :as nd]
    [uncomplicate.neanderthal.vect-math :as ndvm]
    [uncomplicate.neanderthal.native :refer [dv]]
    [fastmath.stats :as stats]))

; TODO: currently none of this handles nulls

(defn z-logistic-scale
  "Fit a numerical seq encoding. It first applies a Z-score scaling (by standard
  deviation around the mean as 0), then calls a sigmoid (logistic function). The
  result is between 0 and 1."
  [coll]
  (let [stats-desc (stats/stats-map coll)]
    {:mean (:Mean stats-desc) :sd (:SD stats-desc)}))

(defn apply-z-logistic-scale
  "Apply a numerical seq encoding obtained from z-logistic-scale."
  [coll transf]
  (assert (:sd transf))
  (let [size (count coll)
        out (double-array size)]
    ; Compute if necessary (otherwise give zeros).
    (if (not (zero? (:sd transf)))
      (with-release [inp (dv coll)
                     subtract-mean (dv (vec
                                         (repeat (count coll)
                                                 (- (:mean transf)))))]
        (nd/transfer! ; sigmoid(z-score = (x-mean) * (1/sd))
          (ndvm/sigmoid (nd/scal! (/ 1.0 (:sd transf))
                                  (nd/axpy! subtract-mean inp)))
          out))
      out)))

; TODO: profile against a cleaner impl (this is the oldest code in the project)
(defn multihot-from-tags
  "Given a column of tags separated by pipes, return a mapping of columns to
  vectors to 0s and 1s. The col names are prefix+$tag."
  ([tags-column] (multihot-from-tags tags-column "tag-"))
  ([tags-column prefix]
   (let [tag->cols (atom {})
         zeros (vec (repeat (count tags-column) 0.0))]
     (dorun (map-indexed
              (fn [row-idx row-val]
                (run! 
                  (fn [tag-col-name]
                    (if (not (@tag->cols tag-col-name))
                      (swap! tag->cols assoc tag-col-name zeros))
                    (swap! tag->cols
                           update-in [tag-col-name]
                           #(assoc % row-idx 1.0)))
                  (set (map #(keyword (str prefix %)) (str/split row-val #"\|")))))
              tags-column))
     @tag->cols)))

