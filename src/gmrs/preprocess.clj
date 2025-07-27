"Operate on semantic data such as data columns to prepare for mills. The
meaning-agnostic things about reformatting etc. should go into wrangle."

(ns gmrs.preprocess
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [gmrs.math :as math]))

; TODO: currently none of this handles nulls

(defn z-logistic-scale
  "Fit a numerical seq encoding. It first applies a Z-score scaling (by standard
  deviation around the mean as 0), then calls a sigmoid (logistic function). The
  result is between 0 and 1."
  [coll]
  (math/desc-stats coll))

(defn apply-z-logistic-scale
  "Apply a numerical seq encoding obtained from z-logistic-scale."
  [coll transf]
  (assert (:sd transf))
  (math/z-logistic-scale coll (:mean transf) (:sd transf)))

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

