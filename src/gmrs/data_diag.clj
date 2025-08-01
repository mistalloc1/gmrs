(ns gmrs.data-diag
  (:require [clojure.string :as str]))

(defn init-or-inc-if-pos [v] (cond (nil? v) 1
                                   (pos? v) (inc v)
                                   :else v))

(defn bump-for
  "Get a map updating keys in attrib-map with init-or-inc-if-pos."
  [ks attrib-map]
  (into {}
        (map (fn [k] { k (init-or-inc-if-pos (get attrib-map k)) })
             ks)))

(defn enough? [num-score full-series-size]
  (> (* num-score 2) full-series-size))

(defn tags-map-usable? [tags-map full-series-size]
  (< (* 3 (count tags-map))
     (* 2 full-series-size)))

(defn heuristic-is-datetime? [value]
  (let [dig-groups (re-seq #"\d+" value)]
    (and (> (count dig-groups) 0)
         (<= (count dig-groups) 8)
         (every? #(< (count %) 5) dig-groups))))

;; TODO: for now only implementing the formats we'll use in testing
(defn diag-potential-datetime
  "Give an update for attrib-map with datetime related diagnostics of the string
  value."
  [value attrib-map]
  (if (heuristic-is-datetime? value)
    (let [value (str/trim value)]
    (merge (try (java.time.LocalDate/parse value)
                (bump-for [:date-local :needs-conv] attrib-map)
                (catch java.time.format.DateTimeParseException _ {}))
           (try (java.time.LocalTime/parse value)
                (bump-for [:time-local :needs-conv] attrib-map)
                (catch java.time.format.DateTimeParseException _ {}))
           (try (java.time.ZonedDateTime/parse value)
                (bump-for [:date-zoned-with-time :needs-conv] attrib-map)
                (catch java.time.format.DateTimeParseException _ {}))))))

(defn is-numtype? [str-value nonnum-chars parse-fun]
  (and (every? #(or (Character/isDigit %)
                    (nonnum-chars %))
               str-value)
       ;; TODO: does trying the parse make sense?
       (try (parse-fun str-value)
            (catch NumberFormatException _ false))))

(defn diag-potential-tags
  "Update or create the attrib-map part which says if tags can be extracted from
  the data series containing the value."
  [value attrib-map full-series-size]
  (let [updated-tags
        (merge (:maybe-tags-map attrib-map)
               (bump-for (str/split value #"\|")
                         (:maybe-tags-map attrib-map)))]
    (if (tags-map-usable? updated-tags full-series-size)
      { :tags (init-or-inc-if-pos (:tags attrib-map)),
        :maybe-tags-map updated-tags }
      ;; mark as unusable
      { :tags -1 })))

;; TODO: handle bigger numeric types, and locales
(defn diag-string-and-update
  "Give updated attrib-map with all string related diagnostics of the value."
  [value attrib-map full-series-size]
  (merge (update
           attrib-map
           :str init-or-inc-if-pos)
         (if (< (count value) 32)
           (reduce into {}
                   [(diag-potential-datetime value attrib-map)
                    (when (is-numtype? value #{\- \+ \space \tab}
                                       Integer/parseInt)
                      (bump-for [:integer :needs-conv] attrib-map))
                    (when (is-numtype? value #{\- \+ \e \. \space \tab}
                                       Float/parseFloat)
                      (bump-for [:float :needs-conv] attrib-map))]))
         (if (and (< (count value) 2048)
                  (not (and (number? (:tags attrib-map))
                            (neg? (:tags attrib-map)))))
           (diag-potential-tags value attrib-map full-series-size))))

(defn diag-all-values
  "Create or update (in the subsequent calls) the attrib-map containing the
  column attributes. The function recurs itself until the coll of values is
  exhausted, then just returns a coll of column attributes decided for the whole
  data series.

  The attributes end up being added if the diag-info associated with them is
  high enough. If it is negative at any point, this means we give up on it."
  ([coll] (diag-all-values coll {} (count coll)))
  ([coll attrib-map full-size]
   (if (empty? coll)
     ;; FIXME: handle the special :needs-conv case which could come from multiple
     ;; underlying "types"
     ;; FIXME: prefer integers to floats which also capture them in diag
     (set
       (filter keyword?
               (map (fn [[attr diag-info]] (if (and (number? diag-info)
                                                    (enough? diag-info full-size))
                                             attr))
                    attrib-map)))
     (recur
       (rest coll)
       (condp apply [(first coll)]
         float? (update attrib-map :float init-or-inc-if-pos)
         integer? (update attrib-map :int init-or-inc-if-pos)
         string? (diag-string-and-update (first coll) attrib-map full-size))
       full-size))))
