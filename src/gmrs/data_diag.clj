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

(defn tags-map-usable?
  "In order for the variable to be usable as tags, the number of unique values
  needs to be less than 2/3 of the whole sample series size."
  [tags-map full-series-size]
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
  (when (heuristic-is-datetime? value)
    (let [value (str/trim value)]
    (merge (try (java.time.LocalDate/parse value)
                (bump-for [:date-local :date-local-needs-conv] attrib-map)
                (catch java.time.format.DateTimeParseException _ {}))
           (try (java.time.LocalTime/parse value)
                (bump-for [:time-local :time-local-needs-conv] attrib-map)
                (catch java.time.format.DateTimeParseException _ {}))
           (try (java.time.ZonedDateTime/parse value)
                (bump-for [:date-zoned-with-time
                           :date-zoned-with-time-needs-conv]
                          attrib-map)
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
         (when (< (count value) 32)
           (reduce into {}
                   [(diag-potential-datetime value attrib-map)
                    (when (is-numtype? value #{\- \+ \space \tab}
                                       ;; int literal specs don't allow whitespace
                                       (comp Integer/parseInt str/trim))
                      (bump-for [:integer :integer-needs-conv] attrib-map))
                    (when (is-numtype? value #{\- \+ \e \. \space \tab}
                                       Float/parseFloat)
                      (bump-for [:float :float-needs-conv] attrib-map))]))
         (when (and (< (count value) 2048)
                    (not (and (number? (:tags attrib-map))
                              (neg? (:tags attrib-map)))))
           (diag-potential-tags value attrib-map full-series-size))))

(defn prefer-keyword
  [s prefer-keyword other-keyword]
  (if (and (contains? s prefer-keyword) (contains? s other-keyword))
    (disj s other-keyword)
    s))

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
     ;; The coll has been exhausted, decide on the attributes to leave.
     (reduce
       (fn [attrs pref-pair]
         (apply prefer-keyword attrs pref-pair))
       (set
         (filter keyword?
                 (map (fn [[attr diag-info]]
                        (when (and (number? diag-info)
                                   (enough? diag-info full-size))
                          attr))
                      attrib-map)))
       [[:integer :float] [:integer-needs-conv :float-needs-conv]
        ;; NOTE: these are more risky - when less options are observed, like the
        ;; number of episodes, 26, 52...
        [:tags :integer] [:tags :integer-needs-conv]
        [:tags :float] [:tags :float-needs-conv]])
     ;; Work on the remaining part of coll.
     (recur
       (rest coll)
       (condp apply [(first coll)]
         float? (update attrib-map :float init-or-inc-if-pos)
         integer? (update attrib-map :int init-or-inc-if-pos)
         string? (diag-string-and-update (first coll) attrib-map full-size))
       full-size))))
