(ns gmrs.io.dataprefs
  "Implement dataprefs - i.e. desired filters we want to use when retrieving
  data. These functions work on data in memory as Clojure objects."
  (:require [clojure.string :as str]
            [tick.core :as t]))

(defn one-pref-interp
  [pref elem]
  (condp = (first pref)
    :non-nil (some? elem),
    := (= elem (second pref)),
    :< (< elem (second pref)),
    :> (> elem (second pref)),
    :before (t/< elem (second pref)),
    :after (t/> elem (second pref)),
    (throw (ex-info "Cannot interpret datapref predicate"
                    { :pred (first pref) }))))

(defn prefs-check-some
  "Use the one flat list of prefs to form an OR form from the predicates."
  [prefs-list elem]
  (some true? (map (fn [pref] (one-pref-interp pref elem)) prefs-list)))

(defn prefs-interp
  "Create a function applying the prefs to an element argument, which may be
  a part of a collection. Subsequent prefs are treated as predicates as
  conjoined by AND, and the statements inside a pref as treated as alternatives
  (as if connected by OR).

  Example pref: [[[:> 5]] [[:< 8]]] means that the element must be both greater
  than 5 and lower than 8. [[[:> 5] [:< 8]]] would mean that either of those
  must be true.

  It's recommended to use syntax quoting (with `) when constructing prefs."
  [prefs]
  (fn [elem] (every? true?
                     (map (fn [prefs-list] (prefs-check-some prefs-list elem))
                          prefs))))

(defn filter-with-col-prefs
  "Use col-prefs to filter the records. Col-prefs should be a map from column
  specs to prefs to be interpreted with prefs-interp. Records must be a
  collection of maps with keywords as keys.

  Column specs should be keywords. If they begin with :ref-, they are interpreted
  literally with the stuff after the :ref-, otherwise they are used as keys
  to the io-settings map."
  [io-settings col-prefs records]
  (filter some?
          (apply map
                 (fn [record & check-results]
                   (when (every? true? check-results) record))
                 records
                 ;; Collect maps from prefs for the individual columns.
                 (map (fn [[col-spec prefs]]
                        (let [col-spec-str (name col-spec),
                              col-name (if (str/starts-with? col-spec-str "ref-")
                                         (keyword (subs col-spec-str 4))
                                         (get io-settings col-spec))]
                          (when (and (seq records)
                                     (not (contains? (first records) col-name)))
                           (throw (ex-info
                                    (str "Cannot find field '" (pr-str col-name)
                                         " from " col-spec-str "' as col spec")
                                    { :col-spec-str col-spec-str
                                      :record (first records) })))
                          (map (prefs-interp prefs)
                               (map col-name records))))
                      col-prefs))))
