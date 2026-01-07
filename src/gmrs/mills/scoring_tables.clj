(ns gmrs.mills.scoring-tables
  "Scoring tables are an auxillary form of potential recommendations for some
  mills, like nearest options. They map [case option] vectors to scores, with
  collections of cases and options indicated in the metadata."
  (:require [gmrs.wrangle :as wrangle]))

(defn average-score
  "Compute the average score, ignoring nils."
  [coll]
  (let [coll (filter number? coll)]
    (if (empty? coll) 0.0
      (/ (reduce + 0.0 coll) (count coll)))))

(defn sorted-rec-options
  "Given a scoring table, return a map of cases to vectors of maps { (options id)
  :score } sorted by :score descending."
  [scoring-table]
  ;; FIXME: the empty case
  (reduce (fn [accum case-id]
            (assoc accum case-id
                   (sort-by
                     :score >
                     (map (fn [opt-id]
                            { (-> (meta scoring-table) :io-settings :option-id)
                              opt-id,
                             :score (get scoring-table [case-id opt-id]) })
                          (get (meta scoring-table) :options)))))
          {}
          (get (meta scoring-table) :cases)))

(defn top-scorings
  "Only select the top scoring entries for each case from the scoring-table.
  Metadata will not be preserved."
  [scoring-table]
  (reduce-kv
    (fn [result case top]
      (assoc result [case (:opt top)] (:score top)))
    {}
    (reduce-kv
      (fn [top-scores [case opt] score]
        (if (> score (or (:score (get top-scores case)) -2))
          (assoc top-scores case { :opt opt :score score })
          top-scores))
      {}
      scoring-table)))

(defn options-to-cases-scoring-table
  "Given a scoring table made option-to-option, derive scores for the recommended
  options applicable when recommending them for the cases; do this by averaging
  the scores when 'recommending' for the options already associated with the case."
  [scoring-table cases-options]
  (assert (:options (meta scoring-table)))
  (with-meta
    (reduce
      into {}
      (map (fn [[case-id case-assoc-options]]
             (reduce
               into {}
               ;; For the scored "option-role" options, collect their average
               ;; scores for the options known to have interacted with the case.
               (map (fn [scr-opt-id]
                      { [case-id scr-opt-id]
                        (average-score
                          (map (fn [ass-opt-id]
                                 (if-let [scoring (get scoring-table
                                                       [ass-opt-id scr-opt-id])]
                                   scoring nil))
                               case-assoc-options)) })
                    (:options (meta scoring-table)))))
           cases-options))
    { :cases (keys cases-options) :options (:options (meta scoring-table))
      :io-settings (:io-settings (meta scoring-table)) }))

(defn sorted-with-culled-already-interacted
  "From a scoring table, get a map like from sorted-rec-options, but remove
  the options with which the cases have already interacted. The cases-inters arg
  maps case IDs to interaction records (not IDs)."
  [scoring-table cases-inters]
  (assert (or (empty? scoring-table) (:io-settings (meta scoring-table))))
  (let [opt-id-col (:option-id (:io-settings (meta scoring-table))),
        inter-opt-id (:inter-option (:io-settings (meta scoring-table)))]
    (reduce-kv
      (fn [sorted-recs case-id opt-entries]
        (let [this-case-interd-opts
              (if-let [inters (get cases-inters case-id)]
                (inter-opt-id (wrangle/records-as-cols inters))
                nil)]
          (assoc sorted-recs case-id
                 (filter (fn [opt-entry]
                           (not (some #(= % (opt-id-col opt-entry))
                                      this-case-interd-opts)))
                         opt-entries))))
      {}
      (sorted-rec-options scoring-table))))
