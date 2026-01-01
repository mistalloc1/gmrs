(ns gmrs.io.getters
  (:require [gmrs.wrangle :as wrangle]))

(defn get-governor [io-settings io-setup govern-name]
  (some identity
        (map (fn [give-fun] (give-fun io-settings govern-name))
             (:govern-gives io-setup))))

(defn pages
  "Lazy sequence of concatenated subsequent pages of each give, in columnar
  format. If cols-subset is truthy, it will be used to select only those
  columns."
  [io-settings gives cols-subset preproc-fun]
  (let [full-preproc (if cols-subset
                       (comp preproc-fun #(select-keys % cols-subset))
                       preproc-fun)]
    (lazy-seq (cons
                (with-meta
                  (full-preproc
                    (wrangle/records-as-cols (apply concat (map first gives))))
                  {:io-settings io-settings})
                ;; no need to re-build full-preproc again
                (pages io-settings (map rest gives) false full-preproc)))))

(defn getter
  "Get lazy seqs from each give, taking a page at a time."
  ([io-settings gives] (getter io-settings gives false {} identity))
  ([io-settings gives cols-subset] (getter io-settings gives cols-subset
                                           {} identity))
  ([io-settings gives cols-subset dataprefs]
    (getter io-settings gives cols-subset dataprefs identity))
  ([io-settings gives cols-subset dataprefs preproc-fun]
   (let [realized-gives (map #(apply % io-settings dataprefs)
                             gives)]
     (pages io-settings realized-gives cols-subset preproc-fun))))

;; Conceptually dataprefs come "before" preprocessing, but when calling mills
;; we want preprocessing set and then be able to modify dataprefs by extending
;; getter function partials.

(defn options-getter
  "Lazy sequence of option pages (combining a page from each give). It only
  includes columns set in :option-columns in IO settings."
  ([io-settings io-setup] (options-getter io-settings io-setup identity {}))
  ([io-settings io-setup preproc-fun]
   (options-getter io-settings io-setup preproc-fun {}))
  ([io-settings io-setup preproc-fun dataprefs]
   (getter io-settings (:option-gives io-setup) (:option-columns io-settings)
           dataprefs preproc-fun)))

(defn cases-getter
  "Lazy sequence of case pages (combining a page from each give).It only
  includes columns set in :case-columns in IO settings."
  ([io-settings io-setup] (cases-getter io-settings io-setup identity {}))
  ([io-settings io-setup preproc-fun]
   (cases-getter io-settings io-setup preproc-fun {}))
  ([io-settings io-setup preproc-fun dataprefs]
   (getter io-settings (:case-gives io-setup) (:case-columns io-settings)
           dataprefs preproc-fun)))

(defn inters-getter
  "Lazy sequence of interaction pages (combining a page from each give)."
  ([io-settings io-setup] (inters-getter io-settings io-setup identity {}))
  ([io-settings io-setup preproc-fun]
   (inters-getter io-settings io-setup preproc-fun {}))
  ([io-settings io-setup preproc-fun dataprefs]
   (getter io-settings (:inter-gives io-setup) nil dataprefs preproc-fun)))

(defn decs-getter
  "Lazy sequence of decision pages (combining a page from each give)."
  ([io-settings io-setup] (decs-getter io-settings io-setup identity {}))
  ([io-settings io-setup preproc-fun]
   (decs-getter io-settings io-setup preproc-fun {}))
  ([io-settings io-setup preproc-fun dataprefs]
   (getter io-settings (:dec-gives io-setup) nil dataprefs preproc-fun)))
