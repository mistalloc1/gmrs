(ns gmrs.io.baseless
  (:require [gmrs.io.dataprefs :as prefs]))

(defn memory-give
  "Create an in-memory ('baseless') give function retrieving items from
  store-atom. If possible, the data will be retrieved that satisfies the prefs
  (as interpreted by filter-with-prefs and prefs-interp)."
  [store-atom]
  (fn [io-settings & prefs]
    (cycle
      ;; TODO: what happens to partition-all if the atom value changes?
      (partition-all (io-settings :page-size)
                     (or (seq (prefs/filter-with-col-prefs io-settings prefs
                                                           (vals @store-atom)))
                         (seq (vals @store-atom)))))))

; NOTE: option-id cannot be :score
(defn toy-temp-baseless-io-settings []
  { :option-id :iid, :case-id :uid, :govern-id :gid,
    :inter-id :intid,
    :inter-option :optid, :inter-case :caseid, :inter-timestamp :time,
    :inter-rating :rating, :inter-dec-id :decid,
    :dec-id :decid,
    :dec-options :optids, :dec-case :caseid,
    :page-size 32 })

; TODO: later move this to io-setup source file so it's more general
; NOTE: these are atoms and not refs intentionally, we never want to assume
; the writes to those can be coordinated.
; NOTE: The data stream stores are as hash maps to get the id update/replacement
; behavior. But note this should not be expected by the code, see the
;; docs/data-storage-model.md.
; NOTE: we expect data to be saved and retrieved in the columnar format.
(defn toy-temp-baseless-io-setup []
  (let [option-store (atom {}),
        case-store (atom {}),
        inter-store (atom {}),
        dec-store (atom {}),
        govern-store (atom {})]
    { :option-gives [(memory-give option-store)]
      :option-sends [(fn [settings new-options]
                       (swap! option-store into
                              (map (fn [item]
                                     [((settings :option-id) item) item])
                                   new-options)))]
      :case-gives [(memory-give case-store)]
      :case-sends [(fn [settings new-cases]
                     (swap! case-store into
                            (map (fn [item]
                                   [((settings :case-id) item) item])
                                 new-cases)))]
      :inter-gives [(memory-give inter-store)]
      :inter-sends [(fn [settings new-inters] (swap! inter-store into
                            (map (fn [item]
                                   [((settings :inter-id) item) item])
                                 new-inters)))]
      :dec-gives [(memory-give dec-store)]
      :dec-sends [(fn [settings new-decs] (swap! dec-store into
                            (map (fn [item]
                                   [((settings :dec-id) item) item])
                                 new-decs)))]
      :govern-gives [(fn [settings govern-name] (@govern-store govern-name))]
      :govern-sends [(fn [settings govern-name new-govern]
                       (swap! govern-store assoc govern-name new-govern))] }))
