(ns gmrs.io.baseless)

; NOTE: option-id cannot be :score
(defn toy-temp-baseless-io-settings []
  { :option-id :iid, :case-id :uid, :govern-id :gid,
   :dec-option :optid, :dec-case :caseid, :dec-agree :agree,
   :inter-option :optid, :inter-case :caseid
   :page-size 32 })

; TODO: later move this to io-setup source file so it's more general
; NOTE: these are atoms and not refs intentionally, we never want to assume
; the writes to those can be coordinated.
; NOTE: The data stream stores are as hash maps to get the id update/replacement
; behavior.
; NOTE: we expect data to be saved and retrieved in the columnar format.
(defn toy-temp-baseless-io-setup []
  (let [option-store (atom {}),
        case-store (atom {}),
        dec-store (atom {}),
        inter-store (atom {}),
        govern-store (atom {})]
    { :option-gives [(fn [settings & ignored-args]
                       (cycle
                         (partition-all (settings :page-size)
                                        (vals @option-store))))]
      :option-sends [(fn [settings new-options]
                       (swap! option-store into
                              (map (fn [item]
                                     [((settings :option-id) item) item])
                                   new-options)))]
      :case-gives [(fn [settings & ignored-args]
                     (cycle
                       (partition-all (settings :page-size)
                                      (vals @case-store))))]
      :case-sends [(fn [settings new-cases]
                     (swap! case-store into
                            (map (fn [item]
                                   [((settings :case-id) item) item])
                                 new-cases)))]
      ; FIXME: decs, inters storage model
      :dec-gives [(fn [settings & ignored-args] (vals @dec-store))]
      :dec-sends [(fn [settings new-decs] (swap! dec-store into new-decs))]
      :inter-gives [(fn [settings & ignored-args] (vals @inter-store))]
      :inter-sends [(fn [settings new-inters] (swap! inter-store into new-inters))]
      :govern-gives [(fn [settings govern-name] (@govern-store govern-name))]
      :govern-sends [(fn [settings govern-name new-govern]
                       (swap! govern-store assoc govern-name new-govern))] }))
