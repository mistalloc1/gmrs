(ns gmrs.io.baseless)

; TODO: later move this to io-setup source file so it's more general; make it a
; record type
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
    ; NOTE: option-id cannot be "score"
    { :settings { :option-id "iid" :case-id "uid" :govern-id "gid"
                  :dec-option "optid" :dec-case "caseid" :dec-agree "agree"
                  :inter-option "optid" :inter-case "caseid" }
      :option-gives [(fn [& ignored-args] (vals @option-store))]
      :option-sends [(fn [new-options] (swap! option-store into))]
      :case-gives [(fn [& ignored-args] (vals @case-store))]
      :case-sends [(fn [new-cases] (swap! case-store into))]
      :dec-gives [(fn [& ignored-args] (vals @dec-store))]
      :dec-sends [(fn [new-decs] (swap! dec-store into))]
      :inter-gives [(fn [& ignored-args] (vals @inter-store))]
      :inter-sends [(fn [new-inters] (swap! inter-store into))]
      :govern-gives [(fn [govern-name] (@govern-store govern-name))]
      :govern-sends [(fn [govern-name new-govern]
                       (swap! govern-store assoc govern-name new-govern))] }))
