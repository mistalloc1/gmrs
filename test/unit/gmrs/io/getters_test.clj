(ns gmrs.io.getters-test
  (:require [clojure.test :refer :all]
            [gmrs.command :as cmd]
            [gmrs.io.getters :refer :all]
            [gmrs.wrangle :as wrangle]
            [gmrs.io.baseless :as bs]))

(deftest test-get-governor
  (let [GlobalIOSetup (bs/toy-temp-baseless-io-setup),
        GlobalIOSettings (bs/toy-temp-baseless-io-settings)]
    (run! (fn [send-fun]
            (send-fun GlobalIOSettings "test-guvna" {:hello "governor"}))
        (:govern-sends GlobalIOSetup))
    (is {:hello "governor"}
        (get-governor GlobalIOSettings GlobalIOSetup "test-guvna"))))

(def example-meta
  {:io-settings { :option-id :venue-name
                  :case-id :name
                  :inter-case :case-id
                  :inter-option :opt-id
                  :inter-id :inter-id }})

(def example-options
    [{:venue-name "Warsaw Jazz"
      :genres "jazz"
      :volume 40
      :city "Warsaw"}
     {:venue-name "Kraków Rock"
      :genres "goth|rock"
      :volume -696
      :city "Kraków"}])

(deftest test-integr-getter
  (binding [cmd/*GlobalIOSetup* (bs/toy-temp-baseless-io-setup),
            cmd/*GlobalIOSettings*
            (apply assoc
                   (bs/toy-temp-baseless-io-settings)
                   (reduce into []
                           (:io-settings example-meta)))]
    (cmd/send-options! example-options)
    (let [opts-getter (getter cmd/*GlobalIOSettings*
                              (:option-gives cmd/*GlobalIOSetup*))]
      (is (= (wrangle/records-as-cols example-options)
             (first opts-getter))
          "basic getter"))
    (let [opts-getter (getter cmd/*GlobalIOSettings*
                              (:option-gives cmd/*GlobalIOSetup*)
                              [:volume :city])]
      (is (= (select-keys (wrangle/records-as-cols example-options)
                          [:volume :city])
             (first opts-getter))
          "basic getter"))))

; (run-tests `gmrs.io.getters-test)
