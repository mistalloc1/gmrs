(ns gmrs.io.getters-test
  (:require [clojure.test :refer :all]
            [gmrs.io.getters :refer :all]
            [gmrs.io.baseless :as bs]))

(deftest test-get-governor
  (let [GlobalIOSetup (bs/toy-temp-baseless-io-setup),
        GlobalIOSettings (bs/toy-temp-baseless-io-settings)]
    (run! (fn [send-fun]
            (send-fun GlobalIOSettings "test-guvna" {:hello "governor"}))
        (:govern-sends GlobalIOSetup))
    (is {:hello "governor"}
        (get-governor GlobalIOSettings GlobalIOSetup "test-guvna"))))

; (run-tests `gmrs.io.getters-test)
