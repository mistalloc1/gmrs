(ns gmrs.io.dataprefs-test
  (:require [clojure.test :refer :all]
            [gmrs.io.dataprefs :refer :all]
            [tick.core :as t]))

(deftest test-one-pref-interp
  (is (one-pref-interp '(:non-nil) 5)))
  (is (not (one-pref-interp '(:non-nil) nil)))
  (is (one-pref-interp `(:= 8) 8))
  (is (not (one-pref-interp `(:= 8) 7)))
  (is (one-pref-interp `(:before ~(t/new-date 2024 02 12))
                              (t/new-date 2024 02 8)))
  (is (not (one-pref-interp `(:before ~(t/new-date 2024 02 12))
                              (t/new-date 2024 02 15))))
  (is (one-pref-interp `(:after ~(t/new-date 2024 02 12))
                          (t/new-date 2024 02 15)))
  (is (not (one-pref-interp `(:after ~(t/new-date 2024 02 12))
                          (t/new-date 2024 02 9))))

(deftest test-prefs-check-some
  (is (prefs-check-some [`(:= 9) `(:= 10)] 9))
  (is (not (prefs-check-some [`(:= 9) `(:= 10)] 8))))

(deftest test-prefs-interp
  (let [pref-fun-1-cond (prefs-interp
                          [[`(:before ~(t/new-date 2024 2 12))
                            `(:after ~(t/new-date 2025 2 12))]]),
        pref-fun-2-conds-a (prefs-interp
                             [[`(:before ~(t/new-date 2024 2 12))]
                              [`(:after ~(t/new-date 2025 2 12))]]),
        pref-fun-2-conds-b (prefs-interp [[`(:before ~(t/new-date 2024 2 12))
                                           `(:after ~(t/new-date 2025 2 12))]
                                          [`(:before ~(t/new-date 2026 1 2))]])]
    (is (pref-fun-1-cond (t/new-date 2023 10 8)))
    (is (not (pref-fun-2-conds-a (t/new-date 2023 10 8)))
        "cannot be true")
    (is (pref-fun-1-cond (t/new-date 2026 10 8)))
    (is (not (pref-fun-2-conds-b (t/new-date 2026 10 8)))
        "satisfies the first list of conditions but not the second")
    (is (pref-fun-2-conds-b (t/new-date 2026 1 1)))))

(deftest test-filter-with-col-prefs
  (let [example-data [{:date (t/new-date 2023 2 1) :topic nil},
                      {:date (t/new-date 2023 2 1) :topic "hamsters"},
                      {:date (t/new-date 2022 2 1) :topic "mice"}]]
  (is (= [{:date (t/new-date 2023 2 1) :topic "hamsters"}]
         (filter-with-col-prefs
           {}
           {:ref-date [[`(:after ~(t/new-date 2023 1 1))]],
            :ref-topic [['(:non-nil)]]}
           example-data))
      "simple case with literal column references")
  (is (= [{:date (t/new-date 2023 2 1) :topic "hamsters"}]
         (filter-with-col-prefs
           {:case-id :topic}
           {:ref-date [[`(:after ~(t/new-date 2023 1 1))]],
            :case-id [['(:non-nil)]]}
           example-data))
      "referencing columns though their IO settings meaning")))

; (run-tests 'gmrs.io.dataprefs-test)
