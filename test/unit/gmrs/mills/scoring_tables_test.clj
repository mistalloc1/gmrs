(ns unit.gmrs.mills.scoring-tables-test
  (:require [clojure.test :refer :all]
            [gmrs.mills.scoring-tables :refer :all]))

(deftest test-sorted-rec-options
  (is (= { :c1 [{ :opt-id :o2 :score 0.4 }
                { :opt-id :o1 :score 0.3 }]
           :c2 [{ :opt-id :o1 :score 0.8 }
                { :opt-id :o2 :score 0.2 }] }
         (sorted-rec-options
           (with-meta { [:c1 :o1] 0.3, [:c1 :o2] 0.4,
                       [:c2 :o1] 0.8, [:c2 :o2] 0.2 }
                      { :cases [:c1 :c2] :options [:o1 :o2]
                       :io-settings { :option-id :opt-id } }))))
  (is (= { :c1 []
           :c2 [] }
         (sorted-rec-options
           (with-meta {  }
                      { :cases [:c1 :c2]
                       :io-settings { :option-id :opt-id } })))
      "empty case"))

(deftest test-average-score
  (is (= 0.4 (average-score [1.0 0.1 0.2 0.5 0.2]))))

(deftest test-top-scorings
  (is
    (= { [:case-1 :opt-2] 0.7, [:case-2 :opt-2] 0.7, [:case-3 :opt-3] 0.8 }
       (top-scorings
         { [:case-1 :opt-2] 0.7, [:case-1 :opt-3] 0.3,
           [:case-2 :opt-2] 0.7, [:case-2 :opt-3] 0.5,
           [:case-3 :opt-2] 0.2, [:case-3 :opt-3] 0.8 }))))

(deftest test-options-to-cases-scoring-table
  (let [options-scoring-table
        (with-meta
          { [:opt-1 :opt-2] 0.7, [:opt-1 :opt-3] 0.3, [:opt-4 :opt-2] 0.7,
            [:opt-4 :opt-3] 0.7 [:opt-2 :opt-2] 1.0 [:opt-2 :opt-3] 0.0 }
          { :cases [:opt-1 :opt-4 :opt-2]
            :options [:opt-2 :opt-3] }),
        cases-to-options { :case-1 [:opt-1] :case-2 [:opt-1 :opt-4] }]
    (is (= { [:case-1 :opt-2] 0.7, [:case-1 :opt-3] 0.3,
             [:case-2 :opt-2] 0.7, [:case-2 :opt-3] 0.5 }
           (options-to-cases-scoring-table options-scoring-table
                                           cases-to-options)))))

(deftest test-sorted-with-culled-already-interacted
  (is (= { :case-1 [] :case-2 [{ :opt-id :opt-2 :score 0.7 }] }
         (sorted-with-culled-already-interacted
           (with-meta
             { [:case-1 :opt-2] 0.7, [:case-1 :opt-3] 0.3,
               [:case-2 :opt-2] 0.7, [:case-2 :opt-3] 0.5 }
             { :cases [:case-1 :case-2]
               :options [:opt-2 :opt-3]
               :io-settings
               { :option-id :opt-id :inter-case :cs :inter-option :op
                 :inter-id :id } })
           ;; Case 1 interacts with everything (so no recs), case 2 only with
           ;; opt-3.
           { :case-1 (map (fn [o iid] {:cs :case-1 :op o :id iid })
                          [:opt-1 :opt-2 :opt-3] ["i1-1" "i1-2" "i1-3"])
             :case-2 (map (fn [o iid] {:cs :case-2 :op o :id iid })
                          [:opt-3] ["i2-1"]) }))
      "simple case"))
