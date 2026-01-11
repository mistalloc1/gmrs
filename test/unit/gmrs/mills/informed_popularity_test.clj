(ns gmrs.mills.informed-popularity-test
  (:require [clojure.test :refer :all]
            [gmrs.mills.informed-popularity :refer :all]
            [gmrs.test-commons :refer :all]
            [gmrs.wrangle :as wrangle]))

(deftest test-random-option-mill
  (let [cases (wrangle/cols-from-row-mask
                hotel-cases
                ;; Select John Smith, Marie Leroy and Anna Lindqvist
                [true false false true false true]),
        recs (random-option-mill
               cases #(make-getter cases) #(make-getter hotel-options)
               #(map wrangle/records-as-cols
                    (partition-all 2 (wrangle/cols-as-rows hotel-inters)))
               #(make-getter {})
               mock-pull-strat)]
    (is (= 3 (count recs)) "only recommend for requested cases")
    (is (= 3 (count (take 3 (get recs "John Smith"))))
        "case 1, John Smith gets recommendations")
    (is (< 0 (:score (first (get recs "John Smith"))))
        "recommendations with score")
    (is ((set (:name hotel-options))
         (:name (first (get recs "John Smith"))))
        "recommendations get actual option IDs")
    (is (= 3 (count (take 3 (get recs "Anna Lindqvist"))))
        "case 2, Anna Lindqvist gets recommendations")
    (is (= 3 (count (take 3 (get recs "Marie Leroy"))))
        "case 3, Marie Leroy gets recommendations")))

(def plantportal-meta
  { :io-settings { :case-id :ip, :option-id :page,
                   :inter-id :inter-id, :inter-case :ip, :inter-option :page,
                   :inter-dec-id :gmrs-decision,
                   :dec-id :dec-id, :dec-case :ip, :dec-options :pages } })

(def plantportal-decs
  (with-meta
    (wrangle/records-as-cols
      [{:dec-id 2401 :ip "76.43.120.55" :time "2024-02-01T08:14:35"
        :pages ["/small/dandelion.html", "/small/mezereon.html",
                "/trees/willow.html"]}
       {:dec-id 2402 :ip "15.6.43.14" :time "2024-02-01T08:18:17"
        :pages ["/trees/oak.html", "/trees/larch.html", "/small/rose.html"]}
       {:dec-id 2403 :ip "192.168.1.42" :time "2024-02-01T08:22:45"
        :pages ["/trees/maple.html", "/trees/spruce.html", "/trees/cedar.html"]}
       {:dec-id 2404 :ip "103.86.98.15" :time "2024-02-01T08:35:12"
        :pages ["/small/lily.html", "/trees/pine.html", "/small/orchid.html"]}
       {:dec-id 2405 :ip "45.77.23.89" :time "2024-02-01T09:03:56"
        :pages ["/trees/birch.html", "/small/sunflower.html",
                "/small/orchid.html"]}
       {:dec-id 2406 :ip "76.43.120.55" :time "2024-02-01T09:17:30"
        :pages ["/trees/poplar.html", "/small/azalea.html",
                "/small/lavender.html"]}
       {:dec-id 2407 :ip "203.0.113.7" :time "2024-02-01T09:45:01"
        :pages ["/small/geranium.html", "/small/iris.html", "/trees/walnut.html"]}
       {:dec-id 2408 :ip "15.6.43.14" :time "2024-02-01T10:12:28"
        :pages ["/small/violet.html", "/trees/spruce.html",
                "/small/geranium.html"]}
       {:dec-id 2409 :ip "198.51.100.17" :time "2024-02-01T10:33:44"
        :pages ["/trees/beech.html", "/trees/larch.html", "/trees/cypress.html"]}
       {:dec-id 2410 :ip "207.46.13.159" :time "2024-02-01T11:05:19"
        :pages ["/small/lily.html", "/trees/spruce.html", "/small/magnolia.html"]}
       {:dec-id 2411 :ip "193.14.87.22" :time "2024-02-01T13:02:11"
        :pages ["/trees/olive.html", "/small/azalea.html",
                "/trees/sycamore.html"]}
       {:dec-id 2412 :ip "76.43.120.55" :time "2024-02-01T12:10:33"
        :pages ["/small/hyacinth.html", "/small/geranium.html",
                "/small/carnation.html"]}
       ;; Moved non-chonologically here to get more unpaired decisions for test.
       {:dec-id 2413 :ip "104.244.42.19" :time "2024-02-01T11:27:55"
        :pages ["/trees/poplar.html", "/small/orchid.html", "/trees/yew.html"]}
       {:dec-id 2414 :ip "15.6.43.14" :time "2024-02-01T14:15:47"
        :pages ["/small/geranium.html", "/trees/baobab.html",
                "/small/narcissus.html"]}
       {:dec-id 2415 :ip "208.80.152.201" :time "2024-02-01T14:50:23"
        :pages ["/trees/sequoia.html", "/small/aster.html", "/trees/larch.html"]}])
    plantportal-meta))

(def plantportal-inters
  (with-meta
    (wrangle/records-as-cols
      [{:inter-id 1 :ip "2.46.9.1" :page "/trees/spruce.html"
        :time "2024-02-01T08:16:26"}
       {:inter-id 2 :ip "15.6.43.14" :page "/trees/oak.html"
        :time "2024-02-01T08:18:52" :gmrs-decision 2402}
       {:inter-id 3  :ip "76.43.120.55" :page "/small/dandelion.html"
        :time "2024-02-01T08:15:12" :gmrs-decision 2401}
       {:inter-id 4  :ip "192.168.1.42" :page "/trees/maple.html"
        :time "2024-02-01T08:23:30" :gmrs-decision 2403}
       {:inter-id 5 :ip "207.46.13.159" :page "/small/magnolia.html"
        :time "2024-02-01T11:06:50"}
       {:inter-id 6  :ip "203.0.113.7" :page "/small/geranium.html"
        :time "2024-02-01T09:47:22" :gmrs-decision 2407}
       {:inter-id 7 :ip "104.244.42.19" :page "/trees/yew.html"
        :time "2024-02-01T11:29:15" :gmrs-decision 2413}
       {:inter-id 8 :ip nil :page "/small/geranium.html"
        :time "2024-02-01T09:46:30"}
       {:inter-id 9  :ip "103.86.98.15" :page "/small/lily.html"
        :time "2024-02-01T08:36:45" :gmrs-decision 2404}
       {:inter-id 10  :ip "198.51.100.17" :page "/trees/beech.html"
        :time "2024-02-01T10:34:30" :gmrs-decision 2409}
       {:inter-id 11 :ip "15.6.43.14" :page "/small/geranium.html"
        :time "2024-02-01T14:16:20" :gmrs-decision 2414}
       {:inter-id 12 :ip "193.14.87.22" :page "/trees/olive.html"
        :time "2024-02-01T13:03:40"}
       {:inter-id 13 :ip nil :page "/small/aster.html"
        :time "2024-02-01T14:51:45"}
       {:inter-id 14  :ip "76.43.120.55" :page "/trees/poplar.html"
        :time "2024-02-01T09:18:10" :gmrs-decision 2406}
       {:inter-id 15  :ip "15.6.43.14" :page "/small/violet.html"
        :time "2024-02-01T10:13:55" :gmrs-decision 2408}
       {:inter-id 16 :ip "192.168.1.100" :page "/trees/cedar.html"
        :time "2024-02-01T14:30:10"}])
    plantportal-meta))

(deftest test-click-through-rate-update
  (let [dec-getter (make-getter plantportal-decs),
        inter-getter (make-getter plantportal-inters)]
   (testing "from scratch, one CTR update"
    (let [full-ctr-from-scratch
          (click-through-rate-update {} 0 {} {}
                                     inter-getter dec-getter 10)]
     (is (get (:result full-ctr-from-scratch) "/small/geranium.html"))
     (is (= 1/2 (:ctr (get (:result full-ctr-from-scratch)
                           "/small/geranium.html")))
         "the CTR should be 1/2 - we get a fraction - and not a higher number
         which could result from also counting interactions unpaired with decs")
     (is (= 0 (:ctr (get (:result full-ctr-from-scratch)
                         "/trees/baobab.html"))))
     (is (< 0.5 (:confidence full-ctr-from-scratch) 1)
         "sane looking accumulated confidence - the difference is small, mostly
         0 CTR, so the confidence = <1-diff> should be fairly high")
     (is (= 0 (:unpaired-inters-count full-ctr-from-scratch)))))
   (testing "incremental update"
    (let [update-1 (click-through-rate-update {} 0 {} {}
                                              inter-getter dec-getter 3)
          update-2 (click-through-rate-update
                    (:result update-1) (:unpaired-inters-count update-1)
                    {} {}
                    (:left-inters update-1) (:left-decs update-1)
                    3)]
     (is (= 5 (count (:left-inters update-1))))
     (is (= 5 (count (:left-decs update-1))))
     (is (= 2 (count (:left-inters update-2))))
     (is (= 2 (count (:left-decs update-2))))
     (is (< 0.5 (:confidence update-1) 1))
     (is (< (:confidence update-1) (:confidence update-2) 1))
     (is (= 0.0 (:ctr (get (:result update-1) "/small/geranium.html")))
         "update 1, dec 2407 not seen yet, so the inter 6 should not count")
     (is (= 1/3 (:ctr (get (:result update-2) "/small/geranium.html")))
         "update 2, inter 11 cannot be associated to unseen dec 2414")
     (is (= 0 (:ctr (get (:result update-1) "/small/lily.html")))
         "update 1, decision seen but not an interaction")
     (is (= 1/2 (:ctr (get (:result update-2) "/small/lily.html")))
         "update 2, pair interaction to decision seen in the prev update")
     (is (= 1 (:unpaired-inters-count update-1)))
     (is (= 2 (:unpaired-inters-count update-2)))))))
(run-test test-click-through-rate-update)

; (run-tests 'gmrs.mills.explore-exploit-test)
