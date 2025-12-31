(ns gmrs.io.getters-test
  (:require [clojure.test :refer :all]
            [gmrs.command :as cmd]
            [gmrs.io.getters :refer :all]
            [gmrs.wrangle :as wrangle]
            [gmrs.io.baseless :as bs]))

(deftest test-get-governor
  (let [GlobalIOSetup (bs/toy-temp-baseless-io-setup),
        GlobalIOSettings (bs/toy-temp-baseless-io-settings)]
    (run! (fn [send-fun] (send-fun GlobalIOSettings "test-guvna" {:hello "governor"}))
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

(deftest test-integr-getter-items-sent-to-baseless
  ;; Bind setup to  new instance to be sure we avoid pollution.
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
          "getter with cols subsetting"))
    (let [opts-getter (getter cmd/*GlobalIOSettings*
                              (:option-gives cmd/*GlobalIOSetup*)
                              [:volume :city]
                              {:ref-city [[`(:= "Kraków")]]})]
      (is (= (select-keys (wrangle/records-as-cols (rest example-options))
                          [:volume :city])
             (first opts-getter))
          "getter with dataprefs"))))

(def hotel-cases
  ;; Each of the sub-vectors is meant to supply one "give" and has one page
  ;; inside.
  [[[{:name "John Smith" :country "USA" :checkin-until "22:00"
     :avg-price 180 :amenities "pool|wifi|pet-friendly" :age 34
     :travel-purpose "business"}
    {:name "Sarah Johnson" :country "USA" :checkin-until "20:00"
     :avg-price 50 :amenities "wifi|electric-car-charging" :age 78
     :travel-purpose "leisure"}]]
   [[{:name "Pierre Dubois" :country "France" :checkin-until "23:00"
     :avg-price 120 :amenities "breakfast|wifi|bicycle-rental" :age 45
     :travel-purpose "business"}
    {:name "Marie Leroy" :country "France" :checkin-until "21:00"
     :avg-price 90 :amenities "wifi|kitchenette" :age 21
     :travel-purpose "leisure"}]]
   [[{:name "Erik Andersson" :country "Sweden" :checkin-until "24:00"
     :avg-price 160 :amenities "gym|wifi|airport-shuttle" :age 29
     :travel-purpose "business"}
    {:name "Anna Lindqvist" :country "Sweden" :checkin-until "22:00"
     :avg-price 140 :amenities "breakfast|pool|wifi|babysitting"
     :age 38 :travel-purpose "leisure"}]]])

(def hotel-case-gives
  (map (fn [source] (fn [io-settings & prefs] source))
       hotel-cases))

(defn prepared-io-setup []
  (let [setup (atom (bs/toy-temp-baseless-io-setup))]
    (swap! setup assoc-in [:case-gives] hotel-case-gives)
    @setup))

(deftest test-integr-getter-custom-gives
  (binding [cmd/*GlobalIOSetup* (prepared-io-setup)]
    (let [get (cases-getter cmd/*GlobalIOSettings* cmd/*GlobalIOSetup*)]
      (is (= (wrangle/records-as-cols (apply concat (map first hotel-cases)))
             (first get))))))

; (run-tests `gmrs.io.getters-test)
