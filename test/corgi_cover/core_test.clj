(ns corgi-cover.core-test
  (:require [clojure.test :refer :all]
            [corgi-cover.core :refer :all]))

(def valid-test-states
  #{"IL" "WA" "NY" "CO"})
(def test-data
  [{:name "Chloe" :state "IL" :corgi-count 1 :policy-count 0}
   {:name "Ethan" :state "IL" :corgi-count 4 :policy-count 2}
   {:name "Annabelle" :state "WY" :corgi-count 19 :policy-count 0}
   {:name "Logan" :state "WA" :corgi-count 2 :policy-count 1}])

(deftest eligible-test
  (testing "Corgi cover eligibility"
    (doseq [td test-data]
      (let [s (:state td)
            c (:corgi-count td)]
        (if (valid-test-states s)
          (is (eligible? s c))
          (is (not (eligible? s c))))))))
