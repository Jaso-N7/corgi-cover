(ns corgi-cover.core-test
  (:require [clojure.test :refer :all]
            [corgi-cover.core :refer :all]))

(deftest eligible-test
  (testing "Corgi cover eligibility"
    (let [test-data [{:name "Chloe" :state "IL" :corgi-count 1 :policy-count 0}
                     {:name "Ethan" :state "IL" :corgi-count 4 :policy-count 2}
                     {:name "Annabelle" :state "WY" :corgi-count 19 :policy-count 0}
                     {:name "Logan" :state "WA" :corgi-count 2 :policy-count 1}]]
      (doseq [td test-data]
        (let [{s :state c :corgi-count} td]
          (if (state s)
            (is (eligible? s c))
            (is (not (eligible? s c)))))))))

(deftest tier-coverage-test
  (testing "Returns the correct tier"
    (is (= :platinum (tier-coverage "IL" 7 0)))
    (is (= :platinum (tier-coverage "WA" 3 1)))
    (is (= :gold     (tier-coverage "NY" 3 0)))
    (is (= :silver   (tier-coverage "CO" 1 0)))
    (is (= :none     (tier-coverage "WY" 19 0)))))

(deftest register-test
  (testing "Returns the correct tier based on application"
    (let [test-data [{:name "Chloe" :state "IL" :corgi-count 1 :policy-count 0}
                     {:name "Ethan" :state "IL" :corgi-count 4 :policy-count 2}
                     {:name "Annabelle" :state "WY" :corgi-count 19 :policy-count 0}
                     {:name "Logan" :state "WA" :corgi-count 2 :policy-count 1}]]
      (is (= :silver   (register (get test-data 0))))
      (is (= :platinum (register (test-data 1))))
      (is (= :none     (register (test-data 2))))
      (is (= :silver   (register (test-data 3)))))))

(deftest registration-test
  (testing "Returns the correct tier based on application"
    (let [test-data [{:name "Chloe" :state "IL" :corgi-count 1 :policy-count 0}
                     {:name "Ethan" :state "IL" :corgi-count 4 :policy-count 2}
                     {:name "Annabelle" :state "WY"
                      :corgi-count 19 :policy-count 0}
                     {:name "Logan" :state "WA"
                      :corgi-count 2 :policy-count 1}]
          test-policies {"Chloe" ["secure goldfish"]
                         "Ethan" ["cool cats cover" "megasafe"]}]
      (is (= :silver   (registration (get test-data 0) test-policies)))
      (is (= :platinum (registration (test-data 1) test-policies)))
      (is (= :none     (registration (test-data 2) test-policies)))
      (is (= :silver   (registration (test-data 3) test-policies))))))

;; Ensure ./resources/{in,out} are already created
(deftest test-onboarding
  (let [test-data [{:name "Chloe" :state "IL" :corgi-count 1 :policy-count 0}
                   {:name "Ethan" :state "IL" :corgi-count 4 :policy-count 2}
                   {:name "Annabelle" :state "WY"
                    :corgi-count 19 :policy-count 0}
                   {:name "Logan" :state "WA"
                    :corgi-count 2 :policy-count 1}]
        file "./resources/in/corgi-cover-applications.csv"
        bad-file "./does/not/exist"]
    
    (testing "Can read corgi cover applications CSV file"
      (let [applications (slurp file)]
        (is (= "name, state, corgi-count, policy-count\nChloe, IL, 1, 0\nEthan, IL, 4, 2\nAnnabelle, WY, 19, 0\nLogan, WA, 2, 1"
               applications))))
    
    (testing "Can convert file to data structure"
      (is (= test-data (load-applications file))))
    
    (testing "Validate CSV loaded applications"
      (let [test-policies {"Chloe" ["secure goldfish"]
                           "Ethan" ["cool cats cover" "megasafe"]}]
        (is (= '(true true false true)
               (map #(eligible? (:state %) (:corgi-count %))
                    (load-applications file))))
        (is (= [:silver :platinum :none :silver]
               (map register (load-applications file))))
        (is (= [:silver :platinum :none :silver]
               (map register (load-applications file))))
        (is (= [:silver :platinum]
               (map registration (load-applications file) test-policies)))))

    (testing "Gracefully handles issues"
      (is (nil? (load-applications bad-file))))
    
    (testing "Can create valid and invalid applications CSV files"
      (let [good-file "./resources/out/eligible-corgi-cover-applications.csv"
            bad-file "./resources/out/ineligible-corgi-cover-applications.csv"]

        (map (fn clean-up [f]
               (when (.exists (clojure.java.io/file f))
                 (.delete (clojure.java.io/file f))))
             [good-file bad-file]) 

        (try (verify-applications (load-applications file))
             (catch Exception x
               (throw (ex-info "Double-check code" {:x x}))))
        
        (is (.exists (clojure.java.io/file good-file)))
        (is (.exists (clojure.java.io/file bad-file)))
        (is (= "name, state, corgi-count, policy-count\nChloe, IL, 1, 0\nEthan, IL, 4, 2\nLogan, WA, 2, 1\n"
               (slurp good-file)))
        (is (= "name, state, corgi-count, policy-count\nAnnabelle, WY, 19, 0\n"
               (slurp bad-file)))))))
