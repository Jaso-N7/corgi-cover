(ns corgi-cover.core "Corgi Cover Eligibility"
    (:require [clojure.string :as string])
    (:import [java.io BufferedReader StringReader]))

;;; --- Data Definitions:
(def state
  "A State is a set:
    (hash-set S)
  representing an individuals address to be considered eligible,
  where S is an abbreviated String"
  #{"IL" "WA" "NY" "CO"})

;; A Tier is one of:
;; - :platinum
;; - :gold
;; - :silver
;; - :none
;; INTERPRETATION: Tiers/Packages offered for "Corgi Cover" insurance
;; WHERE: the keyword reflects
;; "corgi cover platinum",
;; "corgi cover gold",
;; "corgi cover silver" or
;; "none" / "non-eligibility" .
;; TEMPLATE:
#_(defn fn-for-tier [t]
    (cond (= t :platinum) '?
          (= t :gold) '?
          (= t :silver) '?
          (= t :none) '?))
;; rules used:
;; - one of: 4 cases
;; - atomic distinct value: keyword

;; An Application is a Map:
;;   (hash-map :name S :state S :corgi-count N :policy-count N)
;; INTERPRETATION: contains details regarding the policy holder
;; WHERE: S is a String and N is a Natural number

;; A Policy is a Map:
;;   (hash-map NM [PS])
;; INTERPRETATION: Policies held by applicant
;; WHERE: NM is the applicants name and PS are the names of their
;; current policies. Both are represented as String.

;;; --- Function Definitions:

;; eligible? : State natural -> boolean
(defn eligible?
  "Returns a boolean indicating a persons eligibilty for the
  'corgi cover' policy.
  Throws an Exception for invalid inputs.
  Examples
  - (eligible? \"IL\" 1) => true"
  [a-state corgi-count]
  (when-not (and (string? a-state) (nat-int? corgi-count))
    (throw (ex-info "Invalid inputs" {:a-state a-state
                                      :corgi-count corgi-count})))
  (and (contains? state a-state) (pos? corgi-count)))

;; tier-coverage : State natural natural -> Tier
(defn tier-coverage
  "Offers 'Corgi Cover' at the specified tier, if the owner lives
 in a particular state, owns a specified amount of corgies and/or
 has an existing policy count.
  Returns :none if not eligible for any Tier."
  [a-state corgi-count policy-count]
  (if (eligible? a-state corgi-count)
    (cond (or (>= corgi-count 7)
              (and (>= corgi-count 3)
                   (pos-int? policy-count))) :platinum
          (>= corgi-count 3)                 :gold
          :else                              :silver)
    :none))

;; register : Application -> Tier
(defn register
  "Input a 'corgi cover' application and determine eligibility"
  [an-application]
  (let [{s :state cc :corgi-count pc :policy-count} an-application]
    (tier-coverage s cc pc)))

;; registration : Application Policy -> Tier
(defn registration
  "Returns Tier coverage based on existing policies and eligibility"
  [an-application policies]
  (let [{applicant :name} an-application
        ms-holder? ((set (get policies applicant)) "megasafe")
        tier (register an-application)]
    (cond (and (not= :none tier) ms-holder?) :platinum
          (not ms-holder?) tier)))

;; load-applications : IO File -> [Applications]
(defn load-applications
  "Opens CSV containing Corgi Cover applications and converts the information
  to an internal data structure.
  Returns NIL if unable to perform operation."
  [file]
  (letfn [(parse-map [ks vs]
            (vec (for [v vs]
                   (apply merge (map #(hash-map (keyword %1)
                                                (or (parse-long %2) %2))
                                     (string/split ks #", ")
                                     (string/split v #", "))))))]

    (try
      (when (.exists (clojure.java.io/file file))
        (let [contents (line-seq (BufferedReader. (StringReader. (slurp file))))
              header       (first contents)
              applications (rest contents)
              converted    (parse-map header applications)]
          (doseq [application converted] (println application))
          converted))
      (catch java.io.FileNotFoundException x
        (println "Unable to load/find file: " file)))))
