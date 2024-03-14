(ns corgi-cover.core "Corgi Cover Eligibility"
    (:require [clojure.string :as string]
              [clojure.java.io :as io]
              [clojure.data.csv :as csv]
              [clojure.data.json :as json])
    (:import [java.io BufferedReader StringReader FileNotFoundException]))

(def eligible-file-path
  "Saved records of all valid corgi cover applications"
  "./resources/out/eligible-corgi-cover-applications.csv")
(def json-file-path
  "Saved records of all valid corgi cover applications used in Megacorp
merger."
  "./resources/out/eligible-corgi-cover-applications.json")
(def ineligible-file-path
  "Saved records of all invalid corgi cover applications"
  "./resources/out/ineligible-corgi-cover-applications.csv")

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

;; not-eligible? : Application -> string?
(defn not-eligible?
  "Returns NIL indicating a person is eligible or returns the reason as a
  string if a problem is found.
  Throws an Exception for invalid inputs."
  [{a-state :state corgi-count :corgi-count}]
  (when-not (eligible? a-state corgi-count)
    (if (not (contains? state a-state))
      (str "Residence not eligible.")
      (str "Does not own a Corgi."))))

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
  [{s :state cc :corgi-count pc :policy-count}]
  (tier-coverage s cc pc))

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
      (when (.exists (io/file file))
        (let [contents (line-seq (BufferedReader. (StringReader. (slurp file))))
              header       (first contents)
              applications (rest contents)
              converted    (parse-map header applications)]
          (doseq [application converted] (println application))
          converted))
      (catch FileNotFoundException x
        (println "Unable to load/find file: " file)))))

;; verify-applications : [Applications] -> IO Files
(defn verify-applications
  "OBSOLETE - replaced by `validate-applications`

  Processes the applications, opens two output files and writes to them
  based upon eligibility check."
  [applications]
  (letfn [(application->string [m]
            (str (:name m) ", " (:state m) ", " (:corgi-count m) ", "
                 (:policy-count m) "\n"))
          (write-file [f a]
            (let [csv-header (str "name, state, corgi-count, policy-count\n")]
              (if (.exists (io/file f))
                (spit f (application->string a) :append true)
                (spit f (str csv-header (application->string a))))))
          (verify [a]
            (if (eligible? (:state a) (:corgi-count a))
              (try
                (write-file eligible-file-path a)
                1
                (catch Exception x
                  (println "Issue processing Eligible applications:\n" x)))
              (try
                (write-file ineligible-file-path a)
                0
                (catch Exception x
                  (println "Issue processing Ineligible applications:\n" x)))))]

    (let [verified (map verify applications)
          valid (reduce + verified)
          invalid (- (count verified) valid)]
      (format "%d of %d applications were valid. Remaining %d were invalid"
              valid (count verified) invalid))))

;; validate-applications : [Applications] -> IO
(defn validate-applications
  "Processes the applications, opens two output files and writes to them
  based upon eligibility check."
  [applications]
  (letfn [(application->string [m]
            (if (:reason m)
              (str (:name m) ", " (:state m) ", " (:corgi-count m) ", "
                   (:policy-count m) ", " (:reason m) "\n")
              (str (:name m) ", " (:state m) ", " (:corgi-count m) ", "
                   (:policy-count m) "\n")))
          (write-file [f a]
            (let [csv-header
                  (if (:reason a)
                    (str "name, state, corgi-count, policy-count, reason\n")
                    (str "name, state, corgi-count, policy-count\n"))]
              (if (.exists (io/file f))
                (spit f (application->string a) :append true)
                (spit f (str csv-header (application->string a))))))
          (verify [a]
            (let [reason (not-eligible? a)]
              (if reason
                (try
                  (write-file ineligible-file-path (assoc a :reason reason))
                  0
                  (catch Exception x
                    (println "Issue processing Invalid applications:\n" x)))
                (try
                  (write-file eligible-file-path a)
                  1
                  (catch Exception x
                    (println "Issue processing Valid applications:\n" x))))))]

    (let [verified (map verify applications)
          valid (reduce + verified)
          invalid (- (count verified) valid)]
      {:valid valid :invalid invalid})))

;; load-and-validate : IO -> IO
;; Saves valid application data to a JSON file
#_(defn load-and-validate [file-in] (throw (ex-info "Not yet implemented" {:file-in file-in})))

(defn load-and-validate
  "Writes valid applications into a JSON file.
  May throw a FileNotFoundException in the event of errors."
  [csv]
  (letfn [(csv->map [head & applications]
            (map #(zipmap (map keyword head) % 1) applications))]
    (when (pos? (:valid (->> csv
                             load-applications
                             validate-applications)))
      (try
        (with-open [writer (io/writer json-file-path)]
          (json/write (apply csv->map
                             (with-open [reader (io/reader eligible-file-path)]
                               (doall
                                (csv/read-csv reader))))
                      writer))
        (catch FileNotFoundException fnf
          (print "Unable to locate "))))))
