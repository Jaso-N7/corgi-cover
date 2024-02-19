(ns corgi-cover.core "Corgi Cover Eligibility")

;;; --- Data Definition:
(def state #{"IL" "WA" "NY" "CO"})
;; A State is a set: (hash-set S)
;; where S is an abbreviated String representing valid states
;; in the Country.

;;; --- Contract, Purpose, Header:
;; eligible? : State Natural -> Boolean
;; Given the State and Corgi count, return a boolean indicating
;; a persons eligibilty for the "corgi cover" policy.
;; Throws an Exception for invalid inputs

;;; --- Examples: see tests

;;; --- Template:
(comment
  (defn eligible?-fn [a-state corgi-count]
    (if (pos? corgi-count)
      (f-to-check-state a-state)
      (throw (ex-info "Invalid args")))))

;;; --- Definitions:
(defn eligible?
  "Given the State and Corgi count, return a boolean indicating
  a persons eligibilty for the 'corgi cover' policy.
  Throws an Exception for invalid inputs.
  (eligible? \"IL\" 1) => true"
  [a-state corgi-count]
  (when-not (and (string? a-state) (nat-int? corgi-count))
    (throw (ex-info "Invalid inputs" {:a-state a-state
                                      :corgi-count corgi-count})))
  (and (contains? state a-state) (pos? corgi-count)))
