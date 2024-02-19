(ns corgi-cover.core "Corgi Cover Eligibility")

;;; --- Data Definitions:
(def state
  "A State is a set: (hash-set S)
  where S is a(n) (abbreviated) String representing where an individual
  must live to be considered eligible."
  #{"IL" "WA" "NY" "CO"})

;; A Tier is a keyword of either:
;; 1. :platinum
;; 2. :gold
;; 3. :silver
;; 4. :none
;; where the keyword reflects
;; "corgi cover platinum",
;; "corgi cover gold",
;; "corgi cover silver" or
;; "none" / "non-eligibility" . 

;;; --- Contract, Purpose, Header:

;; eligible? : State natural -> boolean
;; Given the State and Corgi count, return a boolean indicating
;; a persons eligibilty for the "corgi cover" policy.
;; Throws an Exception for invalid inputs
#_(eligible? [a-state corgi-count] false)

;; tier-coverage : State natural natural -> Tier
;; Offers 'Corgi Cover' at the specified tier, if the owner lives
;; in a particular state, owns a specified amount of corgies and/or
;; has an existing policy count.
;; Returns 'nil' if not eligible for any Tier
#_(tier-coverage [a-state corgi-count policy-count] :?)

;;; --- Examples: see tests

;;; --- Template:
(comment
  (defn eligible? [a-state corgi-count]
    (if (pos? corgi-count)
      (fn-to-check-state a-state)
      (throw (ex-info "Invalid args")))))

(comment
  (defn tier-coverage [a-state corgi-count policy-count]
    (cond (or (>= corgi-count 7)
              (and (>= corgi-count 3)
                   (pos-int? policy-count))) :?
          (>= corgi-count 3) :?
          (fn-to-check-original-policy a-state corgi-count) :?
          :else :?)))

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

(defn tier-coverage
  [a-state corgi-count policy-count]
  (if (eligible? a-state corgi-count)
    (cond (or (>= corgi-count 7)
              (and (>= corgi-count 3)
                   (pos-int? policy-count))) :platinum
          (>= corgi-count 3)                 :gold
          :else                              :silver)
    :none))
