(ns clojure-for-the-brave.Chapter_8)

(def order-details
  {:name "Mitchard Blimmons"
   :email "mitchard.blimmonsgmail.com"})

(def order-details-validations
  {:name
   ["Please enter a name" not-empty]
   :email
   ["Please enter an email address" not-empty
    "Your email address doesn't look like an email address"
    #(or (empty? %) (re-seq #"@" %))]})

(defn error-messages-for
  "Return a seq of error messages"
  [to-validate message-validator-pairs]
  (map first (filter #(not ((second %) to-validate))
                     (partition 2 message-validator-pairs))))

(defn validate
  "Returns a map with a vector of errors for each key"
  [to-validate validations]
  (reduce (fn [errors validation]
            (let [[fieldname validation-check-groups] validation
                  value (get to-validate fieldname)
                  error-messages (error-messages-for value validation-check-groups)]
              (if (empty? error-messages)
                errors
                (assoc errors fieldname error-messages))))
          {}
          validations))

(defmacro if-valid
  "Handle validation more concisely"
  [to-validate validations errors-name & then-else]
  `(let [~errors-name (validate ~to-validate ~validations)]
     (if (empty? ~errors-name)
       ~@then-else)))

(if-valid order-details order-details-validations my-error-name
          (println :success)
          (println :failure my-error-name))
;---------------------------------------------------------------------------------------------------------------
; Exercise 1
;---------------------------------------------------------------------------------------------------------------
(defmacro when-valid
  [to-validate validations & when]
  `(when (empty? (validate ~to-validate ~validations))
     ~@when))

(when-valid order-details order-details-validations
            (println "It's a success!")
            (println :success))

;---------------------------------------------------------------------------------------------------------------
; Exercise 2
;---------------------------------------------------------------------------------------------------------------
(defmacro my-or
  ([] false)
  ([x] x)
  ([x & next]
   `(let [or# ~x]
      (if or# or# (my-or ~@next)))))

(my-or nil "crazy default value")

(my-or "value" "not useful default value")

;---------------------------------------------------------------------------------------------------------------
; Exercise 3
;---------------------------------------------------------------------------------------------------------------
(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})

(defn attr
  [att]
  (fn
    [character]
    ((comp att :attributes) character)))

(defmacro defattrs
  [& args]
  `(do
     ~@(map
         (fn [[f attr]] `(def ~f (attr ~attr)))
         (partition 2 args))))

(defattrs c-int :intelligence c-str :strength c-dex :dexterity)
