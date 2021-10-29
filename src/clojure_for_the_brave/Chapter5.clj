(ns clojure-for-the-brave.Chapter5)

;---------------------------------------------------------------------------------------------------------------
; Exercise 1
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

;---------------------------------------------------------------------------------------------------------------
; Exercise 2
;
; Comp any number of functions
;
; Source: https://gist.github.com/apbendi/f9a2c2ea80167cdc005b
;---------------------------------------------------------------------------------------------------------------

(defn new-comp
  [f & coll-functions]
  (if (empty? coll-functions)
    f
    (let
      [function (first coll-functions)
       new-coll-functions (rest coll-functions)]
      (recur (fn [& args]
               (f (apply function args))) new-coll-functions))))

;((new-comp true? empty? identity)[])

;---------------------------------------------------------------------------------------------------------------
; Exercise 3
;
; Source: https://github.com/phyous/brave-clojure/blob/master/src/brave_clojure/ch_5_functional_programming.clj
;---------------------------------------------------------------------------------------------------------------

(defn my-assoc-in
  [m [k & ks] v]
  (if ks
    (assoc m k (my-assoc-in (get m k) ks v))
    (assoc m k v)))

;---------------------------------------------------------------------------------------------------------------
; Exercise 4 - update-in example
;---------------------------------------------------------------------------------------------------------------

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(update-in asym-hobbit-body-parts [4 :size] #(+ 10 %))

;---------------------------------------------------------------------------------------------------------------
; Exercise 5 - build update-in
;---------------------------------------------------------------------------------------------------------------

(defn my-update-in
  [m [k & ks] f & args]
  (if ks
    (assoc m k (apply my-update-in (get m k) ks f args))
    (apply update m k f args)))