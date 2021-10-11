(ns clojure-for-the-brave.Chapter-3)


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

(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set [part (matching-part part)])))))))

(defn my-reduce
  ([f initial coll]
   (loop [result initial
          remaining coll]
     (if (empty? remaining)
       result
       (recur (f result (first remaining)) (rest remaining)))))
  ([f [head & tail]]
   (my-reduce f head tail)))

(defn better-symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part (matching-part part)])))
          []
          asym-body-parts))

(defn hit
  [asym-body-parts]
  (let [sym-parts (better-symmetrize-body-parts asym-body-parts)
        body-part-size-sum (reduce + (map :size sym-parts))
        target (rand body-part-size-sum)]
    (loop [[part & remaining] sym-parts
           accumulated-size (:size part)]
      (println "rand value:" target "accumalted size" accumulated-size "discarted part" part)
      (if (> accumulated-size target)
        part
        (recur remaining (+ accumulated-size (:size (first remaining))))))))

(println
  (hit asym-hobbit-body-parts))

;---------------------------------------------------------------------------------------------------------------
;Exercise 1
;---------------------------------------------------------------------------------------------------------------
(println (str 3 1 2 3 3 3 3 3))

(println (str "testing" " " 3 1 2 3 3 3 3 3))

(println (list 1 2 3 4))

(println (vector 1 2 3 4))

(println (hash-map :key1 2 :key2 4))

(println (hash-set 1 2 3 3 3 3 3 3))

(println (set '(1 2 3 3 3 3 3)))

(println (set [ 1 2 3 3 3 3 3]))

;---------------------------------------------------------------------------------------------------------------
;Exercise 2
;---------------------------------------------------------------------------------------------------------------
(defn inc100
  "function that takes a number and adds 100 to it"
  [number]
  (+ number 100))

;---------------------------------------------------------------------------------------------------------------
;Exercise 3
;---------------------------------------------------------------------------------------------------------------
(defn dec-maker
  [number]
  #(- %1 number))

(def dec9 (dec-maker 9))
(def dec100 (dec-maker 100))

(println (dec9 10))
(println (dec100 102))

;---------------------------------------------------------------------------------------------------------------
;Exercise 4
;---------------------------------------------------------------------------------------------------------------
(defn mapset
  [f coll]
  (set (map f coll)))

(println (mapset inc [1 1 2 2]))

;---------------------------------------------------------------------------------------------------------------
;Exercise 5-6
;---------------------------------------------------------------------------------------------------------------
(defn replace-alien-part
  [{:keys [size name]} index]
  {:name (clojure.string/replace name #"^left-" (str "number" "-" index "-")),
   :size size})

(defn replace-alien-part?
  [{name :name}]
  (re-find #"^left-" name))

(defn replace-n-times
  [part n]
  (loop [new-parts []
         n n]
    (if (= n 0)
      new-parts
      (recur (conj new-parts (replace-alien-part part n))
             (dec n)))))

(defn new-alien-part
  [part n]
  (if (replace-alien-part? part)
    (replace-n-times part n)
    (conj [] part)))

(defn generic-symmetrize-body-parts-aliens
    "Expects a seq of maps that have a :name and :size and a number of matching body"
    [ref-parts n]
  (into [] (reduce (fn [initial part]
                     (concat initial (new-alien-part part n)))
                   [] ref-parts)))

(println (generic-symmetrize-body-parts-aliens asym-hobbit-body-parts 10))
