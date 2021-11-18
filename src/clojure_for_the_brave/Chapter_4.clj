(ns clojure-for-the-brave.Chapter-4
  (:require [clojure.data.csv :as csv]))

(def filename "suspects.csv")

;---------------------------------------------------------------------------------------------------------------
;Map Using Reduce
;---------------------------------------------------------------------------------------------------------------
(defn my-map [f & c]
  (let [c (partition (count c)
                     (apply interleave c))]
    (lazy-seq (reduce (fn [s k] (conj s (apply f k))) [] c))))

;credits: https://clojuredocs.org/clojure.core/reduce#example-564d10bce4b0538444398272

;---------------------------------------------------------------------------------------------------------------
;Conj in terms of Into
;---------------------------------------------------------------------------------------------------------------
(defn my-conj
  [target & additions]
  (into target additions))

;---------------------------------------------------------------------------------------------------------------
;Into in terms of Conj
;---------------------------------------------------------------------------------------------------------------
(defn my-into
  [target additions]
  (apply conj target additions))

;---------------------------------------------------------------------------------------------------------------
;Partial in terms of Apply
;---------------------------------------------------------------------------------------------------------------
(defn my-partial
  [partialized-fn & args]
  (fn [& more-args]
    (apply partialized-fn (into more-args args))))

;---------------------------------------------------------------------------------------------------------------
;Vampire Issue
;---------------------------------------------------------------------------------------------------------------
(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name          identity
                  :glitter-index str->int})
(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\r\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(glitter-filter 3 (mapify (parse (slurp filename))))

;---------------------------------------------------------------------------------------------------------------
;Exercise 1
;---------------------------------------------------------------------------------------------------------------

(defn glitter-filter-names
  [minimum-glitter records]
  (map :name (filter #(>= (:glitter-index %) minimum-glitter) records)))

(glitter-filter-names 3 (mapify (parse (slurp filename))))

;---------------------------------------------------------------------------------------------------------------
;Exercise 2
;---------------------------------------------------------------------------------------------------------------

(defn append-suspect
  [{:keys [name glitter-index]}
   filename]
  (spit filename
        (str "\r\n" name "," glitter-index) :append true))

;(append-suspect {:name "Gustavo" :glitter-index 23} filename)

;---------------------------------------------------------------------------------------------------------------
;Exercise 3
;---------------------------------------------------------------------------------------------------------------

(defn validate-name
  [{:keys [name]}]
  (string? name))

(defn validate-glitter-index
  [{:keys [glitter-index]}]
  (number? glitter-index))

(def map-keywords
  {:name?          validate-name
   :glitter-index? validate-glitter-index})

(defn validate-and-append
  [map-keywords record]
  (let [check-name ((map-keywords :name?) record)
        check-glitter-index ((map-keywords :glitter-index?) record)]
    (when (and check-name check-glitter-index)
      (append-suspect record filename))))

;test
;(validate-and-append map-keywords {:name "Gustavo" :glitter-index 24})
;(validate-and-append map-keywords {:name 33 :glitter-index 23})
;(validate-and-append map-keywords {:name "Gustavo" :glitter-index "23"})
;(validate-and-append map-keywords {:name1 "Gustavo" :glitter-index 23})

;---------------------------------------------------------------------------------------------------------------
;Exercise 3 - Alternative solution
;---------------------------------------------------------------------------------------------------------------

(defn record-valid?
  [record]
  (and (validate-name record) (validate-glitter-index record)))

(defn validate-and-append2
  [record]
  (when (record-valid? record)
    (append-suspect record filename)))

;test
;(validate-and-append2 {:name "Vinicius" :glitter-index 24})
;(validate-and-append2 {:name 33 :glitter-index 23})
;(validate-and-append2 {:name "Gustavo" :glitter-index "23"})
;(validate-and-append2 {:name1 "Gustavo" :glitter-index 23})

;---------------------------------------------------------------------------------------------------------------
;Exercise 4
;---------------------------------------------------------------------------------------------------------------

(defn add-eol
  [text]
  (map #(str % "\r\n") text))

(defn separate-with-comma
  [coll]
  (map #(clojure.string/join "," %) coll))

(defn map-to-csv
  [coll]
  (clojure.string/join (add-eol
                         (separate-with-comma
                           (map vals coll)))))

;(str (map-to-csv (mapify (parse (slurp filename)))))

;---------------------------------------------------------------------------------------------------------------
;Exercise 4 - Thread Last
;---------------------------------------------------------------------------------------------------------------

(defn map-to-csv-thread
  [coll]
  (->> coll
       (map vals)
       separate-with-comma
       add-eol
       clojure.string/join))

;(str (map-to-csv-thread (mapify (parse (slurp filename)))))