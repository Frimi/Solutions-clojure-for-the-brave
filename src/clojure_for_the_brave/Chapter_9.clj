(ns clojure-for-the-brave.Chapter-9
  (:require [clojure.string :as str]))

;---------------------------------------------------------------------------------------------------------------
;Butter Issue
;---------------------------------------------------------------------------------------------------------------
(def yak-butter-international
  {:store      "Yak Butter International"
   :price      90
   :smoothness 90})

(def butter-than-nothing
  {:store      "Butter Than Nothing"
   :price      150
   :smoothness 83})

;; This is the butter that meets our requirements
(def baby-got-yak
  {:store      "Baby Got Yak"
   :price      110
   :smoothness 99})

(defn mock-api-call
  [result]
  (Thread/sleep 1000)
  result)

(defn satisfactory?
  "If the butter meets our criteria, return the butter, else return false"
  [butter]
  (and (<= (:price butter) 100)
       (>= (:smoothness butter) 97)
       butter))

(comment (time (some (comp satisfactory? mock-api-call)
                     [yak-butter-international butter-than-nothing baby-got-yak])))

(comment (time
           (let [butter-promise (promise)]
             (doseq [butter [yak-butter-international butter-than-nothing baby-got-yak]]
               (future (if-let [satisfactory-butter (satisfactory? (mock-api-call butter))]
                         (deliver butter-promise satisfactory-butter))))
             (println "And the winner is:" (deref butter-promise 1100 "timeout")))))

;---------------------------------------------------------------------------------------------------------------
;Enqueue Issue
;---------------------------------------------------------------------------------------------------------------
(defmacro wait
  "Sleep `timeout` seconds before evaluating body"
  [timeout & body]
  `(do (Thread/sleep ~timeout) ~@body))

(comment (time (let [saying3 (promise)]
                 (future (deliver saying3 (wait 100 "Cheerio!")))
                 @(let [saying2 (promise)]
                    (future (deliver saying2 (wait 400 "Pip pip!")))
                    @(let [saying1 (promise)]
                       (future (deliver saying1 (wait 200 "'Ello, gov'na!")))
                       (println @saying1)
                       saying1)
                    (println @saying2)
                    saying2)
                 (println @saying3)
                 saying3)))

(defmacro enqueue
  ([q concurrent-promise-name concurrent serialized]
   `(let [~concurrent-promise-name (promise)]
      (future (deliver ~concurrent-promise-name ~concurrent))
      (deref ~q)
      ~serialized
      ~concurrent-promise-name))
  ([concurrent-promise-name concurrent serialized]
   `(enqueue (future) ~concurrent-promise-name ~concurrent ~serialized)))

(comment (-> (enqueue saying (wait 200 "'Ello, gov'na!") (println @saying))
             (enqueue saying (wait 400 "Pip pip!") (println @saying))
             (enqueue saying (wait 100 "Cheerio!") (println @saying))))

;---------------------------------------------------------------------------------------------------------------
;Exercise 1
;---------------------------------------------------------------------------------------------------------------
(def site<>url
  {:google "https://www.google.com.br/search?q="
   :bing "https://www.bing.com/search?q="})

(defn search-phrase
  [phrase url]
  (slurp (str url phrase)))

(defn search
  [phrase]
  (let [google-search (promise)
        bing-search (promise)]
    (future
      (deliver google-search (search-phrase phrase (:google site<>url))))
    (future
      (deliver bing-search (search-phrase phrase (:bing site<>url))))
    (deref bing-search 2000 "Timeout Error")
    (deref google-search 2000 "Timeout Error")))

(comment (search "batata"))
;---------------------------------------------------------------------------------------------------------------
;Exercise 2
;---------------------------------------------------------------------------------------------------------------
(defn search-general
  "Search for the front page HTML results of a string on a site (:google or :bing)"
  [phrase site]
  (let [search (promise)]
    (future
      (deliver search (condp = site
                        :google (search-phrase phrase (:google site<>url))
                        :bing (search-phrase phrase (:bing site<>url)))))
    (deref search 2000 "Timeout Error")))

(comment (search-general "batata" :bing))

;---------------------------------------------------------------------------------------------------------------
;Exercise 3
;---------------------------------------------------------------------------------------------------------------
(defn get-urls
  [source]
  (re-seq #"https?://[^\"]*" source))

(defn search-efficiently
  [search-phrase url]
  (let [urls (atom [])]
   (with-open [page (clojure.java.io/reader (str url search-phrase))]
     (doseq [line (line-seq page)]
       (when-let [filtered-line (get-urls line)]
         (swap! urls conj filtered-line))))
   @urls))

(defn search-https-vetor
  "Search for the front page HTML results of a string on a site (:google or :bing)"
  [search-phrase site]
  (let [search (promise)]
    (future
      (deliver search (condp = site
                        :google (search-efficiently search-phrase (:google site<>url))
                        :bing (search-efficiently search-phrase (:bing site<>url)))))
    (deref search 2000 "Timeout Error")))

(search-https-vetor "batata" :bing)
(search-https-vetor "andre" :google)