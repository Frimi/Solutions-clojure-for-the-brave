(ns clojure-for-the-brave.core-test
  (:require [clojure.test :refer :all]
            [clojure-for-the-brave.Chapter5 :as sut]))

(defn async-generator-mouth
  [valor]
  [{:name "head" :size 3}
   {:name "left-eye" :size 1}
   {:name "left-ear" :size 1}
   {:name "mouth" :size 1}
   {:name "nose" :size valor}
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

(deftest a-test
  (testing "FIXME, I fail."
    (is (= (sut/my-update-in (async-generator-mouth 1) [4 :size] / 2)
           (async-generator-mouth 1/2))))
  (testing "FIXME, I fail."
    (is (= (sut/my-update-in (async-generator-mouth 1) [4 :size] + 10 10 10 10 10)
           (async-generator-mouth 51)))))
