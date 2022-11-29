(ns aoc.2020.day-18
  [:require
   [aoc.utils :as u]
   [clojure.core.match :refer [match]]
   [clojure.string :as s]])

(defn parse-expression [expr]
  (let [expanded (s/replace expr #"\(|\)" {"(" "( " ")" " )"})]
    (map #(if (re-matches #"\d+" %)
            (u/parse-int %)
            %) (re-seq #"[^\s]+" expanded))))

(def data
  (->> (slurp "resources/2020/day-18.txt")
       s/split-lines
       (map parse-expression)))

(defn simplify [expr]
  (match [expr]
         [([x] :seq)] x
         [([x ")" & r] :seq)] (conj r x)
         [(["(" & r] :seq)] (simplify (simplify r))
         [([a op "(" & r] :seq)] (simplify (concat (list a op) (simplify r)))
         [([a "+" b & r] :seq)] (simplify (conj r (+ a b)))
         [([a "*" b & r] :seq)] (simplify (conj r (* a b)))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map simplify)
        (apply +))))

(defn complexify [expr]
  (match [expr]
         [([x] :seq)] (conj nil x)
         [(["(" x ")" & r] :seq)] (conj r x)
         [([x ")" & r] :seq)] (concat (list x ")") r)
         [(["(" & r] :seq)]  (complexify (conj (complexify r) "("))
         [([a op "(" & r] :seq)] (complexify (concat (list a op) (complexify (conj r "("))))
         [([a "*" & r] :seq)] (let [[b & r] (complexify r)]
                                (complexify (conj r (* a b))))
         [([a "+" b & r] :seq)] (complexify (conj r (+ a b)))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (map complexify)
        (map complexify)
        (map complexify)
        (map complexify)
        (mapcat complexify)
        (apply +))))
