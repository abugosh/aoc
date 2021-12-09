(ns aoc.day-7
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data
  (map u/parse-int (s/split (s/trim (slurp "resources/day-7.txt")) #",")))

(defn flat-cost
  [start end]
  (Math/abs (- start end)))

(defn cost->list
  [point f xs]
  (->> xs
       (pmap (partial f point))
       (apply +)))

(defn find-cost
  [f crabs]
  (->> (range 0 (last (sort crabs)))
       (pmap #(cost->list %1 f crabs))
       sort
       first))

(defn part-one
  ([] (part-one data))
  ([input]
   (find-cost flat-cost input)))

(def exp-cost-memo (memoize (fn [x] (apply + (range 1 (inc x))))))

(defn exp-cost
  [start end] 
  (exp-cost-memo (flat-cost start end)))

(defn part-two
  ([] (part-two data))
  ([input]
   (find-cost exp-cost input)))

(part-one)
(part-two)
