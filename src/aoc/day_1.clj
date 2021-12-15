(ns aoc.day-1
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data 
  (map u/parse-int
       (s/split (slurp "resources/day-1.txt") #"\n")))

(defn part-one
  ([] (part-one data))
  ([input]
  (->> input
       rest
       (map - input)
       (filter neg?)
       count)))

(defn part-two
  ([] (part-two data))
  ([input]
  (let [xs (map + input (rest input) (rest (rest input)))]
    (part-one xs))))
