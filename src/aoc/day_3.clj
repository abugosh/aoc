(ns aoc.day-3
  [:require [clojure.string :as s]
            [clojure.core.match :refer [match]]])

(def data 
  (map (fn [x]
         x)
       (s/split (slurp "resources/day-3.txt") #"\n")))

(defn part-one
  ([] (part-one data))
  ([input]
   input))

(defn part-two
  ([] (part-two data))
  ([input]
   input))

(part-one)
(part-two)
