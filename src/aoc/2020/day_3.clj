(ns aoc.2020.day-3
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (->> (slurp "resources/2020/day-3.txt")
       (s/split-lines)
       (map #(s/split % #""))
       (map cycle)))

(defn trees-on-slope [input right down]
  (->> input
       (take-nth down)
       (map-indexed (fn [i ls] (nth ls (* i right))))
       (drop 1)
       (filter #(= % "#"))
       count))

(defn part-one
  ([] (part-one data))
  ([input]
   (trees-on-slope input 3 1)))

(defn part-two
  ([] (part-two data))
  ([input]
   (* (trees-on-slope input 1 1)
      (trees-on-slope input 3 1)
      (trees-on-slope input 5 1)
      (trees-on-slope input 7 1)
      (trees-on-slope input 1 2))))
