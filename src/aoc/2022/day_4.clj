(ns aoc.2020.day-4
  [:require
   [aoc.utils :as u]
   [clojure.set :as st]
   [clojure.string :as s]])

(def data
  (->> (slurp "resources/2022/day-4.txt")
       s/split-lines
       (map (partial re-matches #"(\d+)-(\d+),(\d+)-(\d+)"))
       (map (partial drop 1))
       (map (partial map u/parse-int))
       (map (partial split-at 2))))

(defn range-contains? [[s1 e1] [s2 e2]]
  (or (and (>= s2 s1) (<= e2 e1)) (and (>= s1 s2) (<= e1 e2))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (filter (partial apply range-contains?))
        count)))


(defn range-overlap? [[s1 e1] [s2 e2]]
  (->> (st/intersection (->> (range s1 (inc e1)) (apply hash-set))
                        (->> (range s2 (inc e2)) (apply hash-set)))
       count
       pos?))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (filter (partial apply range-overlap?))
        count)))
