(ns aoc.2020.day-3
  [:require
   [aoc.utils :as u]
   [clojure.set :as st]
   [clojure.string :as s]])

(def data
  (->> (slurp "resources/2022/day-3.txt")
       s/split-lines
       (map (fn [line]
              [(apply hash-set (map int (subs line 0 (/ (count line) 2))))
               (apply hash-set (map int (subs line (/ (count line) 2))))]))))

(defn priority [c]
  (if (>= c (int \a))
    (inc (- c (int \a)))
    (+ 27 (- c (int \A)))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map #(apply st/intersection %))
        (map first)
        (map priority)
        (apply +))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (map #(apply st/union %))
        (partition 3)
        (map #(apply st/intersection %))
        (map first)
        (map priority)
        (apply +))))
