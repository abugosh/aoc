(ns aoc.2020.day-1
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (->> (slurp "resources/2022/day-1.txt")
       s/split-lines
       (partition-by #(= "" %))
       (remove #(= (list "") %))
       (map #(map u/parse-int %))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map #(apply + %))
        (apply max))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (map #(apply + %))
        sort
        (take-last 3)
        (apply +))))
