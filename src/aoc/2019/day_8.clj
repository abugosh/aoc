(ns aoc.2019.day-8
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (->> (s/split (s/trim (slurp "resources/2019/day-8.txt")) #"")
       (map u/parse-int)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (partition (* 25 6))
        (sort-by #(count (filter (partial = 0) %)))
        first
        ((fn [x] (* (count (filter (partial = 1) x)) (count (filter (partial = 2) x))))))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (partition (* 25 6))
        (apply map vector)
        (map #(first (drop-while (partial = 2) %)))
        (partition 25))))
