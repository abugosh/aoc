(ns aoc.2020.day-5
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(defn parse-data [line]
  (let [row (subs line 0 7)
        col (subs line 7)]
    {:row (Integer/parseInt (s/replace row #"F|B" {"F" "0" "B" "1"}) 2)
     :col (Integer/parseInt (s/replace col #"R|L" {"R" "1" "L" "0"}) 2)}))

(def data
  (->> (slurp "resources/2020/day-5.txt")
       s/split-lines
       (map parse-data)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map #(+ (* 8 (:row %)) (:col %)))
        (apply max))))

(defn find-seat [seats]
  (let [start (apply min seats)
        end (apply max seats)]
    (first (clojure.set/difference (set (range start end)) (set seats)))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (map #(+ (* 8 (:row %)) (:col %)))
        find-seat)))
