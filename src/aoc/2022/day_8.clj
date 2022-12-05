(ns aoc.2020.day-8
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (->> (slurp "resources/2022/day-8.txt")
       s/split-lines
       (map #(s/split % #""))
       (mapv (partial mapv u/parse-int))))

(defn visible-from? [map [yp xp] [y x]]
  (let [hieght (get-in map [y x])]
    (loop [y (+ y yp)
           x (+ x xp)]
      (let [loc (get-in map [y x] :vis)]
        (cond
          (= loc :vis) true
          (>= loc hieght) false
          :else (recur (+ y yp) (+ x xp)))))))

(defn visible-trees [map]
  (for [y (range (count map))
        x (range (count (first map)))
        :when (or (visible-from? map [1 0] [y x])
                  (visible-from? map [0 1] [y x])
                  (visible-from? map [-1 0] [y x])
                  (visible-from? map [0 -1] [y x]))]
    [y x]))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        visible-trees
        count)))

(defn trees-in-view [map [yp xp] [y x]]
  (let [hieght (get-in map [y x])]
    (loop [y (+ y yp)
           x (+ x xp)
           trees 0]
      (let [loc (get-in map [y x] :vis)]
        (cond
          (= loc :vis) trees
          (>= loc hieght) (inc trees)
          :else (recur (+ y yp) (+ x xp) (inc trees)))))))

(defn score-trees [map]
  (for [y (range (count map))
        x (range (count (first map)))
        :let [score (* (trees-in-view map [1 0] [y x])
                       (trees-in-view map [0 1] [y x])
                       (trees-in-view map [-1 0] [y x])
                       (trees-in-view map [0 -1] [y x]))]]
    score))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        score-trees
        flatten
        (apply max))))
