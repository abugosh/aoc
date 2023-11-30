(ns aoc.2019.day-3
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.set :as set]])

(defn parse-path [path]
  (->> path
       (re-seq #"(\w)(\d+)")
       (map rest)
       (map (fn [[dir dist]] [(keyword dir) (u/parse-int dist)]))))

(def data
  (->> (slurp "resources/2019/day-3.txt")
       s/split-lines
       (map parse-path)))

(defn move [[y x] dir]
  (case dir
    :U [(inc y) x]
    :D [(dec y) x]
    :R [y (inc x)]
    :L [y (dec x)]))

(defn render-segment [start [dir dist]]
  (loop [start start
         dist dist
         line []]
    (if (= dist 0)
      line
      (let [nxt (move start dir)]
        (recur nxt (dec dist) (conj line nxt))))))

(defn render-line [segments]
  (loop [spot [0 0]
         [segment & segments] segments
         line []]
    (if (nil? segment)
      line
      (let [segment (render-segment spot segment)]
        (recur (last segment) segments (concat line segment))))))

(defn manhatten-dist [[y1 x1] [y2 x2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn intersects [line1 line2]
  (set/intersection (set line1) (set line2)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map render-line)
        (apply intersects)
        (map #(manhatten-dist [0 0] %))
        (apply min))))

(defn part-two
  ([] (part-two data))
  ([[l1 l2]]
   (let [l1 (render-line l1)
         l2 (render-line l2)
         pnts (intersects l1 l2)]
     (->> pnts
          (map #(+ (.indexOf l1 %) (.indexOf l2 %) 2))
          (apply min)))))
