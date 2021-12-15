(ns aoc.day-5
  [:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.core.match :refer [match]]])

(defn make-point
  [x y]
  {:x x :y y})

(def data 
  (->> (s/split (slurp "resources/day-5.txt") #"\n")
       (map #(map u/parse-int (drop 1 (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" %1))))
       (map (fn [[x1 y1 x2 y2]] {:start (make-point x1 y1) :end (make-point x2 y2)}))))

(defn vert?
  [{{y1 :y} :start {y2 :y} :end}]
  (= y1 y2))

(defn horiz?
  [{{x1 :x} :start {x2 :x} :end}]
  (= x1 x2))

(defn vert-line
  [{{y :y x1 :x} :start {x2 :x} :end}]
  (map make-point (range (min x1 x2) (inc (max x2 x1))) (repeat y)))

(defn horiz-line
  [{{y1 :y x :x} :start {y2 :y} :end}]
  (map make-point (repeat x) (range (min y1 y2) (inc (max y2 y1)))))

(defn diag-line
  [{{y1 :y x1 :x} :start {y2 :y x2 :x :as end} :end}]
  (cons end
        (map make-point
             (range x1 x2 (if (> x1 x2) -1 1))
             (range y1 y2 (if (> y1 y2) -1 1)))))

(defn line->points
  [line]
  (cond
    (vert? line) (vert-line line)
    (horiz? line) (horiz-line line)
    :else (diag-line line)))

(defn count-overlap
  [lines]
  (->> lines
       (map line->points)
       flatten
       frequencies
       vals
       (filter (partial < 1))
       count))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (filter #(or (horiz? %1) (vert? %1)))
        count-overlap)))

(defn part-two
  ([] (part-two data))
  ([input]
   (count-overlap input)))
