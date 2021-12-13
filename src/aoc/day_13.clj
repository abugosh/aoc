(ns aoc.day-13
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data 
  (s/split-lines (slurp "resources/day-13.txt")))

(defn data-paper
  [d]
  (->> d
       (take-while (complement empty?))
       (map #(s/split %1 #","))
       (map #(map u/parse-int %1))))

(defn data-commands
  [d]
  (->> d
       (drop-while (complement empty?))
       (drop 1)
       (map #(re-seq #"\w=\d+" %1))
       flatten
       (map #(s/split %1 #"="))
       (map (fn [[dir line]]
              [dir (u/parse-int line)]))))

(defn fold-calc
  [line n]
  (- line (- n line)))

(defn fold-x
  [paper line]
  (->> paper
       (map (fn [[x y]]
              (if (> x line)
              [(fold-calc line x) y]
              [x y])))))

(defn fold-y
  [paper line]
  (->> paper
       (map (fn [[x y]]
              (if (> y line)
              [x (fold-calc line y)]
              [x y])))))

(defn fold
  [paper [dir line]]
  (distinct 
    (if (= dir "y")
      (fold-y paper line)
      (fold-x paper line))))

(defn part-one
  ([] (part-one (data-paper data) (data-commands data)))
  ([paper commands]
   (count (fold paper (first commands)))))

(defn fix-paper
  [paper]
  (let [min-x (->> paper (map first) (apply min))
        min-y (->> paper (map second) (apply min))
        x-diff (if (neg? min-x) (- min-x) 0)
        y-diff (if (neg? min-y) (- min-y) 0)]
    (map (fn [[x y]] [(+ x x-diff) (+ y y-diff)]) paper)))

(defn build-canvas
  [paper]
  (let [max-x (->> paper (map first) (apply max))
        max-y (->> paper (map second) (apply max))]
  (vec (repeat (inc max-y) (vec (repeat (inc max-x) "."))))))

(defn etch-canvas
  [paper]
  (->> paper
       (reduce (fn [acc [x y]]
                 (assoc-in acc [y x] "#"))
               (build-canvas paper))))

(defn part-two
  ([] (part-two (data-paper data) (data-commands data)))
  ([paper commands]
   (->> commands
        (reduce #(fold %1 %2) paper)
        fix-paper
        etch-canvas
        (map #(apply str %1))
        (interpose "\n")
        (apply str))))

(part-one)
(part-two)
