(ns aoc.day-17
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.math.numeric-tower :as math]
   [clojure.core.match :refer [match]]])

(def data 
  (->> (slurp "resources/day-17.txt")
       (re-seq #"[-\d]+")
       (map u/parse-int)
       ((fn [[x1 x2 y1 y2]]
          {:x-low (min x1 x2)
           :x-high (max x1 x2)
           :y-low (min y1 y2)
           :y-high (max y1 y2)}))))

(def rat-int? (comp integer? rationalize))

(defn quadratic
  [dist]
  (let [dist (math/abs dist)]
    (max (+ (/ (- 1) 2) (Math/sqrt (- (/ 1 4) (* 4 (/ 1 2) (- dist)))))
         (- (/ (- 1) 2) (Math/sqrt (- (/ 1 4) (* 4 (/ 1 2) (- dist))))))))

(defn apex
  [x]
  (* (/ x 2) (inc x)))

(defn target-range
  [a b]
  (range a (inc b)))

(defn intersect-speed
  [low high vel]
  (->> (target-range low high)
       (map - (repeat (apex vel)))
       (map quadratic)
       (filter rat-int?)
       first))

(defn y-intersect?
  [low high vel]
  (->> (target-range low high)
       (map - (repeat (apex vel)))
       (map quadratic)
       (some rat-int?)
       boolean))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> (range 1000)
        (filter #(y-intersect? (:y-low input) (:y-high input) %1))
        (apply max)
        apex)))

(defn step-x
  [x]
  (- x (compare x 0)))

(defn in-target?
  [{:keys [y-low y-high x-low x-high] :as target} x-vel y-vel x y]
  (cond 
    (or (> x x-high) (< y y-low)) false
    (and (>= x x-low) (<= y y-high)) true
    :else (recur target (step-x x-vel) (dec y-vel) (+ x x-vel) (+ y y-vel))))

(defn find-intersects
  [{:keys [y-low y-high x-low x-high] :as target} max-vel]
  (for [x (->> (range max-vel) (filter #(y-intersect? x-low x-high %1)))
        y (->> (range (- max-vel) max-vel) (filter #(y-intersect? y-low y-high %1)))
        :when (in-target? target x y 0 0)]
    [x y]))

(defn part-two
  ([] (part-two data))
  ([input]
   (count (find-intersects input 500))))
