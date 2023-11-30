(ns aoc.2019.day-10
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.set :as set]
   [clojure.math :as math]])

(def data
  (->> (slurp "resources/2019/day-10.txt")
       s/split-lines
       (map #(s/split % #""))
       (map-indexed (fn [y row] (keep-indexed (fn [x a] (when (= "#" a) [y x])) row)))
       (apply concat)
       set))

(defn gcd [a b]
  (int (.gcd (biginteger a) (biginteger b))))

(defn find-angle [[start-y start-x] [point-y point-x]]
  (let [y (- start-y point-y)
        x (- start-x point-x)
        d (gcd y x)]
    [(- (/ y d)) (- (/ x d))]))

(defn max-x [board]
  (->> board
       (map last)
       (apply max)))

(defn max-y [board]
  (->> board
       (map first)
       (apply max)))

(defn line [mx my [py px] [ay ax]]
  (loop [y py
         x px
         l #{}]
    (if (or (< y 0) (< x 0) (> y my) (> x mx))
      l
      (recur (+ y ay) (+ x ax) (conj l [y x])))))

(defn los [board pnt]
  (let [mx (max-x board)
        my (max-y board)]
    (loop [i 0
           board (disj board pnt)]
      (if (empty? board)
        i
        (recur (inc i) (set/difference board (line mx my pnt (find-angle pnt (first board)))))))))

(defn station-location [board]
  (->> board
       (map (juxt (partial los board) identity))
       (sort-by first)
       last
       last))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        station-location
        (los input))))

(defn angle-degrees [[y x]]
  (+ 90 (if (neg? x)
          (+ 180 (math/to-degrees (* 2 (math/atan (/ (- y) (+ (- x) (math/hypot (- x) (- y))))))))
          (math/to-degrees (* 2 (math/atan (/ y (+ x (math/hypot x y)))))))))

(defn manhatten-dist [[y1 x1] [y2 x2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn laser-order [astroids]
  (loop [astroids astroids
         nuked []]
    (let [vapor (map first astroids)]
      (if (empty? astroids)
        nuked
        (recur (->> astroids (map rest) (filter (complement empty?))) (concat nuked vapor))))))

(defn part-two
  ([] (part-two data))
  ([input]
   (let [base (station-location input)]
     (->> (disj input base)
          (map (juxt #(angle-degrees (find-angle base %)) (partial manhatten-dist base) identity))
          (group-by first)
          (sort-by first)
          (map last)
          (map (partial sort-by second))
          (map (partial map last))
          laser-order
          (#(nth % 199))
          ((fn [[y x]] (+ y (* x 100))))))))
