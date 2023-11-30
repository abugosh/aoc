(ns aoc.2019.day-12
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.math.numeric-tower :as math]])

(def data
  (->> (slurp "resources/2019/day-12.txt")
       s/split-lines
       (map (partial re-matches #"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>"))
       (map rest)
       (map (partial map parse-long))))

(defn build-moon [x y z]
  {:pos [x y z] :vel [0 0 0]})

(defn apply-gravity [{pos :pos :as moon-1} {other-pos :pos}]
  (update moon-1 :vel (partial map +) (map compare other-pos pos)))

(defn apply-velocity [{vel :vel :as moon}]
  (update moon :pos (partial map +) vel))

(defn step [moons]
  (->> moons
       (map #(reduce apply-gravity % moons))
       (map apply-velocity)))

(defn moon-energy [{:keys [:pos :vel]}]
  (* (apply + (map abs pos))
     (apply + (map abs vel))))

(defn total-energy [moons]
  (apply + (map moon-energy moons)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map (partial apply build-moon))
        (iterate step)
        (drop 1000)
        first
        total-energy)))

(defn moving? [moons]
  (->> moons
       (map :vel)
       flatten
       (some (partial not= 0))))

(defn axis-intervals [moons]
  (let [moons (map #(assoc % :vel [0]) moons)]
    (for [i (range (count (:pos (first moons))))
          :let [moons (map #(assoc % :pos [(nth (:pos %) i)]) moons)]]
      (->> moons
           (iterate step)
           (drop 1)
           (take-while moving?)
           count
           inc
           (* 2)))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (map (partial apply build-moon))
        axis-intervals
        (reduce math/lcm))))
