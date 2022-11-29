(ns aoc.2021.day-25
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data (slurp "resources/2021/day-25.txt"))

(def sample
"v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>")

(def convert {"v" :v ">" :>})

(defn build-row
  [row]
  (->> (s/split row #"")
       (mapv convert)))

(defn build-floor
  [d]
  (->> d
       s/split-lines
       (mapv build-row)
       vec))

(defn move-east?
  [floor [y x]]
  (if (= (count (first floor)) (inc x))
    (and (= :> (get-in floor [y x])) (nil? (get-in floor [y 0])))
    (and (= :> (get-in floor [y x])) (nil? (get-in floor [y (inc x)])))))

(defn move-east
  [floor [y x]]
  (-> floor
      (assoc-in [y x] nil)
      (assoc-in (if (= (count (first floor)) (inc x)) [y 0] [y (inc x)]) :>)))

(defn step-east
  [floor]
  (->> (for [y (range (count floor))
             x (range (count (first floor)))
             :when (move-east? floor [y x])]
         [y x])
       (reduce move-east floor)))

(defn move-south?
  [floor [y x]]
  (if (= (count floor) (inc y))
    (and (= :v (get-in floor [y x])) (nil? (get-in floor [0 x])))
    (and (= :v (get-in floor [y x])) (nil? (get-in floor [(inc y) x])))))

(defn move-south
  [floor [y x]]
  (-> floor
      (assoc-in [y x] nil)
      (assoc-in (if (= (count floor) (inc y)) [0 x] [(inc y) x]) :v)))

(defn step-south
  [floor]
  (->> (for [y (range (count floor))
             x (range (count (first floor)))
             :when (move-south? floor [y x])]
         [y x])
       (reduce move-south floor)))

(defn step
  [floor]
  (->> floor step-east step-south))

(defn count-steps
  [floor]
  (loop [floor floor
         index 1]
    (let [next-floor (step floor)]
      (if (= floor next-floor)
        index
        (recur next-floor (inc index))))))

(defn part-one
  ([] (part-one (build-floor data)))
  ([input]
   (count-steps input)))

(defn part-two
  ([] (part-two data))
  ([input]
   nil))

;(time (part-one (build-floor sample)))
(time (part-one))
