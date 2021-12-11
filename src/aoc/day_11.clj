(ns aoc.day-11
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data 
  (->> (s/split-lines (slurp "resources/day-11.txt"))
       (mapv #(->> (s/split %1 #"")
                  (mapv u/parse-int)))))

(defn build-state
  [cave]
  {:cave cave
   :flashes 0})

(defn inc-cave
  [cave]
  (mapv #(mapv inc %1) cave))

(defn points-surrounding
  [cave [y x]]
  (for [xp (range (dec x) (+ x 2))
        yp (range (dec y) (+ y 2))
        :when (and (>= xp 0)
                    (>= yp 0)
                    (< xp (count (first cave)))
                    (< yp (count cave)))]
    [yp xp]))

(defn flash-points
  [cave]
  (for [y (range (count cave))
        x (range (count (first cave)))
        :when (> (get-in cave [y x]) 9)]
    [y x]))

(defn flash
  [cave point]
  (assoc-in
    (reduce #(update-in %1 %2 inc) cave (points-surrounding cave point))
    point -100000))

(defn flash-cave
  [cave]
  (loop [c cave]
    (let [points (flash-points c)]
      (if (empty? points)
        c
        (recur (reduce flash c points))))))

(defn clean-cave
  [cave]
  (mapv (fn [xs] (mapv #(if (neg? %1) 0 %1) xs)) cave))

(defn update-state
  [{:keys [cave flashes]}]
  (let [new-cave (->> cave inc-cave flash-cave)]
    {:cave (clean-cave new-cave)
     :flashes (->> new-cave flatten (filter neg?) count (+ flashes))}))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> (range 100)
        (reduce (fn [acc _] (update-state acc)) (build-state input))
        :flashes)))

(defn all-flash?
  [cave]
  (every? #(= 0 %1) (flatten cave)))

(defn find-all-flash
  [cave]
  (loop [i 0
         c cave]
    (if (all-flash? c)
      i
      (recur (inc i) (->> c inc-cave flash-cave clean-cave)))))

(defn part-two
  ([] (part-two data))
  ([input]
   (find-all-flash input)))

(part-one)
(part-two)
