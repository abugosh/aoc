(ns aoc.2020.day-15
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(defn parse-start [line]
  (->> (s/split line #",")
       (map u/parse-int)))

(def data
  (->> (slurp "resources/2020/day-15.txt")
       s/trim-newline
       parse-start))

(defn next-number [[x & ls]]
  (let [turns (take-while #(not= % x) ls)
        dist (count turns)]
    (if (= dist (count ls))
      0
      (inc dist))))

(defn play [numbers]
  (conj numbers (next-number numbers)))

(defn number-for [turn base]
  (->> base
       (iterate play)
       (some #(and (= turn (count %)) %))
       first))

(defn part-one
  ([] (part-one data))
  ([input]
   (number-for 2020 (reverse input))))

(defn build-state [numbers]
  (let [state (->> numbers
                   (map-indexed #(list %2 %1))
                   flatten
                   (apply hash-map))]
    (-> state
        (assoc :next (next-number (reverse numbers)))
        (assoc :turn (count numbers)))))

(defn step [{nxt :next turn :turn :as state}]
  (if (state nxt)
    (-> state
        (assoc :next (- turn (state nxt)))
        (assoc nxt turn)
        (update :turn inc))
    (-> state
        (assoc :next 0)
        (assoc nxt turn)
        (update :turn inc))))

(defn step-to [turn base]
  (some #(and (= (:turn %) turn) %) (iterate step (build-state base))))

(defn part-two
  ([] (part-two data))
  ([input]
   (:next (step-to (dec 30000000) input))))
