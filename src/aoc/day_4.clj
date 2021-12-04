(ns aoc.day-4
  [:require [clojure.string :as s]
            [clojure.core.match :refer [match]]])

(def guesses
  (map #(Integer/parseInt %1) (s/split (first (s/split-lines (slurp "resources/day-4.txt"))) #",")))

(def boards
  (->> (s/split-lines (slurp "resources/day-4.txt"))
       (drop 1)
       (remove empty?)
       (map #(map (fn [x] (Integer/parseInt x)) (s/split (s/trim %1) #"\s+")))
       (partition 5)
       (map (fn [x]
              {:rows (map set x)
               :cols (->> (range 5)
                          (map (fn [y] (map #(nth %1 y) x)))
                          (map set))}))))

(defn winner?
  [nums board]
  (->> (concat (:rows board) (:cols board))
       (map #(apply disj %1 nums))
       (some empty?)))

(defn score
  [nums board]
  (* (first (reverse nums))
     (->> (:cols board)
          (map #(apply disj %1 nums))
          (reduce concat)
          (apply +))))

(defn part-one
  ([] (part-one guesses boards))
  ([nums b]
   (loop [i 5]
     (let [cur (take i nums)
           winner (first (filter (partial winner? cur) b))]
       (if (nil? winner)
         (recur (inc i))
         (score cur winner))))))

(defn part-two
  ([] (part-two guesses boards))
  ([nums b]
   (loop [i 5
          bs b]
     (let [cur (take i nums)]
       (if (= 1 (count bs))
         (part-one nums bs)
         (recur (inc i) (remove (partial winner? cur) bs)))))))

(part-one)
(part-two)
