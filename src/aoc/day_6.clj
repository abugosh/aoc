(ns aoc.day-6
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data 
  (map u/parse-int (clojure.string/split (clojure.string/trim (slurp "resources/day-6.txt")) #",")))

(defn day
  [xs]
  (flatten (map (fn [fish]
                  (if (= 0 fish)
                    [6 8]
                    (dec fish)))
                xs)))

(defn loop-days
  [days f xs]
  (loop [i 1
         fish xs]
    (if (> i days)
      fish
      (recur (inc i) (f fish)))))

(defn part-one
  ([] (part-one 80 data))
  ([days input]
   (count (loop-days days day input))))

(defn build-state
  [xs]
  (->> xs
      (group-by identity)
      (reduce-kv #(assoc %1 %2 (count %3)) (reduce #(assoc %1 %2 0) {} (range 9)))))

(defn update-state
  [state]
  (-> {}
      (assoc 0 (get state 1))
      (assoc 1 (get state 2))
      (assoc 2 (get state 3))
      (assoc 3 (get state 4))
      (assoc 4 (get state 5))
      (assoc 5 (get state 6))
      (assoc 6 (+ (get state 0 0) (get state 7 0)))
      (assoc 7 (get state 8))
      (assoc 8 (get state 0))))

(defn part-two
  ([] (part-two 256 data))
  ([days input]
   (->> input
        build-state
        (loop-days days update-state)
        vals
        (remove nil?)
        (apply +))))
