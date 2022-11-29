(ns aoc.2020.day-17
  [:require
   [aoc.utils :as u]
   [clojure.set :as st]
   [clojure.string :as s]])

(defn build-start [lines]
  (for [y (range (count lines))
        x (range (count (first lines)))
        :when (= (get-in lines [y x]) "#")]
    [y x]))

(def data
  (->> (slurp "resources/2020/day-17.txt")
       s/split-lines
       (mapv #(s/split % #""))
       build-start))

(defn points-around [[z y x :as point]]
  (disj (->> (for [z (list (dec z) z (inc z))
                   y (list (dec y) y (inc y))
                   x (list (dec x) x (inc x))]
               [z y x])
             (apply hash-set))
        point))

(defn step [state]
  (let [next-state (->> state
                        (filter (fn [point]
                                  (let [num-neighbors (count (st/intersection state (points-around point)))]
                                    (or (= num-neighbors 2) (= num-neighbors 3))))))
        horizon (st/difference (->> state
                                    (mapcat points-around)
                                    (apply hash-set))
                               state)]
    (->> horizon
         (filter (fn [point] (= 3 (count (st/intersection (points-around point) state)))))
         (concat next-state)
         (apply hash-set))))

(defn part-one
  ([] (part-one data))
  ([input]
   (count (nth (iterate step (apply hash-set (map #(apply vector 0 %) input))) 6))))

(defn hyper-points-around [[a z y x :as point]]
  (disj (->> (for [a (list (dec a) a (inc a))
                   z (list (dec z) z (inc z))
                   y (list (dec y) y (inc y))
                   x (list (dec x) x (inc x))]
               [a z y x])
             (apply hash-set))
        point))

(defn hyper-step [state]
  (let [next-state (->> state
                        (filter (fn [point]
                                  (let [num-neighbors (count (st/intersection state (hyper-points-around point)))]
                                    (or (= num-neighbors 2) (= num-neighbors 3))))))
        horizon (st/difference (->> state
                                    (mapcat hyper-points-around)
                                    (apply hash-set))
                               state)]
    (->> horizon
         (filter (fn [point] (= 3 (count (st/intersection (hyper-points-around point) state)))))
         (concat next-state)
         (apply hash-set))))

(defn part-two
  ([] (part-two data))
  ([input]
   (count (nth (iterate hyper-step (apply hash-set (map #(apply vector 0 0 %) input))) 6))))
