(ns aoc.day-12
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data 
  (let [paths (->> (s/split-lines (slurp "resources/day-12.txt"))
       (map #(s/split %1 #"-"))
       (mapcat (fn [x] [x (reverse x)])))
        graph (->> paths
         (map first)
         (reduce #(assoc %1 %2 '()) {}))]
    (reduce #(update %1 (first %2) conj (second %2)) graph paths)))

(defn big-cave?
  [cave]
  (= cave (s/upper-case cave)))

(def small-cave? (complement big-cave?))

(defn to-visit
  [visited caves]
  (distinct (concat (filter big-cave? caves)
                    (apply disj (set caves) visited))))

(defn find-paths
  [graph visited cave]
  (let [v (conj visited cave)]
    (cond
      (= cave "end") (list (list "end"))
      (empty? (to-visit v (graph cave))) nil
      :else (->> (graph cave)
                 (to-visit v)
                 (mapcat #(find-paths graph v %1))
                 (remove nil?)
                 (map #(cons cave %1))))))

(defn part-one
  ([] (part-one data))
  ([input]
   (count (find-paths input #{} "start"))))

(defn find-paths-2
  [graph visited cave]
  (let [v (conj visited cave)]
    (cond
      (= cave "end") (list (list "end"))
      (empty? (to-visit v (graph cave))) nil
      :else (->> (graph cave)
                 (to-visit v)
                 ((fn [visitable] 
                    (concat (mapcat #(find-paths graph visited %1) visitable)
                            (mapcat #(find-paths-2 graph v %1) visitable))))
                 (remove nil?)
                 (map #(cons cave %1))
                 distinct))))

(defn part-two
  ([] (part-two data))
  ([input]
   (count (find-paths-2 input #{"start"} "start"))))
