(ns aoc.2021.day-18
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data (slurp "resources/2021/day-18.txt"))

(defn build-data
  [d]
  (->> d
       s/split-lines
       (map (comp eval read-string))))

(defn add-right
  [tree value]
  (cond 
    (nil? value) tree
    (number? tree) (+ tree value)
    :else [(first tree) (add-right (second tree) value)]))

(defn add-left
  [tree value]
  (cond 
    (nil? value) tree
    (number? tree) (+ tree value)
    :else [(add-left (first tree) value) (second tree)]))

(def explode
  (memoize
    (fn ([tree] (:tree (explode tree 0)))
      ([tree depth]
       (cond
         (number? tree) {:tree tree}
         (= depth 3) (match [tree]
                            [[[a b] x]] (if (number? x) 
                                          {:tree [0 (+ b x)] :left a}
                                          {:tree [(+ b (first x)) 0] :left a :right (second x)})
                            [[x [a b]]] {:tree [(+ x a) 0] :right b}
                            :else {:tree tree})
         :else (let [left (explode (first tree) (inc depth)) 
                     t (cond-> [(:tree left) (second tree)]
                         (number? (second tree)) (add-right (:right left))
                         (coll?   (second tree)) (update 1 add-left (:right left)))
                     right (explode (second t) (inc depth))]
                 {:tree (cond-> [(:tree left) (:tree right)]
                          (number? (:tree left))  (add-left (:left right))
                          (coll?   (:tree left))  (update 0 add-right (:left right)))
                  :left (:left left)
                  :right (:right right)}))))))

(def split
  (memoize (fn [tree]
             (cond
               (coll? tree) (if (= (first tree) (split (first tree)))
                              [(first tree) (split (second tree))]
                              [(split (first tree)) (second tree)])
               (< tree 10) tree
               :else [(quot tree 2) (+ (rem tree 2) (quot tree 2))]))))

(defn reduce-tree
  [tree]
  (if (= tree (explode tree))
    (if (= tree (split tree))
      tree
      (recur (split tree)))
    (recur (explode tree))))

(defn add
  [s1 s2]
  (reduce-tree [s1 s2]))

(defn magnitude
  [tree]
  (if (number? tree)
    tree
    (+ (* 3 (magnitude (first tree))) (* 2 (magnitude (second tree))))))

(defn part-one
  ([] (part-one (build-data data)))
  ([input]
   (magnitude (reduce add input))))

(defn part-two
  ([] (part-two (build-data data)))
  ([input]
   (apply max (for [x input
                    y input
                    :when (not= x y)]
                (magnitude (add x y))))))
