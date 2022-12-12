(ns aoc.2020.day-14
  [:require
   [aoc.utils :as u]
   [clojure.set :as set]
   [clojure.string :as s]])

(defn parse-line [line]
  (as-> line $
    (re-seq #"(\d+),(\d+)" $)
    (map (partial drop 1) $)
    (map (partial map u/parse-int) $)
    (map #(list %1 %2) $ (drop 1 $))))

(defn line-range [f a b]
  (->> (range (min a b) (inc (max a b))) (map f)))

(defn build-line [[[xs ys] [xe ye]]]
  (if (= xs xe)
    (line-range #(list xs %) ys ye)
    (line-range #(list % ys) xs xe)))

(def data
  (->> (slurp "resources/2022/day-14.txt")
       s/split-lines
       (mapcat parse-line)
       (mapcat build-line)
       (into #{})))

(defn next-sand [cave]
  (let [void-level (->> cave (map second) (apply max))]
    (loop [[x y :as s-loc] [500 0]]
      (if (= y void-level)
        nil
        (if-let [fall-loc (some #(and (nil? (cave %)) %) [(list x (inc y)) (list (dec x) (inc y)) (list (inc x) (inc y))])]
          (recur fall-loc)
          s-loc)))))

(defn fill-cave [cave]
  (loop [cave cave]
    (let [next-loc (next-sand cave)]
      (cond
        (= nil next-loc) cave
        (= (list 500 0) next-loc) (conj cave next-loc)
        :else (recur (conj cave next-loc))))))

(defn part-one
  ([] (part-one data))
  ([input]
   (count (set/difference (fill-cave input) input))))

(defn add-base [cave]
  (let [base-level (+ 2 (->> cave (map second) (apply max)))
        max-x (+ base-level (->> cave (map first) (apply max)))
        min-x (- base-level (->> cave (map first) (apply min)))]
    (into cave (build-line [[min-x base-level] [max-x base-level]]))))

(defn part-two
  ([] (part-two data))
  ([input]
   (let [cave (add-base input)]
     (count (set/difference (fill-cave cave) cave)))))
