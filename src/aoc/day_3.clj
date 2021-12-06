(ns aoc.day-3
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data 
  (->> (s/split (slurp "resources/day-3.txt") #"\n")
       (map #(->> (s/split %1 #"")
                  (map u/parse-int)))))

(defn bin->int
  [bin]
  (Integer/parseInt (apply str bin) 2))

(defn freqs
  [input]
  (reduce #(map + %1 %2) input))

(defn common
  [f xs]
  (let [half (/ (count xs) 2)]
    (map #(if (f half %1) 1 0) (freqs xs))))

(defn part-one
  ([] (part-one data))
  ([input]
   (* (bin->int (common > input))
      (bin->int (common < input)))))

(defn find-root
  ([f xs] (find-root f xs []))
  ([f xs base]
   (let [cur (first (common f xs))
         poss (filter #(= (first %1) cur) xs)]
     (cond
       (empty? poss) nil
       (= 1 (count poss)) (concat base (first poss))
       :else (recur f (map rest poss) (conj base cur))))))

(defn part-two
  ([] (part-two data))
  ([input]
   (* (bin->int (find-root > input))
      (bin->int (find-root <= input)))))

(part-one)
(part-two)
