(ns aoc.day-3
  [:require [clojure.string :as s]
            [clojure.core.match :refer [match]]])

(def data 
  (->> (s/split (slurp "resources/day-3.txt") #"\n")
       (map (fn [x]
              (->> (s/split x #"")
                   (map #(Integer/parseInt %1)))))))

(defn bin->int
  [bin]
  (Integer/parseInt (apply str bin) 2))

(defn freqs
  [input]
  (reduce (fn [acc x] 
            (map + acc x))
          input))

(defn common
  [f xs]
  (let [half (/ (count xs) 2)]
    (map #(if (f half %1) "1" "0") (freqs xs))))

(defn part-one
  ([] (part-one data))
  ([input]
   (* (bin->int (common > input))
      (bin->int (common < input)))))

(defn find-root
  [f xs]
  (let [cur (Integer/parseInt (first (common f xs)))
        poss (filter #(= (first %1) cur) xs)]
    (cond
      (empty? poss) nil
      (= 1 (count poss)) (first poss)
      :else (cons cur (find-root f (map rest poss))))))

(defn part-two
  ([] (part-two data))
  ([input]
   (* (bin->int (find-root > input))
      (bin->int (find-root <= input)))))

(part-one)
(part-two)
