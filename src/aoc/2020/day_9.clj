(ns aoc.2020.day-9
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (->> (slurp "resources/2020/day-9.txt")
       s/split-lines
       (map #(Long/valueOf %))))

(defn valid-number? [ls num]
  (let [diff (apply hash-set (map #(- num %) ls))]
    (not (empty? (clojure.set/intersection diff (apply hash-set ls))))))

(defn find-invalid [ls]
  (let [preamble (take 25 ls)
        n (nth ls 25)]
    (if (valid-number? ls n)
      (recur (rest ls))
      n)))

(defn take-sum [cipher goal]
  (loop [ls cipher
         weak []
         c 0]
    (cond
      (= c goal) weak
      (> c goal) nil
      :else (recur (rest ls) (conj weak (first ls)) (+ c (first ls))))))

(defn find-weakness [cipher goal]
  (loop [ls cipher]
    (let [weakness (take-sum ls goal)]
      (if (and weakness (>= (count weakness) 2))
        weakness
        (recur (rest ls))))))

(defn part-one
  ([] (part-one data))
  ([input]
   (find-invalid input)))

(defn part-two
  ([] (part-two data))
  ([input]
   (let [weakness (find-weakness (reverse input) (find-invalid input))]
     (+ (apply min weakness) (apply max weakness)))))
