(ns aoc.2024.day-7
  [:require
   [clojure.string :as s]])

(defn parse-target [line]
  (->> (s/split line #":")
       first
       parse-long))

(defn parse-vals [line]
  (->> (s/split line #" ")
       (drop 1)
       (map parse-long)
       (apply list)))

(def data
  (->> (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map (juxt parse-target parse-vals))))

(defn solvable? [[target eq]]
  (loop [[[a b & ls] & rst] [eq]]
    (cond
      (and (nil? b) (nil? a)) false
      (and (nil? b) (= a target)) true
      (nil? b) (recur rst)
      :else (recur (conj rst
                         (cons (+ a b) ls)
                         (cons (* a b) ls))))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (filter solvable?)
        (map first)
        (apply +))))

(defn fancy-solvable? [[target eq]]
  (loop [[[a b & ls] & rst] [eq]]
    (cond
      (and (nil? b) (nil? a)) false
      (and (nil? b) (= a target)) true
      (nil? b) (recur rst)
      :else (recur (conj rst
                         (cons (+ a b) ls)
                         (cons (* a b) ls)
                         (cons (parse-long (str a b)) ls))))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (filter fancy-solvable?)
        (map first)
        (apply +))))
