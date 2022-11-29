(ns aoc.2021.day-2
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]
   [clojure.core.logic :as cl]
   [clojure.core.logic.fd :as fd]
   ])

(declare program-params)

(def data (slurp "resources/2021/day-24.txt"))

(def param-table
    [(program-params 1 12 6)
     (program-params 1 10 2)
     (program-params 1 10 13)
     (program-params 26 -6 8)
     (program-params 1 11 13)
     (program-params 26 -12 8)
     (program-params 1 11 3)
     (program-params 1 12 11)
     (program-params 1 12 10)
     (program-params 26 -2 8)
     (program-params 26 -5 14)
     (program-params 26 -4 6)
     (program-params 26 -4 8)
     (program-params 26 -12 2)])

(defn parse-line
  [line]
  (let [[a b c] (s/split line #" ")
        a (keyword a)
        b (keyword b)]
       (cond
         (nil? c) (list a b)
         (re-find #"\d" c) (list a b (u/parse-int c))
         :else (list a b (keyword c)))))

(defn program-data
  [d]
  (->> d
       s/split-lines
       (map parse-line)))

(defn split-input
  [i]
  (map u/parse-int (s/split (str i) #"")))

(defn build-alu
  [input]
  {:w 0 :x 0 :y 0 :z 0 :input input :base-input input})

(defn reg
  [state r]
  (get state r r))

(defn step
  [state [instr a b]]
  (let [a-val (reg state a)
        b-val (reg state b)]
    (case instr 
      :inp (-> state
               (assoc a (first (:input state)))
               (update :input rest))
      :add (update state a + b-val)
      :mul (update state a * b-val)
      :div (update state a quot b-val)
      :mod (update state a mod b-val)
      :eql (assoc state a (if (= a-val b-val) 1 0)))))

(defn run-program
  [program input]
  (reduce step (build-alu input) program))

(def transform
  (fn [input carry d a b]
  (if (= input (+ a (mod carry 26)))
    (quot carry d)
    (+ b input (* (quot carry d) 26)))))

(defn program-params
  [d a b]
  (if (= d 1)
    (fn [input carry] (transform input carry d a b))
    (fn [input carry] (if (= input (+ a (mod carry 26))) (quot carry d) nil))))

(defn keygen
  [params carry]
  (if (empty? (rest params))
    (let [digit-gen (first params)]
      (->> (range 1 10)
           (map (fn [i] [i (digit-gen i carry)]))
           (remove (comp nil? second))
           (filter (comp (partial = 0) second))
           (map #(list (first %)))
           ))
    (let [digit-gen (first params)]
      (->> (range 1 10)
           (map (fn [i] [i (digit-gen i carry)]))
           (remove (comp nil? second))
           (mapcat (fn [[i c]] (map #(conj % i) (keygen (rest params) c))))))))

(defn part-one
  ([] (part-one param-table))
  ([input]
   (->> (keygen input 0)
        (map #(Long/parseLong (apply str %)))
        (apply max))))

(defn part-two
  ([] (part-two param-table))
  ([input]
   (->> (keygen input 0)
        (map #(Long/parseLong (apply str %)))
        (apply min))))
