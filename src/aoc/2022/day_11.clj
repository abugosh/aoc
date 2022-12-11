(ns aoc.2020.day-11
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(defn parse-monkey [divisor [id items op tst t f]]
  (let [[_ id] (re-matches #"Monkey (\d+):" id)
        items (re-seq #"\d+" items)
        [_ a op b] (re-matches #"  Operation: new = (old|\d+) ([*|+]) (old|\d+)" op)
        tst (re-find #"\d+" tst)
        t (re-find #"\d+" t)
        f (re-find #"\d+" f)]
    {:id (u/parse-int id)
     :inspects 0
     :items (map parse-long items)
     :test (fn [item] (if (= 0 (mod item (u/parse-int tst))) (u/parse-int t) (u/parse-int f)))
     :op-1 (eval (read-string (str "(fn [old] (int (/ (" op " " a " " b ") 3)))")))
     :op-2 (eval (read-string (str "(fn [old] (mod (" op " " a " " b ") " divisor "))")))}))

(def data
  (let [d (->> (slurp "resources/2022/day-11.txt") s/split-lines)
        divisor (->> d
                     (map (partial re-matches #"  Test: divisible by (\d+)"))
                     (remove nil?)
                     (map second)
                     (map u/parse-int)
                     (apply *))]
    (->> d
         (partition-by empty?)
         (remove #(= 1 (count %)))
         (map (partial parse-monkey divisor))
         (reduce #(assoc %1 (:id %2) %2) []))))

(defn turn [op-key state id]
  (let [{items :items test :test op op-key} (get state id)]
    (as-> items $
      (map op $)
      (reduce #(update-in %1 [(test %2) :items] conj %2) state $)
      (update-in $ [id :inspects] + (count items))
      (assoc-in $ [id :items] (list)))))

(defn round [op-key state]
  (->> state
       (map :id)
       (reduce (partial turn op-key) state)))

(defn monkey-business [state]
  (->> state
       (map :inspects)
       sort
       (take-last 2)
       (apply *)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (iterate (partial round :op-1))
        (drop 20)
        first
        monkey-business)))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (iterate (partial round :op-2))
        (drop 10000)
        first
        monkey-business)))
