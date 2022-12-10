(ns aoc.2020.day-10
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(defn parse-line [line]
  (if (= line "noop")
    {:cmd :noop
     :val 0
     :len 1}
    (let [[_ v] (s/split line #" ")]
      {:cmd :addx
       :len 2
       :val (u/parse-int v)})))

(def data
  (->> (slurp "resources/2022/day-10.txt")
       s/split-lines
       (map parse-line)))

(defn cycle-reg [cmds c]
  (loop [cycle 1
         [{len :len val :val} & r] cmds
         reg 1]
    (if (< c (+ cycle len))
      reg
      (recur (+ cycle len) r (+ reg val)))))

(defn cycle-strength [cmds c]
  (* c (cycle-reg cmds c)))

(defn part-one
  ([] (part-one data))
  ([input]
     (apply + (->> [20 60 100 140 180 220] (map (partial cycle-strength input))))))

(defn pixel-for [cmds c]
  (let [i (mod c 40)
        i (dec (if (= i 0) 40 i))]
    (if (#{(dec i) i (inc i)} (cycle-reg cmds c))
      "#"
      ".")))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> (range 1 241)
        (map (partial pixel-for input))
        (partition 40)
        (map (partial apply str))
        (cons ""))))
