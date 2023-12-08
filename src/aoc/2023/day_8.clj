(ns aoc.2020.day-8
  [:require
   [clojure.string :as s]
   [clojure.math.numeric-tower :as nt]])

(defn parse-maps [[instructions _ & network]]
  (let [instructions (s/split instructions #"")
        network (->> network
                     (map (partial re-seq #"\w\w\w"))
                     (map (juxt first (comp vec rest)))
                     (into {}))]
    {:instructions (map {"L" 0 "R" 1} instructions) :network network}))

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       parse-maps))

(defn count-steps [{:keys [:instructions :network]} destination? start]
  (loop [loc start
         [ins & rest-ins] (cycle instructions)
         steps 0]
    (if (destination? loc)
      steps
      (recur (get-in network [loc ins]) rest-ins (inc steps)))))

(defn part-one
  ([] (part-one data))
  ([input]
   (count-steps input (partial = "ZZZ") "AAA")))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        :network
        keys
        (filter #(s/ends-with? % "A"))
        (map (partial count-steps input #(s/ends-with? % "Z")))
        (reduce nt/lcm))))
