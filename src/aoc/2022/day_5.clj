(ns aoc.2020.day-5
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(defn parse-stacks [raw-stacks]
  (let [[raw-ids & raw-stacks] (reverse raw-stacks)
        ids (->> raw-ids (re-seq #"\d") (map u/parse-int))]
    (->> raw-stacks
         (map rest)
         (map (partial take-nth 4))
         (apply interleave)
         (partition (count raw-stacks))
         (map (partial remove (partial = \space)))
         (map vec)
         (zipmap ids))))

(defn parse-step [raw-step]
  (let [[_ qnty from to] (re-matches #"move (\d+) from (\d) to (\d)" raw-step)]
    {:qnty (u/parse-int qnty)
     :from (u/parse-int from)
     :to (u/parse-int to)}))

(defn build-state [raw-stacks raw-steps]
  {:stacks (parse-stacks raw-stacks)
   :steps (->> raw-steps (drop 1) (map parse-step))})

(def data
  (->> (slurp "resources/2022/day-5.txt")
       s/split-lines
       (split-with (comp not empty?))
       (apply build-state)))

(defn do-step-9000 [stacks {:keys [qnty from to]}]
  (loop [stacks stacks
         qnty qnty]
    (if (= 0 qnty)
      stacks
      (let [crate (peek (stacks from))]
        (recur (-> stacks
                   (update from pop)
                   (update to conj crate))
               (dec qnty))))))

(defn stack-tops [stacks]
  (->> (keys stacks)
       sort
       (map stacks)
       (map last)))

(defn part-one
  ([] (part-one data))
  ([{stacks :stacks steps :steps}]
   (->> steps
        (reduce do-step-9000 stacks)
        stack-tops
        (apply str))))

(defn do-step-9001 [stacks {:keys [qnty from to]}]
  (let [crates (take-last qnty (stacks from))]
    (-> stacks
        (update from (partial drop-last qnty))
        (update to concat crates))))

(defn part-two
  ([] (part-two data))
  ([{stacks :stacks steps :steps}]
   (->> steps
        (reduce do-step-9001 stacks)
        stack-tops
        (apply str))))
