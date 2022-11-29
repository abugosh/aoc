(ns aoc.2021.day-21
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data 
  (->> (s/split-lines (slurp "resources/2021/day-21.txt"))
       (map #(re-seq #"\d" %))
       (map second)
       (map u/parse-int)))

(defn build-die
  []
  {:rolls 0
   :die (cycle (range 1 101))})

(defn roll-die
  [die times]
  [(-> die (update :rolls + times) (update :die (partial drop times))) (take times (:die die))])

(defn build-player
  [pos]
  {:score 0
   :track (drop (dec pos) (cycle (range 1 11)))})

(defn pos-player
  [{track :track}]
  (first track))

(defn move-player
  [{track :track :as player} roll]
  (-> player
      (update :track (partial drop roll))
      (update :score + (first (drop roll track)))))

(defn winner?
  [{score :score}]
  (>= score 1000))

(defn game-score
  [{score :score} {rolls :rolls}]
  (* score rolls))

(defn game
  [p1 p2]
  (loop [main p1
         alt p2
         die (build-die)]
    (let [[die rolls] (roll-die die 3)
          main (move-player main (apply + rolls))]
      (if (winner? main)
        (game-score alt die)
        (recur alt main die)))))

(defn part-one
  ([] (part-one data))
  ([input]
   (game (build-player (first input)) (build-player (second input)))))

(defn move-score
  [start roll]
  (if (= 10 (+ start roll)) 10 (mod (+ start roll) 10)))

(defn play-score
  [[score locations]]
  (->> locations
       (mapcat (fn [[loc n]] [[(move-score loc 3) (* 1 n)]
                              [(move-score loc 4) (* 3 n)]
                              [(move-score loc 5) (* 6 n)]
                              [(move-score loc 6) (* 7 n)]
                              [(move-score loc 7) (* 6 n)]
                              [(move-score loc 8) (* 3 n)]
                              [(move-score loc 9) (* 1 n)]]))
       (map (fn [[loc n]] {(+ score loc) {loc n}}))
       (apply merge-with (partial merge-with +))))

(defn step
  [state]
  (->> state
       (pmap play-score)
       (apply merge-with (partial merge-with +))))

(defn filter-keys
  [f xs]
  (->> xs
       keys
       (filter f)
       (select-keys xs)))

(defn wins
  [xs]
  (filter-keys (partial < 20) xs))

(defn loses
  [xs]
  (filter-keys (partial > 21) xs))

(defn sum-vals
  [xs]
  (->> xs
       vals
       (mapcat vals)
       (apply +)))

(defn sum-loses
  [xs]
  (sum-vals (loses xs)))

(defn sum-wins
  [xs]
  (sum-vals (wins xs)))

(defn dissoc-wins
  [xs]
  (->> xs
       wins
       keys
       (apply dissoc xs)))

(defn quantum-game
  [p1 p2]
  (loop [main {0 {p1 (bigint 1)}}
         main-win (bigint 0)
         alt  {0 {p2 (bigint 1)}}
         alt-win  (bigint 0)]
    (let [main (step main)
          main-win (+ main-win (* (sum-wins main) (sum-loses alt)))
          main (dissoc-wins main)]
      (if (empty? main)
        main-win
        (recur alt alt-win main main-win)))))

(defn part-two
  ([] (part-two data))
  ([input]
   (quantum-game (first input) (second input))))
