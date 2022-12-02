(ns aoc.2020.day-2
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def decode-guide {"A" :rock
                   "B" :paper
                   "C" :scissors
                   "X" :rock
                   "Y" :paper
                   "Z" :scissors})

(def decode-outcome {"X" :lose
                     "Y" :draw
                     "Z" :win})

(defn parse-game [line]
  (let [[_ opp me] (re-matches #"(\w) (\w)" line)]
    {:opp (decode-guide opp)
     :me (decode-guide me)
     :outcome (decode-outcome me)
     :score 0}))

(def data
  (->> (slurp "resources/2022/day-2.txt")
       s/split-lines
       (map parse-game)))

(def shape-score {:rock 1
                  :paper 2
                  :scissors 3})

(defn score-shape [{me :me :as game}]
  (update game :score + (shape-score me)))

(def outcome-score {:rock {:rock 3
                           :paper 0
                           :scissors 6}
                    :paper {:rock 6
                            :paper 3
                            :scissors 0}
                    :scissors {:rock 0
                               :paper 6
                               :scissors 3}})

(defn score-outcome [{me :me opp :opp :as game}]
  (update game :score + (get-in outcome-score [me opp])))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map score-shape)
        (map score-outcome)
        (map :score)
        (apply +))))

(def map-outcome {:win {:rock :paper
                        :paper :scissors
                        :scissors :rock}
                  :lose {:rock :scissors
                         :paper :rock
                         :scissors :paper}
                  :draw {:rock :rock
                         :paper :paper
                         :scissors :scissors}})

(defn fix-plays [{outcome :outcome opp :opp :as game}]
  (assoc game :me (get-in map-outcome [outcome opp])))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (map fix-plays)
        part-one)))
