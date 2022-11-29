(ns aoc.2020.day-8
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(defn parse-instruction [line]
  (let [[op arg] (s/split line #" ")]
       {:op op
        :arg (u/parse-int arg)
        :tainted false}))

(defn build-state [instructions]
  {:acc 0
   :eip 0
   :state :not-run
   :program (conj (vec instructions) (parse-instruction "fin +0"))})

(def data
  (->> (slurp "resources/2020/day-8.txt")
       s/split-lines
       (map parse-instruction)
       build-state))

(defn step [state]
  (if (= :success (:state state))
    state
    (let [eip (:eip state)
          ins (get-in state [:program eip])]
      (if (:tainted ins)
        (assoc state :state :error)
        (recur (assoc-in (case (:op ins)
                           "acc" (-> state (update :eip inc) (update :acc + (:arg ins)))
                           "jmp" (update state :eip + (:arg ins))
                           "fin" (assoc state :state :success)
                           (update state :eip inc))
                         [:program eip :tainted] true))))))

(defn find-fix [base-state]
  (some (fn [x]
          (let [state (step (update-in base-state [:program x :op] {"nop" "jmp" "jmp" "nop" "acc" "acc"}))]
            (if (= :success (:state state))
              state
              nil)))
        (range 0 (count (:program base-state)))))

(defn part-one
  ([] (part-one data))
  ([input]
   (:acc (step input))))

(defn part-two
  ([] (part-two data))
  ([input]
   (:acc (find-fix input))))
