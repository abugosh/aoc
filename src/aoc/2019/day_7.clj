(ns aoc.2019.day-7
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.math.combinatorics :as combo]])

(def data
  (->> (s/split (s/trim (slurp "resources/2019/day-7.txt")) #",")
       (map u/parse-int)
       vec))

(defn parse-ins [data eip]
  (->> (s/split (format "%05d" (nth data eip)) #"")
       (map u/parse-int)
       (apply (fn [a b c op1 op2]
                {:mode-1 c :mode-2 b :mode-3 a :op (u/parse-int (str op1 op2)) :eip eip :data data}))))

(defn deref-param [ins i]
  (let [p (nth (ins :data) (+ i (ins :eip)))]
    (case (ins (keyword (str "mode-" i)))
      0 (nth (ins :data) p)
      1 p)))

(defn build-state [state eip input output data]
  {:state state
   :eip eip
   :input input
   :output output
   :data data})

(defn initial-state [data input]
  (build-state :running 0 input [] (transient data)))

(defn step-state [{:keys [data eip input output]}]
  (let [ins (parse-ins data eip)]
    (case (:op ins)
      99 (build-state :halt eip input output (persistent! data))
      1 (build-state :running (+ 4 eip) input output (assoc! data (nth data (+ eip 3)) (+ (deref-param ins 1) (deref-param ins 2))))
      2 (build-state :running (+ 4 eip) input output (assoc! data (nth data (+ eip 3)) (* (deref-param ins 1) (deref-param ins 2))))
      3 (if (empty? input)
          (build-state :need-input eip  input output data)
          (build-state :running (+ 2 eip) (pop input) output (assoc! data (nth data (+ eip 1)) (peek input))))
      4 (build-state :running (+ 2 eip) input (conj output (deref-param ins 1)) data)
      5 (build-state :running (if (not= 0 (deref-param ins 1)) (deref-param ins 2) (+ 3 eip)) input output data)
      6 (build-state :running (if (= 0 (deref-param ins 1)) (deref-param ins 2) (+ 3 eip)) input output data)
      7 (build-state :running (+ 4 eip) input output (assoc! data (nth data (+ eip 3)) (if (< (deref-param ins 1) (deref-param ins 2)) 1 0)))
      8 (build-state :running (+ 4 eip) input output (assoc! data (nth data (+ eip 3)) (if (= (deref-param ins 1) (deref-param ins 2)) 1 0))))))

(defn run-state [state]
  (loop [state state]
    (let [state (step-state state)]
      (if (= (:state state) :running)
        (recur state)
        state))))

(defn run [data input]
  (run-state (initial-state data input)))

(defn run-amp [data phase signal]
  (first (:output (run data [signal phase]))))

(defn test-amp [data [a b c d e]]
  (->> 0
       (run-amp data a)
       (run-amp data b)
       (run-amp data c)
       (run-amp data d)
       (run-amp data e)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> (range 0 5)
        combo/permutations
        (map (partial test-amp input))
        (apply max))))

(defn feedback-amp [data [a b c d e]]
  (loop [signal 0
         a-state (run data [a])
         b-state (run data [b])
         c-state (run data [c])
         d-state (run data [d])
         e-state (run data [e])]
    (let [a-state (run-state (update a-state :input conj signal))
          b-state (run-state (update b-state :input conj (last (:output a-state))))
          c-state (run-state (update c-state :input conj (last (:output b-state))))
          d-state (run-state (update d-state :input conj (last (:output c-state))))
          e-state (run-state (update e-state :input conj (last (:output d-state))))
          signal (last (:output e-state))]
      (if (= :halt (:state e-state))
        signal
        (recur signal a-state b-state c-state d-state e-state)))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> (range 5 10)
        combo/permutations
        (map (partial feedback-amp input))
        (apply max))))
