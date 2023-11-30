(ns aoc.2019.day-13
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (->> (s/split (s/trim (slurp "resources/2019/day-13.txt")) #",")
       (map parse-long)
       (zipmap (range))))

(defn parse-ins [data eip rp]
  (->> (s/split (format "%05d" (get data eip 0)) #"")
       (map u/parse-int)
       (apply (fn [a b c op1 op2]
                {:mode-1 c :mode-2 b :mode-3 a :op (u/parse-int (str op1 op2)) :eip eip :rp rp :data data}))))

(defn deref-param [mode ins i]
  (let [p (get (ins :data) (+ i (ins :eip)))]
    (case (ins (keyword (str "mode-" i)))
      0 ({:w p :r (get (ins :data) p 0)} mode)
      1 p
      2 ({:w (+ (:rp ins) p) :r (get (ins :data) (+ (:rp ins) p) 0)} mode))))

(defn build-state [state eip rp input output data]
  {:state state
   :eip eip
   :rp rp
   :input input
   :output output
   :data data})

(defn initial-state [data input]
  (build-state :running 0 0 input [] (transient data)))

(defn step-state [{:keys [data eip rp input output]}]
  (let [ins (parse-ins data eip rp)]
    (case (:op ins)
      99 (build-state :halt eip rp input output (persistent! data))
      1 (build-state :running (+ 4 eip) rp input output (assoc! data (deref-param :w ins 3) (+ (deref-param :r ins 1) (deref-param :r ins 2))))
      2 (build-state :running (+ 4 eip) rp input output (assoc! data (deref-param :w ins 3) (* (deref-param :r ins 1) (deref-param :r ins 2))))
      3 (if (empty? input)
          (build-state :need-input eip rp input output data)
          (build-state :running (+ 2 eip) rp (pop input) output (assoc! data (deref-param :w ins 1) (peek input))))
      4 (build-state :running (+ 2 eip) rp input (conj output (deref-param :r ins 1)) data)
      5 (build-state :running (if (not= 0 (deref-param :r ins 1)) (deref-param :r ins 2) (+ 3 eip)) rp input output data)
      6 (build-state :running (if (= 0 (deref-param :r ins 1)) (deref-param :r ins 2) (+ 3 eip)) rp input output data)
      7 (build-state :running (+ 4 eip) rp input output (assoc! data (deref-param :w ins 3) (if (< (deref-param :r ins 1) (deref-param :r ins 2)) 1 0)))
      8 (build-state :running (+ 4 eip) rp input output (assoc! data (deref-param :w ins 3) (if (= (deref-param :r ins 1) (deref-param :r ins 2)) 1 0)))
      9 (build-state :running (+ 2 eip) (+ rp (deref-param :r ins 1)) input output data))))

(defn run-state [state]
  (loop [state state]
    (let [state (step-state state)]
      (if (= (:state state) :running)
        (recur state)
        state))))

(defn run [data input]
  (run-state (initial-state data input)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> (run input [])
        :output
        (partition 3)
        (map last)
        (filter (partial = 2))
        count)))

(defn render-game [output]
  )

(defn part-two
  ([] (part-two data))
  ([input]
   ))
