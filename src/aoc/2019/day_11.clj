(ns aoc.2019.day-11
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (->> (s/split (s/trim (slurp "resources/2019/day-11.txt")) #",")
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

(defn base-state [program start]
  {:program (run program []) :loc [0 0] :dir :N :grid {[0 0] start}})

(def turn-table
  {:N {0 :W 1 :E}
   :S {0 :E 1 :W}
   :E {0 :N 1 :S}
   :W {0 :S 1 :N}})

(defn turn-robot [dir turn]
  (get-in turn-table [dir turn]))

(defn move-robot [dir [y x]]
  (case dir
    :N [(inc y) x]
    :S [(dec y) x]
    :E [y (inc x)]
    :W [y (dec x)]))

(defn step [{:keys [:program :loc :dir :grid]}]
  (let [program (run-state (assoc program :input [(get grid loc 0)]))
        [color turn] (take-last 2 (:output program))
        dir (turn-robot dir turn)]
    {:program program :loc (move-robot dir loc) :dir dir :grid (assoc grid loc color)}))

(defn run-robot [start program]
  (loop [state (base-state program start)]
    (if (= :halt (:state (:program state)))
      (:grid state)
      (recur (step state)))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (run-robot 0)
        count)))

(defn standardize-grid [grid]
  (let [min-y (->> grid keys (map first) (apply min) abs)
        min-x (->> grid keys (map last) (apply min) abs)]
    (update-keys grid (fn [[y x]] [(+ y min-y) (+ x min-x)]))))

(defn build-board [max-y max-x]
  (vec (map vec (take (inc max-y) (repeatedly #(repeat (inc max-x) "."))))))

(defn part-two
  ([] (part-two data))
  ([input]
   (let [grid (->> input (run-robot 1) standardize-grid)
         max-y (->> grid keys (map first) (apply max))
         max-x (->> grid keys (map last) (apply max))]
     (->> grid
          (reduce (fn [board [k v]] (assoc-in board k ({0 "." 1 "#"} v))) (build-board max-y max-x))
          (map s/join)
          reverse))))
