(ns aoc.2019.day-5
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (->> (s/split (s/trim (slurp "resources/2019/day-5.txt")) #",")
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

(defn run [data input]
  (loop [eip 0
         input input
         output []
         data (transient data)]
    (let [ins (parse-ins data eip)]
      (case (:op ins)
        99 output
        1 (recur (+ 4 eip) input output (assoc! data (nth data (+ eip 3)) (+ (deref-param ins 1) (deref-param ins 2))))
        2 (recur (+ 4 eip) input output (assoc! data (nth data (+ eip 3)) (* (deref-param ins 1) (deref-param ins 2))))
        3 (recur (+ 2 eip) (pop input) output (assoc! data (nth data (+ eip 1)) (peek input)))
        4 (recur (+ 2 eip) input (conj output (deref-param ins 1)) data)
        5 (recur (if (not= 0 (deref-param ins 1)) (deref-param ins 2) (+ 3 eip)) input output data)
        6 (recur (if (= 0 (deref-param ins 1)) (deref-param ins 2) (+ 3 eip)) input output data)
        7 (recur (+ 4 eip) input output (assoc! data (nth data (+ eip 3)) (if (< (deref-param ins 1) (deref-param ins 2)) 1 0)))
        8 (recur (+ 4 eip) input output (assoc! data (nth data (+ eip 3)) (if (= (deref-param ins 1) (deref-param ins 2)) 1 0)))))))

(defn part-one
  ([] (part-one data))
  ([input]
   (last (run input [1]))))

(defn part-two
  ([] (part-two data))
  ([input]
   (last (run input [5]))))
