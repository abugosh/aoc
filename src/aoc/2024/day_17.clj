(ns aoc.2024.day-17
  [:require
   [clojure.string :as s]
   [clojure.math.numeric-tower :as nt]])

(def data
  (->> (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (keep (partial re-seq #"\d+"))
       (map (partial mapv parse-long))
       ((juxt (comp #(conj % 0 []) vec (partial map first) (partial take 3)) (comp first (partial drop 3))))))

(defn step [program [a b c i out]]
  (let [ins (get program i)
        op (get program (inc i))]
    (if (nil? ins)
      [:DONE out]
      (let [combo (case op
                    0 0
                    1 1
                    2 2
                    3 3
                    4 a
                    5 b
                    6 c
                    7 :error)
            i (+ 2 i)]
        (case ins
          ;; adv
          0 [(quot a (nt/expt 2 combo)) b c i out]
          ;; bxl
          1 [a (bit-xor b op) c i out]
          ;; bst
          2 [a (mod combo 8) c i out]
          ;; jnz
          3 (if (zero? a)
              [a b c i out]
              [a b c op out])
          ;; bxc
          4 [a (bit-xor b c) c i out]
          ;; out
          5 [a b c i (conj out (mod combo 8))]
          ;; bdv
          6 [a (quot a (nt/expt 2 combo)) c i out]
          ;; cdv
          7 [a b (quot a (nt/expt 2 combo)) i out])))))

(defn do-program [[regs program]]
  (loop [[flag out :as state] regs]
    (if (= flag :DONE)
      out
      (recur (step program state)))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> (do-program input)
        (s/join ","))))

(defn big-xor
  [f & r]
  (reduce (fn [acc v] (.xor acc (biginteger v))) (biginteger f) r))

(defn program [i]
  (loop [a i
         out []]
    (if (zero? a)
      out
      (let [b (big-xor (mod a 8) 2)
            c (quot a (nt/expt 2 b))
            b (big-xor (big-xor b c) 7)]
        (recur (quot a 8) (conj out (mod b 8)))))))

(defn build-a [ls]
  (reduce (fn [acc n]
            (+ n (* acc 8N)))
          0 ls))

(defn solver [prog]
  (loop [i 0
         sol []]
    (if (= i (count prog))
      sol
      (recur (inc i) (some #(and (= (take-last (inc i) prog) (program (build-a (conj sol %)))) (conj sol %)) (range 0 10000))))))

(defn part-two
  ([] (part-two data))
  ([input]
   (build-a (solver (second input)))))
