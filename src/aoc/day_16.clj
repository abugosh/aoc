(ns aoc.day-16
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(declare parse)

(def data 
  (s/trim (slurp "resources/day-16.txt")))

(def hex->bin {"0" "0000" "1" "0001" "2" "0010" "3" "0011" "4" "0100" "5" "0101" "6" "0110" "7" "0111"
               "8" "1000" "9" "1001" "A" "1010" "B" "1011" "C" "1100" "D" "1101" "E" "1110" "F" "1111"})

(defn data->bin
  [d]
  (->> (s/split d #"")
       (map hex->bin)
       (mapcat #(s/split %1 #""))))

(defn bin->int
  [bin]
  (Long/parseLong (apply str bin) 2))

(defn p-type
  [{msg :msg :as packet}]
  (-> packet
      (assoc :type (bin->int (take 3 msg)))
      (assoc :msg (drop 3 msg))))

(defn build-packet
  [msg]
  (p-type {:version (bin->int (take 3 msg))
   :msg (drop 3 msg)}))

(defn end-literal?
  [xs]
  (= "0" (first xs)))

(defn nop?
  [xs]
  (or (every? (partial = "0") xs) (empty? xs)))

(defn parse-literal
  ([msg] (parse-literal msg []))
  ([msg literal]
  (let [part (take 5 msg)
        rest-msg (drop 5 msg)]
    (if (end-literal? part)
      {:msg rest-msg :literal (bin->int (concat literal (drop 1 part)))}
      (recur rest-msg (concat literal (drop 1 part)))))))

(defn parse-op-len
  [msg]
  (let [len (bin->int (take 15 msg))
        rest-msg (drop (+ 15 len) msg)]
    (loop [msg (take len (drop 15 msg))
           packets []]
      (let [{m :msg :as packet} (parse msg)]
        (if (nop? m)
          {:msg rest-msg :packets (conj packets packet)}
          (recur m (conj packets packet)))))))

(defn parse-op-count
  [msg]
  (let [len (bin->int (take 11 msg))]
    (loop [i 1
           msg (drop 11 msg)
           packets []]
      (let [{m :msg :as packet} (parse msg)]
        (if (= i len)
          {:msg m :packets (conj packets packet)}
          (recur (inc i) m (conj packets packet)))))))

(defn parse
  [msg]
  (let [packet (build-packet msg)]
    (match [packet]
           [{:type 4 :msg m}] (merge packet (parse-literal m))
           [{:msg (["0" & r] :seq)}] (merge packet (parse-op-len r))
           [{:msg (["1" & r] :seq)}] (merge packet (parse-op-count r)))))

(defn sum-version
  [packet]
  (+ (:version packet)
     (->> (:packets packet)
          (map sum-version)
          (apply +))))

(defn p-comp
  [f x y]
  (if (f x y) 1 0))

(defn process
  [packet]
  (match [packet]
         [{:type 4 :literal x}] x
         [{:type 0 :packets p}] (->> p (map process) (apply +))
         [{:type 1 :packets p}] (->> p (map process) (apply *))
         [{:type 2 :packets p}] (->> p (map process) (apply min))
         [{:type 3 :packets p}] (->> p (map process) (apply max))
         [{:type 5 :packets p}] (->> p (map process) (apply p-comp >))
         [{:type 6 :packets p}] (->> p (map process) (apply p-comp <))
         [{:type 7 :packets p}] (->> p (map process) (apply p-comp =))))

(defn part-one
  ([] (part-one (data->bin data)))
  ([input]
   (sum-version (parse input))))

(defn part-two
  ([] (part-two (data->bin data)))
  ([input]
   (process (parse input))))
