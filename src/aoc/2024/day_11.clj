(ns aoc.2024.day-11
  [:require
   [clojure.string :as s]])

(def data
  (->> (s/split (s/trim (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))) #" ")
       (map parse-long)))

(def apply-rules (memoize
                  (fn [n]
                    (cond
                      (= n 0) {1 1}
                      (even? (count (str n))) (let [x (s/split (str n) #"")
                                                    [a b] (split-at (/ (count x) 2) x)
                                                    a (parse-long (s/join a))
                                                    b (parse-long (s/join b))]
                                                (if (= a b)
                                                  {a 2}
                                                  {a 1 b 1}))
                      :else {(* n 2024) 1}))))

(defn make-stones [ls]
  (zipmap ls (repeat 1)))

(defn blink [stones]
  (->> stones
       (map (fn [[k v]] (update-vals (apply-rules k) (partial * v))))
       (apply merge-with +)))

(defn do-blinks [stones c]
  (->> stones
       make-stones
       (iterate blink)
       (drop c)
       first
       vals
       (apply +)))

(defn part-one
  ([] (part-one data))
  ([input]
   (do-blinks input 25)))

(defn part-two
  ([] (part-two data))
  ([input]
   (do-blinks input 75)))
