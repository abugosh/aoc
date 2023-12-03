(ns aoc.2020.day-3
  [:require
   [clojure.string :as s]])

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map #(s/split % #""))
       vec))

(def symbols #{"=" "*" "%" "/" "-" "&" "#" "+" "$" "@"})

(defn add-symbol [board {[y x] :loc len :len :as number}]
  (assoc number :symbol (first (for [y (range (dec y) (+ 2 y))
                                     x (range (dec x) (+ 1 x len))
                                     :let [v (get-in board [y x] ".")]
                                     :when (symbols v)]
                                 {:loc [y x] :symbol v}))))

(defn pull-number [board y x]
  (let [loc [y x]]
    (loop [x x
           n []]
      (let [v (get-in board [y x] "")]
        (if-not (parse-long v)
          (add-symbol board {:loc loc :len (count n) :num (parse-long (apply str n))})
          (recur (inc x) (conj n v)))))))

(defn search-board [board]
  (loop [y 0
         x 0
         numbers []]
    (let [v (get-in board [y x])]
      (cond
        (and (= x 0) (nil? v)) numbers
        (nil? v) (recur (inc y) 0 numbers)
        (parse-long v) (let [number (pull-number board y x)]
                         (recur y (+ x (:len number)) (conj numbers number)))
        :else (recur y (inc x) numbers)))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        search-board
        (filter :symbol)
        (map :num)
        (apply +))))

(defn part-two
  ([] (part-two data))
  ([input]
     (->> input
          search-board
          (filter #(= "*" (:symbol (:symbol %))))
          (group-by :symbol)
          vals
          (filter #(= 2 (count %)))
          (map #(apply * (map :num %)))
          (apply +))))
