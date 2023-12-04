(ns aoc.2020.day-4
  [:require
   [clojure.string :as s]
   [clojure.set :as set]
   [clojure.math :as math]])

(defn parse-numbers [numbers]
  (->> numbers
       (re-seq #"\d+")
       (map parse-long)))

(defn parse-line [line]
  (let [[card numbers] (s/split line #":")
        [_ id] (s/split card #"\W")
        [winning pulled] (s/split numbers #"\|")]
    {:id (parse-long id) :winning (parse-numbers winning) :pulled (parse-numbers pulled)}))

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map parse-line)))

(defn score-count [c]
  (if (= 0 c)
    0
    (int (math/pow 2 (dec c)))))

(defn match-count [cards]
  (->> cards
       (map (juxt :winning :pulled))
       (map (partial map set))
       (map (partial apply set/intersection))
       (map count)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        match-count
        (map score-count)
        (apply +))))

(defn count-cards [cards]
  (loop [[card & cards] cards
         [cur-count & counts] (take (inc (count cards)) (repeat 1))
         final []]
    (if (nil? card)
      final
      (let [[up r] (split-at card counts)]
        (recur cards (concat (map (partial + cur-count) up) r) (conj final cur-count))))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        match-count
        count-cards
        (apply +))))
