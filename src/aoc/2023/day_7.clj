(ns aoc.2020.day-7
  [:require
   [clojure.string :as s]])

(defn parse-game [game]
  (let [[hand bid] (s/split game #"\W+")
        hand (->> (s/split hand #"") (map keyword))
        bid (parse-long bid)]
    {:hand hand :bid bid}))

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map parse-game)))

(def type-map {[5] :five-of-a-kind [4 1] :four-of-a-kind
               [3 2] :full-house [3 1 1] :three-of-a-kind
               [2 2 1] :two-pair [2 1 1 1] :one-pair
               [1 1 1 1 1] :high-card})

(defn add-type [{hand :hand :as game}]
  (let [type (->> hand frequencies vals sort reverse)]
    (assoc game :type (type-map type))))

(def game-val-map (->> [:2 :3 :4 :5 :6 :7 :8 :9 :T :J :Q :K :A
                        :high-card :one-pair :two-pair
                        :three-of-a-kind :full-house
                        :four-of-a-kind :five-of-a-kind]
                       (map-indexed #(vector %2 %1))
                       (into {})))

(defn game-key [mapping {:keys [:hand :type]}]
  (map mapping (cons type hand)))

(defn game-compare [g1 g2]
  (->> g2
       (map - g1)
       (drop-while zero?)
       first))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map add-type)
        (sort-by (partial game-key game-val-map) game-compare)
        (map :bid)
        (map-indexed #(* (inc %1) %2))
        (apply +))))

(def joker-val-map (->> [:J :2 :3 :4 :5 :6 :7 :8 :9 :T :Q :K :A
                        :high-card :one-pair :two-pair
                        :three-of-a-kind :full-house
                        :four-of-a-kind :five-of-a-kind]
                       (map-indexed #(vector %2 %1))
                       (into {})))

(defn add-joker-type [{hand :hand :as game}]
  (let [hand (frequencies hand)
        jokers (get hand :J 0)
        [high-card & cards] (->> (dissoc hand :J) vals sort reverse)
        cards (cons (+ jokers (or high-card 0)) cards)]
    (assoc game :type (type-map cards))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (map add-joker-type)
        (sort-by (partial game-key joker-val-map) game-compare)
        (map :bid)
        (map-indexed #(* (inc %1) %2))
        (apply +))))
