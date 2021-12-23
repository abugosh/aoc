(ns aoc.day-23
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(declare do-cave-move simplify-board)

(def data (slurp "resources/day-23.txt"))

(defn build-game
  [d]
  (let [[a-cave b-cave c-cave d-cave] (->> d
                                           s/split-lines
                                           (drop 2)
                                           (take 2)
                                           (map #(re-seq #"[A|B|C|D]" %))
                                           (apply map #(list %1 %2)))]
    (-> {:board (vec (repeat 11 nil))
         :energy 0
         :depth 2
         2 a-cave
         4 b-cave
         6 c-cave
         8 d-cave})))

(defn build-deep-game
  [d]
  (let [[a-cave b-cave c-cave d-cave] (->> d
                                           s/split-lines
                                           (drop 2)
                                           (take 2)
                                           (map #(re-seq #"[A|B|C|D]" %))
                                           (apply map #(list %1 %2)))]
    (-> {:board (vec (repeat 11 nil))
         :energy 0
         :depth 4
         2 (list (first a-cave) "D" "D" (second a-cave))
         4 (list (first b-cave) "C" "B" (second b-cave))
         6 (list (first c-cave) "B" "A" (second c-cave))
         8 (list (first d-cave) "A" "C" (second d-cave))})))

(def caves (list 2 4 6 8))
(def cave-type {2 "A" 4 "B" 6 "C" 8 "D"})
(def crust-cave {"A" 2 "B" 4 "C" 6 "D" 8})
(def move-energy {"A" 1 "B" 10 "C" 100 "D" 1000})

(defn move-cave-energy
  [e depth cave]
  (* e (- depth (count cave))))

(defn legal-spaces-cave
  [{board :board :as game} cave]
  (if-some [crust (first (get game cave))]
    (if (and (= crust (cave-type cave)) (every? (partial = crust) (game cave)))
      (list)
      (remove #(cave-type %)
              (concat (take-while #(nil? (get board %)) (range (inc cave) 11))
                      (take-while #(nil? (get board %)) (range (dec cave) -1 -1)))))
    (list)))

(defn legal-spaces-board
  [{board :board :as game} space]
  (let [crust (or (get board space) (first (get game space)))
        cave (crust-cave crust)]
    (cond
      (nil? cave) nil
      (= space cave) nil
      (not-every? #(nil? (get board %)) (range (inc (min space cave)) (max space cave))) nil
      (and (> (:depth game) (count (get game cave))) (every? (partial = crust) (game cave))) space)))

(defn do-board-move
  [{board :board :as game} space]
  (let [crust (or (get board space) (first (get game space)))
        cave (crust-cave crust)
        e (move-energy crust)]
    (if (and (cave-type space) (not= cave space))
      (do-cave-move game space cave)
      (-> game
          (update :energy + (+ (* e (- (max space cave) (min space cave)))
                               (move-cave-energy e (:depth game) (get game cave))))
          (update cave conj crust)
          (assoc-in [:board space] nil)))))

(defn do-cave-move
  [game cave end]
  (let [crust (first (get game cave))
        e (move-energy crust)
        game (-> game
                 (update :energy + (+ (move-cave-energy e (:depth game) (rest (get game cave)))
                                      (* e (- (max cave end) (min cave end)))))
                 (update cave rest)
                 (assoc-in [:board end] crust))]
    (if (cave-type end)
      (do-board-move game end)
      game)))

(defn simplify-board
  [game]
  (if-some [move (some #(legal-spaces-board game %) (range 0 11))]
    (recur (do-board-move game move))
    game))

(defn solved?
  [game]
  (and (every? nil? (:board game))
       (every? (partial = "A") (game 2))
       (every? (partial = "B") (game 4))
       (every? (partial = "C") (game 6))
       (every? (partial = "D") (game 8))))

(defn search
  ([game] (search game Integer/MAX_VALUE 15))
  ([game min-energy depth]
   (cond 
     (= depth 0) min-energy
     (solved? game) (min (:energy game) min-energy)
     (< min-energy (:energy game)) min-energy
     :else (let [games (mapcat (fn [cave] (map #(do-cave-move game cave %) (legal-spaces-cave game cave))) caves)]
             (reduce #(search %2 %1 (dec depth)) min-energy (map simplify-board games))))))

(defn part-one
  ([] (part-one (build-game data)))
  ([input]
   (search input)))

(defn part-two
  ([] (part-two (build-deep-game data)))
  ([input]
   (search input)))
