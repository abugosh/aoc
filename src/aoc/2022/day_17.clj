(ns aoc.2020.day-17
  [:require
   [clojure.data.int-map :as i]
   [clojure.string :as s]])

(def data
  (as-> (slurp "resources/2022/day-17.txt") $
    (s/trim $)
    (s/split $ #"")
    (map keyword $)))

(def base-rocks [[2 3 4 5]
                 [3 12 13 14 23]
                 [24 14 02 03 04]
                 [2 12 22 32]
                 [2 3 12 13]])

(def base-board (->> (range 7) (into (i/dense-int-set))))

(defn generate-rock-stream []
  (let [rocks (atom (cycle base-rocks))]
    (fn [] (let [n (first @rocks)]
             (swap! rocks rest)
             n))))

(defn generate-jet-stream [jets]
  (let [jets (atom (cycle jets))]
    (fn [] (let [n (first @jets)]
             (swap! jets rest)
             n))))

(defn generate-state [jets] {:board base-board
                             :rock-stream (generate-rock-stream)
                             :jet-stream (generate-jet-stream jets)})

(defn board-height [board]
  (int (/ (last board) 10)))

(defn generate-rock [height rock] (map #(+ % (* 10 height)) rock))

(defn jet [dir rock]
  (let [dir (if (= dir :>) 1 -1)
        new-rock (map (partial + dir) rock)]
    (if (->> new-rock (some #(> (mod % 10) 6)))
      rock
      new-rock)))

(defn move [board dir rock]
  (let [new-rock (jet dir rock)]
    (if (some board new-rock)
      rock
      new-rock)))

(defn fall [board rock]
  (let [new-rock (map (partial + -10) rock)]
    (if (some board new-rock)
      [(apply conj board rock) nil]
      [board new-rock])))

(defn drop-rock [board jets rock]
  (let [[board new-rock] (->> rock
                              (move board (jets))
                              (fall board))]
    (if (nil? new-rock)
      board
      (recur board jets new-rock))))

(defn step [jet-stream rock-stream board]
  (let [board (drop-rock board jet-stream (generate-rock (+ 4 (board-height board)) (rock-stream)))]
    (if (< 300 (count board))
      (into (i/dense-int-set) (drop 100 board))
      board)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> base-board
        (iterate (partial step (generate-jet-stream input) (generate-rock-stream)))
        (drop 2022)
        first
        board-height)))

(defn find-height [jets rounds]
  (->> base-board
        (iterate (partial step (generate-jet-stream jets) (generate-rock-stream)))
        (drop rounds)
        first
        board-height))

(defn find-seq [jets]
  (let [game (->> base-board
                  (iterate (partial step (generate-jet-stream jets) (generate-rock-stream)))
                  (drop 50000)
                  (map board-height))]
    (loop [len 100]
      (if (= len 10000)
        nil
        (if (= (map - (take len game) (drop 1 (take len game)))
               (map - (take len (drop len game)) (drop 1 (take len (drop len game)))))
          [(- (->> game (drop len) first) (first game)) len]
          (recur (inc len)))))))

(defn find-large-height [jets rocks n d]
  (let [factor (mod rocks d)]
    (+ (find-height jets factor) (bigint (* (- rocks factor) (/ n d))))))

(defn part-two
  ([] (part-two data))
  ([input]
   (apply find-large-height input 1000000000000 (find-seq data))))
