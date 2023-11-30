(ns aoc.2020.day-22
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(defn parse-board [lines]
  (->> lines
       (map (partial map str))
       (mapv (partial mapv {" " nil "." :. "#" :#}))))

(defn parse-path [path]
  (->> path
       (re-seq #"\d+|\w")
       (map #(or (parse-long %) (keyword %)))))

(defn parse-file [[board path]]
  {:board (parse-board board)
   :path (parse-path (second path))})

(def data
  (->> (slurp "resources/2022/day-22.txt")
       s/split-lines
       (split-with seq)
       parse-file))

(def dir-table {:north [-1 0]
                 :south [1 0]
                 :east [0 1]
                 :west [0 -1]})

(def wrap-table {:north [1 0]
                 :south [-1 0]
                 :east [0 -1]
                 :west [0 1]})

(def rotate-table {:L {:north :west
                       :south :east
                       :east :north
                       :west :south}
                   :R {:north :east
                       :south :west
                       :east :south
                       :west :north}})

(defn start-state [board]
  {:loc [0 (->> board
                first
                (take-while (partial not= :.))
                count)]
   :dir :east})

(defn move-flat [board {loc :loc dir :dir :as state} dist]
  (if (= dist 0)
    state
    (let [new-loc (mapv + loc (dir-table dir))]
      (case (get-in board new-loc)
        nil (loop [new-loc loc]
              (if (nil? (get-in board new-loc))
                (let [new-state (move board (assoc state :loc new-loc) dist)]
                  (if (= (:loc new-state) new-loc)
                    state
                    new-state))
                (recur (mapv + new-loc (wrap-table dir)))))
        :. (recur board (assoc state :loc new-loc) (dec dist))
        :# state))))

(defn rotate [state ins]
  (update state :dir (rotate-table ins)))

(defn follow-path [move-fn board state ins]
  (if (keyword? ins)
    (rotate state ins)
    (move-fn board state ins)))

(def dir-value {:north 3
                :south 1
                :east 0
                :west 2})

(defn password [{[row column] :loc dir :dir}]
  (+ (* 1000 (inc row)) (* 4 (inc column)) (dir-value dir)))

(defn part-one
  ([] (part-one data))
  ([{board :board path :path}]
   (->> path
        (reduce (partial follow-path move-flat board) (start-state board))
        password)))

(def corner-table {:up [-50 0]
                   :down [50 0]
                   :left [0 -50]
                   :right [0 50]})

(defn board-to [board corner dir]
  (and corner
       (let [new-corner (mapv + corner (corner-table dir))]
         (if (get-in board new-corner)
           new-corner
           nil))))

(defn start-cube-state [board]
  {:loc [0 0]
   :dir :east
   :side :top})

(defn start-cube [board]
  (let [top-loc (:loc (start-state board))
        start {:top top-loc
          :bottom nil
          :left (board-to board top-loc :left)
          :right (board-to board top-loc :right)
          :up (board-to board top-loc :up)
          :down (board-to board top-loc :down)}]
    (as-> start $
      (assoc $ :bottom (some #(board-to board ($ %) :down) [:left :right :up :down]))
      (assoc $ :left (board-to board ($ :bottom) :left))
      (assoc $ :up (board-to board ($ :left) :down)))))

(def cube-dir-table {:up [-1 0]
                     :down [1 0]
                     :left [0 -1]
                     :right [0 1]})



(defn move-cube [board cube {loc :loc dir :dir side :side :as state} dist]
  (if (= dist 0)
    state
    (let [side (cube side)
          new-loc (mapv + loc (cube-dir-table dir))
          board-loc (mapv + new-loc side)]
      (if (or (>= (first new-loc)  50)
              (>= (second new-loc) 50)
              (neg? (first new-loc))
              (neg? (second new-loc)))
        nil
        (case (get-in board board-loc)
          :. (recur board cube (assoc state :loc new-loc) (dec dist))
          :# state)))))

(defn part-two
  ([] (part-two data))
  ([input]
   nil))
