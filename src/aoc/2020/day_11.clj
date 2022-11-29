(ns aoc.2020.day-11
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (->> (slurp "resources/2020/day-11.txt")
       s/split-lines
       (mapv #(s/split % #""))
       (mapv #(mapv keyword %))))

(defn spaces-around [board y x]
  (for [yy (range (dec y) (+ 2 y))
        xx (range (dec x) (+ 2 x))
        :let [sp (get-in board [yy xx] :not-found)]
        :when (and (not= sp :not-found) (not (and (= y yy) (= x xx))))]
    sp))

(defn gen-occupied-around [sp-fn]
  (fn [board y x]
    (->> (sp-fn board y x)
         (filter #(= :# %))
         count)))

(def occupied-around (gen-occupied-around spaces-around))

(defn gen-step-space [occ-fn occ-count]
  (fn [board y x]
    (case (get-in board [y x])
      :L (if (= 0 (occ-fn board y x))
           :#
           :L)
      :# (if (< occ-count (occ-fn board y x))
           :L
           :#)
      (get-in board [y x]))))

(def step-space (gen-step-space occupied-around 3))

(defn step-board [step-fn board]
  (->> (for [yy (range (count board))
             xx (range (count (first board)))]
         (step-fn board yy xx))
       (partition (count (first board)))
       (mapv vec)))

(defn fix-board [step-fn board]
  (let [nxt (step-board step-fn board)]
    (if (= nxt board)
      board
      (recur step-fn nxt))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (fix-board step-space)
        flatten
        (filter #(= :# %))
        count)))

(defn look-in [board y y-fn x x-fn]
  (loop [yy (y-fn y)
         xx (x-fn x)]
    (let [sp (get-in board [yy xx] :not-found)]
      (if (= :. sp)
        (recur (y-fn yy) (x-fn xx))
        sp))))

(defn seats-around [board y x]
  (for [y-fn [dec identity inc]
        x-fn [dec identity inc]
        :let [sp (if (not (and (= y-fn identity) (= x-fn identity)))
                   (look-in board y y-fn x x-fn)
                   :not-found)]
        :when (and (not= sp :not-found) )]
    sp))

(def seats-occupied-around (gen-occupied-around seats-around))

(def seats-step-space (gen-step-space seats-occupied-around 4))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (fix-board seats-step-space)
        flatten
        (filter #(= :# %))
        count)))
