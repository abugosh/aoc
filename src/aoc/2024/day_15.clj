(ns aoc.2024.day-15
  [:require
   [clojure.string :as s]
   [aoc.utils :as u]])

(defn parse-grid [lines]
  (let [grid (->> lines
                  (mapv #(s/split % #""))
                  u/grid->map)
        groups (u/group-vals grid)]
    [(set (groups "#"))
     [(first (groups "@")) (set (groups "O"))]]))

(defn parse-big-grid [lines]
  (let [grid (->> lines
                  (mapv #(s/split % #""))
                  (mapv (partial mapcat {"#" ["#" "#"]
                                         "@" ["@" "."]
                                         "O" ["[" "]"]
                                         "." ["." "."]}))
                  (mapv vec)
                  u/grid->map)
        groups (u/group-vals grid)]
    [(set (groups "#"))
     [(first (groups "@")) (set (groups "[")) (set (groups "]"))]]))

(defn parse-moves [lines]
  (->> lines
       (map #(s/split % #""))
       (apply concat)
       (map {"^" :N ">" :E "<" :W "v" :S})))

(def data
  (->> (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (partition-by empty?)
       ((juxt first (comp parse-moves last)))))

(defn move [walls [loc boxen] dir]
  (let [nxt (u/gen-pnt loc (u/dir-map dir))]
    (cond
      (walls nxt) [loc boxen]
      (boxen nxt) (let [[a b] (move walls [nxt boxen] dir)]
                    (if (= a nxt)
                      [loc boxen]
                      [nxt (-> b (conj a) (disj nxt))]))
      :else [nxt boxen])))

(defn do-moves [f [walls state] moves]
  (reduce (partial f walls) state moves))

(defn score [[a b]]
  (+ (* 100 a) b))

(defn part-one
  ([] (part-one data))
  ([input]
   (let [world (parse-grid (first input))]
     (->> (last input)
          (do-moves move world)
          last
          (map score)
          (apply +)))))

(defn push-boxen-ew [walls [box l-boxen r-boxen :as state] dir]
  (let [vel (u/dir-map dir)
        f (fn push [[box a-boxen b-boxen :as state]]
            (let [nxt (u/gen-pnt box vel)
                  nnxt (u/gen-pnt nxt vel)
                  nnnxt (u/gen-pnt nnxt vel)]
              (cond
                (walls nxt) state
                (a-boxen nxt) (let [[loc a-boxen b-boxen] (push [nnxt a-boxen b-boxen])]
                                (if (= loc nnxt)
                                  state
                                  [nxt (conj (disj a-boxen nxt) nnxt) (conj (disj b-boxen nnxt) nnnxt)]))
                :else [nxt a-boxen b-boxen])))]
    (if (= dir :E)
      (f state)
      (let [[box r-boxen l-boxen] (f [box r-boxen l-boxen])]
        [box l-boxen r-boxen]))))

(defn push-boxen-ns [walls [box l-boxen r-boxen :as base-state] dir]
  (let [vel (u/dir-map dir)
        nxt (u/gen-pnt box vel)
        f (fn push [box [a-boxen b-boxen :as state]]
            (if-not (or (a-boxen box) (b-boxen box))
              state
              (let [[a b] (if (a-boxen box) [box (u/gen-pnt box (u/dir-map :E))] [(u/gen-pnt box (u/dir-map :W)) box])
                    a-nxt (u/gen-pnt a vel)
                    b-nxt (u/gen-pnt b vel)]
                (if (or (walls a-nxt) (walls b-nxt))
                  state
                  (let [[a-boxen b-boxen] (->> state (push a-nxt) (push b-nxt))]
                    (if (or (a-boxen a-nxt) (b-boxen a-nxt) (a-boxen b-nxt) (b-boxen b-nxt))
                      state
                      [(conj (disj a-boxen a) a-nxt) (conj (disj b-boxen b) b-nxt)]))))))]
    (if (walls nxt)
      base-state
      (let [[l-boxen r-boxen] (f nxt [l-boxen r-boxen])]
        (if (or (l-boxen nxt) (r-boxen nxt))
          base-state
          [nxt l-boxen r-boxen])))))

(defn big-move [walls state dir]
  (cond
    (#{:E :W} dir) (push-boxen-ew walls state dir)
    (#{:N :S} dir) (push-boxen-ns walls state dir)))

(defn part-two
  ([] (part-two data))
  ([input]
   (let [world (parse-big-grid (first input))]
     (->> (last input)
          (do-moves big-move world)
          second
          (map score)
          (apply +)))))
