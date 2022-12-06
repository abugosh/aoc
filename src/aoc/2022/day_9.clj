(ns aoc.2020.day-9
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(defn parse-line [line]
  (let [[_ dir cnt] (re-matches #"(\w) (\d+)" line)]
    [(keyword dir) (u/parse-int cnt)]))

(defn flatten-motion [[dir cnt]]
  (repeat cnt dir))

(def data
  (->> (slurp "resources/2022/day-9.txt")
       s/split-lines
       (map parse-line)
       (mapcat flatten-motion)))

(def short-rope {:h [0 0] :t (list [0 0])})

(defn move [[y x] dir]
  (case dir
    :U [(inc y) x]
    :D [(dec y) x]
    :R [y (inc x)]
    :L [y (dec x)]))

(defn near? [[a b] [c d]]
  (let [y (abs (- a c))
        x (abs (- b d))]
    (and (<= y 1) (<= x 1))))

(defn follow [[hy hx :as hd] [ty tx :as tl]]
  (if (near? hd tl)
    tl
    [(cond
       (> hy ty) (inc ty)
       (< hy ty) (dec ty)
       (= hy ty) ty)
     (cond
       (> hx tx) (inc tx)
       (< hx tx) (dec tx)
       (= hx tx) tx)]))

(defn update-tail [hd [t & r]]
  (if (nil? t)
    nil
    (let [tl (follow hd t)]
      (conj (update-tail tl r) tl))))

(defn build-trail [{h :h t :t} motions]
  (loop [h h
         t t
         trail #{}
         [dir & r] motions]
    (if (nil? dir)
      trail
      (let [hd (move h dir)
            tl (update-tail hd t)]
        (recur hd
               tl
               (conj trail (last tl))
               r)))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (build-trail short-rope)
        count)))

(def long-rope {:h [0 0] :t (repeat 9 [0 0])})

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (build-trail long-rope)
        count)))
