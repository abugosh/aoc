(ns aoc.2020.day-7
  [:require
   [aoc.utils :as u]
   [clojure.core.match :refer [match]]
   [clojure.string :as s]])

(defn parse-line [line]
  (if (s/starts-with? line "$")
    (cond
      (= line "$ cd /") {:cmd :home}
      (= line "$ ls") nil
      (= line "$ cd ..") {:cmd :up}
      :else {:cmd :cd :dir (second (re-matches #"\$ cd (\w+)" line))})
    (if (s/starts-with? line "dir")
      {:cmd :add :obj {:name (second (re-matches #"dir (\w+)" line)) :type :dir}}
      (let [[_ size name] (re-matches #"(\d+) (.+)" line)]
        {:cmd :add :obj {:name name :type :file :size (u/parse-int size)}}))))

(defn build-fs [cmds]
  (loop [loc []
          fs {}
         [cmd & r] cmds]
    (match cmd
           nil fs
           {:cmd :home} (recur ["/"] fs r)
           {:cmd :up} (recur (pop loc) fs r)
           {:cmd :cd :dir d} (recur (conj loc d) fs r)
           {:cmd :add :obj obj} (recur loc
                                         (assoc fs (conj loc (:name obj)) obj)
                                         r))))

(def data
  (->> (slurp "resources/2022/day-7.txt")
       s/split-lines
       (map parse-line)
       (remove nil?)
       build-fs))

(defn size-dir [fs dir]
  (->> fs
       (filter #(= :file (:type (second %))))
       (filter #(= dir (take (count dir) (first %))))
       (map second)
       (map :size)
       (apply +)))

(defn fs-directories [fs]
  (->> fs
       (filter #(= :dir (:type (second %))))
       (map first)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        fs-directories
        (map (partial size-dir input))
        (filter (partial >= 100000))
        (apply +))))

(defn part-two
  ([] (part-two data))
  ([input]
   (let [target (- 30000000 (- 70000000 (size-dir input ["/"])))]
     (->> input
          fs-directories
          (map (partial size-dir input))
          sort
          (some #(and (>= % target) %))))))
