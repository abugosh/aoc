(ns aoc.2020.day-6
  [:require
   [clojure.string :as s]])

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map #(s/split % #":"))
       (map last)
       (map s/trim)
       (map #(s/split % #"\W+"))))

(defn find-record-breakers [[time record]]
  (for [t (range 0 time)
        :let [dist (* (- time t) t)]
        :when (> dist record)]
    t))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map (partial map parse-long))
        (apply map vector)
        (map find-record-breakers)
        (map count)
        (apply *))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (map (partial apply str))
        (map parse-long)
        find-record-breakers
        count)))
