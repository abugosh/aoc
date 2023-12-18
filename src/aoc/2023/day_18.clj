(ns aoc.2020.day-18
  [:require
   [clojure.string :as s]])

(defn parse-line [line]
  (let [[_ dir num color] (re-matches #"([UDLR]) (\d+) \(#(\w+)\)" line)]
    [(keyword dir) (parse-long num) color]))

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map parse-line)))

(def dir-map {:U [-1 0]
              :D [1 0]
              :R [0 1]
              :L [0 -1]})

(defn gen-pnt [dir pnt]
  (mapv + pnt (dir-map dir)))

(defn dig [start [dir len]]
  (->> start
       (iterate (partial gen-pnt dir))
       (drop 1)
       (take len)))

(defn follow-ins [ins-list]
  (loop [pnt [0 0]
         [ins & ins-list] ins-list
         trench #{}]
    (if (nil? ins)
      trench
      (let [seg (dig pnt ins)]
        (recur (last seg) ins-list (into trench seg))))))

(defn flood-dig [trench]
  (loop [[cur & open] [[1 1]]
         lagoon trench]
    (cond
      (nil? cur) lagoon
      (lagoon cur) (recur open lagoon)
      :else (recur (concat open (map #(gen-pnt % cur) [:U :D :R :L])) (conj lagoon cur)))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        follow-ins
        flood-dig
        count)))

(defn parse-color [color]
  (let [len (Integer/parseInt (s/join (drop-last 1 color)) 16)
        dir ([:R :D :L :U] (parse-long (str (last color))))]
    [dir len]))

(defn parse-color-ins [ins-list]
  (->> ins-list
       (map last)
       (map parse-color)))

(defn ins-segments [ins-list]
  (loop [cur [0 0]
         [[dir len :as ins] & ins-list] ins-list
         segments []]
    (if (nil? ins)
      segments
      (let [end (mapv + cur (map (partial * len) (dir-map dir)))]
        (recur end ins-list (conj segments [cur end]))))))

(defn shoelace [segments]
  (/ (->> segments
          (map (fn [[[x1 y1] [x2 y2]]] (- (* x2 y1) (* x1 y2))))
          (apply +))
     2))

(defn part-two
  ([] (part-two data))
  ([input]
   (let [ins-list (parse-color-ins input)
         perimeter (inc (/ (apply + (map second ins-list)) 2))]
     (->> ins-list
          ins-segments
          shoelace
          (+ perimeter)))))
