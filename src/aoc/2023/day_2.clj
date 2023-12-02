(ns aoc.2020.day-2
  [:require
   [clojure.string :as s]])

(defn parse-reveal [reveal]
  (let [[_ red] (re-matches #".*?(\d+) red.*" reveal)
        [_ blue] (re-matches #".*?(\d+) blue.*" reveal)
        [_ green] (re-matches #".*?(\d+) green.*" reveal)]
    {:red (parse-long (or red "0"))
     :blue (parse-long (or blue "0"))
     :green (parse-long (or green "0"))}))

(defn parse-game [game]
  (let [[_ id reveals] (re-matches #"Game (\d+): (.*)" game)
        id (parse-long id)
        reveals (map s/trim (s/split reveals #";"))]
    {:id id :reveals (map parse-reveal reveals)}))

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map parse-game)))

(defn max-color [color reveals]
  (->> reveals (map color) (apply max)))

(defn add-max-reveals [game]
  (-> game
      (assoc :max-red (max-color :red (:reveals game)))
      (assoc :max-blue (max-color :blue (:reveals game)))
      (assoc :max-green (max-color :green (:reveals game)))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map add-max-reveals)
        (filter #(and (>= 12 (:max-red %)) (>= 14 (:max-blue %)) (>= 13 (:max-green %))))
        (map :id)
        (apply +))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (map add-max-reveals)
        (map #(* (:max-red %) (:max-blue %) (:max-green %)))
        (apply +))))
