(ns aoc.2020.day-1
  [:require
   [clojure.string :as s]])

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map (partial re-seq #"\d"))
        (map (juxt first last))
        (map (partial apply str))
        (map parse-long)
        (apply +))))

(defn words-to-digits [coord]
  (get {"one" "1"
        "two" "2"
        "three" "3"
        "four" "4"
        "five" "5"
        "six" "6"
        "seven" "7"
        "eight" "8"
        "nine" "9"} coord coord))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (map (fn [coord]
               (->> [(re-matches #"^.*?(\d|one|two|three|four|five|six|seven|eight|nine).*" coord)
                     (re-matches #".*(\d|one|two|three|four|five|six|seven|eight|nine).*$" coord)]
                    (map last)
                    (map words-to-digits)
                    (apply str)
                    parse-long)))
        (apply +))))
