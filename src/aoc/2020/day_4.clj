(ns aoc.2020.day-4
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.set    :as st]])

(defn parse-data [line]
  (->> (s/split line #" ")
       (map #(s/split % #":"))
       flatten
       (apply hash-map)))

(def data
  (->> (slurp "resources/2020/day-4.txt")
       s/split-lines
       (partition-by #(= % ""))
       (remove #(= % '("")))
       (map #(s/join " " %))
       (map parse-data)))

(defn valid-keys? [passport]
  (->> passport
       keys
       set
       (st/subset? #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> data
        (filter valid-keys?)
        count)))

(defn valid-passport? [passport]
  (every? identity (-> passport
                       (update "byr" (fn [x] (let [byr (u/parse-int x)] (and (>= byr 1920) (<= byr 2002)))))
                       (update "iyr" (fn [x] (let [iyr (u/parse-int x)] (and (>= iyr 2010) (<= iyr 2020)))))
                       (update "eyr" (fn [x] (let [eyr (u/parse-int x)] (and (>= eyr 2020) (<= eyr 2030)))))
                       (update "hgt" (fn [x] (let [[_ m unit] (re-matches #"(\d+)(\w+)" x)
                                                   measure (u/parse-int m)]
                                               (or (and (= unit "cm") (and (>= measure 150) (<= measure 193)))
                                                   (and (= unit "in") (>= measure 59) (<= measure 76))))))
                       (update "hcl" #(re-matches #"#[0-9a-f]{6}+" %))
                       (update "ecl" #(contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} %))
                       (update "pid" #(re-matches #"\d{9}+" %))
                       vals)))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> data
        (filter valid-keys?)
        (filter valid-passport?)
        count)))
