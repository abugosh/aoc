(ns aoc.2020.day-13
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(defn build-time-table [[ts schedule]]
  {:ts (u/parse-int ts)
   :schedule (->> (s/split schedule #",")
                  (map (fn [bus]
                         (if (= "x" bus)
                           :x
                           (u/parse-int bus)))))})

(def data
  (->> (slurp "resources/2020/day-13.txt")
       s/split-lines
       build-time-table))

(defn wait-time [ts bus]
  (let [dist (mod ts bus)]
    (- bus dist)))

(defn part-one
  ([] (part-one data))
  ([{:keys [ts schedule]}]
   (->> schedule
        (filter #(not= :x %))
        (reduce (fn [earliest bus]
                  (if (< (wait-time ts bus) (wait-time ts earliest))
                    bus
                    earliest)))
        (#(* (wait-time ts %) %)))))

(defn find-ratio [base target offset]
  (->> (range base)
       (some #(and (= offset (wait-time (* base %) target)) %))))

(defn gen-ratio-fn [base target ratio]
  (fn [x] (+ (* base target x) (* base ratio))))

(defn build-wait-table [schedule]
  (let [slowest (apply max (remove #{:x} schedule))
        parts (partition-by #(= slowest %) schedule)
        lower (->> parts
                   first
                   reverse
                   (map-indexed #(if (= %2 :x) :x {:bus %2 :true-offset (- (inc %1)) :offset (- %2 (inc %1))})))
        upper (->> parts
                   last
                   (map-indexed #(if (= %2 :x) :x {:bus %2 :true-offset (inc %1) :offset (inc %1)})))]
    {:base slowest
     :offsets (remove #(= slowest (:bus %)) (remove #{:x} (concat lower upper)))}))

(defn find-gold [{:keys [base offsets]}]
  (let [target (->> offsets (sort-by #(:bus %)) last)
        ratio (find-ratio base (:bus target) (:offset target))
        ratio-fn (gen-ratio-fn base (:bus target) ratio)]
    (loop [ts 0]
      (if (every? (fn [{:keys [bus offset]}] (= bus (wait-time (+ (ratio-fn ts) offset) bus))) offsets)
        (ratio-fn ts)
        (recur (inc ts))))))

(defn part-two
  ([] (part-two data))
  ([input]
   (let [table (build-wait-table (:schedule input))
         ts (find-gold table)]
     (+ ts (->> (:offsets table) (map :true-offset) (apply min))))))
