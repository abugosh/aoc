(ns aoc.2020.day-12
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(defn parse-direction [line]
  (let [[_ cmd unit] (re-matches #"(\w)(\d+)" line)]
    {:cmd cmd
     :unit (u/parse-int unit)}))

(def data
  (->> (slurp "resources/2020/day-12.txt")
       s/split-lines
       (map parse-direction)))

(defn follow-instructions [instructions]
  (reduce (fn [state {cmd :cmd unit :unit}]
            (case cmd
              "N" (update state :x + unit)
              "S" (update state :x - unit)
              "E" (update state :y + unit)
              "W" (update state :y - unit)
              "R" (update state :facing #(mod (+ % unit) 360))
              "L" (update state :facing #(mod (- % unit) 360))
              "F" (case (:facing state)
                    0 (update state :x + unit)
                    180 (update state :x - unit)
                    90 (update state :y + unit)
                    270 (update state :y - unit))))
          {:facing 90 :x 0 :y 0}
          instructions))

(defn part-one
  ([] (part-one data))
  ([input]
   (let [{x :x y :y} (follow-instructions input)]
     (+ (Math/abs x) (Math/abs y)))))

(defn follow-real-instructions [instructions]
  (reduce (fn [state {cmd :cmd unit :unit}]
            (case cmd
              "N" (update state :y + unit)
              "S" (update state :y - unit)
              "E" (update state :x + unit)
              "W" (update state :x - unit)
              "R" (case unit
                    0 state
                    90 (-> state
                           (assoc :x (:y state))
                           (assoc :y (- (:x state))))
                    180 (-> state
                            (assoc :x (- (:x state)))
                            (assoc :y (- (:y state))))
                    270 (-> state
                            (assoc :x (- (:y state)))
                            (assoc :y (:x state))))
              "L" (case unit
                    0 state
                    90 (-> state
                           (assoc :x (- (:y state)))
                           (assoc :y (:x state)))
                    180 (-> state
                            (assoc :x (- (:x state)))
                            (assoc :y (- (:y state))))
                    270 (-> state
                            (assoc :x (:y state))
                            (assoc :y (- (:x state)))))
              "F" (-> state
                      (update :ship-x #(+ % (* unit (:x state))))
                      (update :ship-y #(+ % (* unit (:y state)))))))
          {:x 10 :y 1 :ship-x 0 :ship-y 0}
          instructions))

(defn part-two
  ([] (part-two data))
  ([input]
   (let [{x :ship-x y :ship-y} (follow-real-instructions input)]
     (+ (Math/abs x) (Math/abs y)))))
