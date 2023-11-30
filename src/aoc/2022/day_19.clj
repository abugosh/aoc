(ns aoc.2020.day-19
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(defn parse-line [line]
  (let [[_ id ore-ore clay-ore obsidian-ore obsidian-clay geode-ore geode-obsidian] (re-matches #"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian." line)]
    {:id (u/parse-int id)
     :ore {:ore (u/parse-int ore-ore)}
     :clay {:ore (u/parse-int clay-ore)}
     :obsidian {:ore (u/parse-int obsidian-ore) :clay (u/parse-int obsidian-clay)}
     :geode {:ore (u/parse-int geode-ore) :obsidian (u/parse-int geode-obsidian)}}))

(def data
  (->> (slurp "resources/2022/day-19.txt")
       s/split-lines
       (map parse-line)))

(def exp-data (parse-line "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."))
(def exp-data2 (parse-line "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."))

(def base-state {:time 24
                 :bots {:do-nothing 0 :ore 1 :clay 0 :obsidian 0 :geode 0}
                 :resources {:ore 0 :clay 0 :obsidian 0 :geode 0}})

(defn score [state blue-print]
  (+ (* 100 (get-in state [:bots :geode]))
     (* 10 (min (* (:time state) (/ (get-in state [:bots :ore]) (get-in blue-print [:geode :ore])))
                (* (:time state) (/ (get-in state [:bots :obsidian]) (get-in blue-print [:geode :obsidian])))))))

(defn add-bot [state blue-print bot]
  (if (nil? state)
    nil
    (let [bot-print (blue-print bot)
          needed-time (->> (merge-with - bot-print (select-keys (:resources state) (keys bot-print)))
                           (map (fn [[b cost]] (let [rate (get-in state [:bots b])]
                                                 (if (= 0 rate)
                                                   (if (pos? cost)
                                                     1000
                                                     0)
                                                   (int (Math/ceil (/ cost rate)))))))
                           (apply max)
                           inc)
          new-state (-> state
                        (update :resources #(merge-with + % (update-vals (:bots state) (partial * needed-time))))
                        (update :resources #(merge-with - % bot-print))
                        (update-in [:bots bot] inc)
                        (update :time - needed-time))]
      (if (neg? (:time new-state))
        nil
        new-state))))

(defn finalize-state [state]
  (update state :resources #(merge-with + % (update-vals (:bots state) (partial * (:time state))))))

(defn possible-bots [state]
  (cond
    ;; (< 1 (get-in state [:bots :geode])) [:geode]
    (< 1 (get-in state [:bots :obsidian])) [:obsidian :geode]
    (< 1 (get-in state [:bots :clay])) [:clay :obsidian]
    :else [:ore :clay]))

(defn bfs [state blue-print]
  (let [bots [:geode :obsidian :clay :ore]]
    (loop [ ;; q (conj (clojure.lang.PersistentQueue/EMPTY) state)
           q [state]
           most-geodes 0]
      (let [state (peek q)]
        (cond
          (nil? state) most-geodes
          ;; (> most-geodes (* (:time state) (max 1 (get-in state [:bots :geode])))) (recur (pop q) most-geodes)
          ;; (= 0 (:time state))
          ;; (recur (pop q) (max most-geodes (get-in state [:resources :geode])))
          :else (recur (apply conj (pop q) (->> state
                                                possible-bots
                                                (map (partial add-bot state blue-print))
                                                (remove nil?)))
                       (max most-geodes (get-in (finalize-state state) [:resources :geode]))))))))

(defn build-bot-set [state blue-print bots]
  (let [end-state (reduce #(add-bot %1 blue-print %2) state bots)]
    (if end-state
      (finalize-state end-state)
      nil)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map (juxt :id (partial bfs base-state)))
        (map (partial apply *))
        (apply +))))

(defn part-two
  ([] (part-two data))
  ([input]
   nil))
