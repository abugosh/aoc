(ns aoc.2020.day-19
  [:require
   [clojure.string :as s]])

(defn parse-cond [cond]
  (if (s/ends-with? cond "}")
    (subs cond 0 (dec (count cond)))
    (let [[_ a op b dest] (re-matches #"(\w+)(.)(\d+):(\w+)" cond)]
      [op a b dest])))

(defn parse-rule [rule]
  (let [[rule-name conds] (s/split rule #"\{")
        conds (s/split conds #",")]
    [(keyword rule-name) (map parse-cond conds)]))

(defn parse-part [part]
  (let [[x m a s] (map parse-long (re-seq #"\d+" part))]
    {:x x :m m :a a :s s}))

(def data
  (let [[rules _ parts] (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
                             s/split-lines
                             (partition-by #{""}))]
    {:rules (into {} (map parse-rule rules)) :parts (map parse-part parts)}))

(defn rule-fn [rule]
  (let [conds (->> rule (drop-last 1) (map (partial apply format "(%s %s %s) :%s")))
        default (str ":" (last rule))]
    (-> "(fn [{:keys [x m a s]}] (cond
              %s
              :else %s))"
        (format (s/join "\n" conds) default)
        read-string
        eval)))

(defn part-accepted? [rules part]
  (loop [cur :in]
    (let [out ((rules cur) part)]
      (cond
        (= out :A) true
        (= out :R) false
        :else (recur out)))))

(defn part-one
  ([] (part-one data))
  ([{rules :rules parts :parts}]
   (let [rules (update-vals rules rule-fn)]
     (->> parts
          (filter (partial part-accepted? rules))
          (mapcat vals)
          (apply +)))))

(defn refine-rule [rule]
  (let [conds (->> rule (drop-last 1) (mapv (fn [[op a b dest]] [(keyword op) (keyword a) (parse-long b) (keyword dest)])))
        default (keyword (last rule))]
    (conj conds default)))

(defn valid-part? [part]
  (->> part
       vals
       (every? (partial apply <))))

(defn apply-rule [rule part]
  (let [conds (->> rule (drop-last 1))
        default (last rule)]
    (loop [part part
           [[op a b dest :as cond] & conds] conds
           res []]
      (if (nil? cond)
        (if (valid-part? part) (conj res [default part]) res)
        (case op
          :> (let [gt (update part a (fn [[x y]] [(inc (max x b)) y]))
                   lt (update part a (fn [[x y]] [x (min y b)]))]
               (if (valid-part? gt)
                 (recur lt conds (conj res [dest gt]))
                 (recur part conds res)))
          :< (let [gt (update part a (fn [[x y]] [(max x b) y]))
                   lt (update part a (fn [[x y]] [x (dec (min y b))]))]
               (if (valid-part? lt)
                 (recur gt conds (conj res [dest lt]))
                 (recur part conds res))))))))

(defn accepted-combos [rules]
  (loop [[[rule part :as cur] & open] [[:in {:x [1 4000] :m [1 4000] :a [1 4000] :s [1 4000]}]]
         accepted []]
    (cond
      (nil? cur) accepted
      (= :A rule) (recur open (conj accepted part))
      (= :R rule) (recur open accepted)
      :else (recur (concat open (apply-rule (rules rule) part)) accepted))))

(defn part-two
  ([] (part-two data))
  ([{rules :rules}]
   (->> (update-vals rules refine-rule)
        accepted-combos
        (map (fn [x] (->> x vals (map (fn [[a b]] (inc (- b a)))) (apply *))))
        (apply +))))
