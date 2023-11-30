(ns aoc.2020.day-21
  (:require
   [aoc.utils :as u]
   [clojure.core.match :refer [match]]
   [clojure.string :as s]))

(def op-table {"+" +
               "-" -
               "*" *
               "/" /})

(defn rule
  ([_ const] const)
  ([id op t1 t2]
   (fn [state]
     (let [t1 (state t1)
           t2 (state t2)]
       (if (and (number? t1) (number? t2))
         (assoc! state id (op t1 t2))
         state)))))

(defn parse-rule [line]
  (let [[b id t1 op t2] (re-matches #"(\w+): (\w+) ([\+\-\*/]) (\w+)" line)]
    (and b
         [[id (op-table op) t1 t2]])))

(defn parse-axiom [line]
  (let [[b id n] (re-matches #"(\w+): (\d+)" line)]
    (and b
         [[id (u/parse-int n)]])))

(def data
  (->> (slurp "resources/2022/day-21.txt")
       s/split-lines
       (mapcat #(or (parse-rule %) (parse-axiom %)))))

(def exp-data (->> "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"
                   s/split-lines
                   (mapcat #(or (parse-rule %) (parse-axiom %)))))

(defn simplify [state]
  (persistent! (reduce-kv (fn [m _ v] (if (fn? v)
                                        (v m)
                                        m))
                          (transient state) state)))

(defn simplify-for [state id]
  (loop [state state]
    (if-not (fn? (state id))
      state
      (recur (simplify state)))))

(defn part-one
  ([] (part-one data))
  ([input]
   (-> (into {} (map (juxt first (partial apply rule)) input))
       (simplify-for "root")
       (get "root"))))

(defn solve [state guess]
  (-> state
      (assoc "humn" guess)
      (simplify-for "root")))

(defn check-range [state start end]
  (some (fn [x] (let [state (solve state x)]
                   (and (state "root-2")
                        (state "humn")))) (range start end)))

(defn tree-rule
  ([_ const] const)
  ([_ op t1 t2] [op t1 t2]))

(def not-coll? (comp coll?))

(defn replace-vars [rules]
  (reduce-kv (fn [m k v]
               (if-not (coll? v)
                 (assoc m k v)
                 (let [[op t1 t2] v
                       p1 (get rules t1 t1)
                       p2 (get rules t2 t2)]
                   (if (and (number? p1) (number? p2))
                     (assoc m k (op p1 p2))
                     (assoc m k [op
                                 (if (number? p1) p1 t1)
                                 (if (number? p2) p2 t2)])))))
             {} rules))

(defn clean-vars [rules]
  (reduce-kv (fn [m k v]
               (if (number? v)
                 m
                 (assoc m k v)))
             {} rules))

(def simplify-table {+ :+
                     - :-
                     * :*
                     / :/
                     = :=})

(defn simplify-rule [rules rule]
  (let [rule-val (rules rule)]
    (if (coll? rule-val)
      (let [[op t1 t2] rule-val
            t1 (simplify-rule rules t1)
            t2 (simplify-rule rules t2)]
        [(simplify-table op) t1 t2])
      (or rule-val rule))))

(defn build-rules [rules]
  (-> (into {} (map (juxt first (partial apply tree-rule)) rules))
      (assoc "humn" :humn)
      (update "root" #(-> % rest (conj =)))))

(defn simplify-eq [rules]
  (loop [rules (build-rules rules)]
    (let [new-rules (replace-vars rules)]
      (if (= new-rules rules)
        (clean-vars rules)
        (recur new-rules)))))

(defn oracle [humn]
  (=
   (*
    5
    (-
     40172078470474
     (/
      (+
       427
       (+
        87
        (*
         (+
          (/
           (-
            (*
             2
             (+
              (/
               (-
                (*
                 11
                 (-
                  (/
                   (+
                    399
                    (/
                     (+
                      (*
                       (-
                        (/
                         (+
                          356
                          (*
                           2
                           (-
                            (*
                             10
                             (+
                              610
                              (/
                               (-
                                (/
                                 (-
                                  (*
                                   2
                                   (+
                                    516
                                    (*
                                     (+
                                      (/
                                       (+
                                        (-
                                         (/
                                          (+
                                           (*
                                            (-
                                             (*
                                              3
                                              (-
                                               (/
                                                (+
                                                 611
                                                 (*
                                                  (+
                                                   (/
                                                    (+
                                                     (+
                                                      (-
                                                       (*
                                                        (-
                                                         (/
                                                          (+
                                                           (+
                                                            (*
                                                             (+
                                                              857
                                                              (/
                                                               (+
                                                                425
                                                                (-
                                                                 (/
                                                                  (+
                                                                   (*
                                                                    7
                                                                    (-
                                                                     (/
                                                                      (-
                                                                       (*
                                                                        9
                                                                        (+
                                                                         humn
                                                                         446))
                                                                       308)
                                                                      11)
                                                                     306))
                                                                   797)
                                                                  2)
                                                                 910))
                                                               3))
                                                             14)
                                                            759)
                                                           984)
                                                          7)
                                                         910)
                                                        14)
                                                       937)
                                                      180)
                                                     345)
                                                    2)
                                                   907)
                                                  2))
                                                9)
                                               185))
                                             539)
                                            3)
                                           872)
                                          11)
                                         404)
                                        467)
                                       10)
                                      362)
                                     85)))
                                  655)
                                 7)
                                783)
                               2)))
                            159)))
                         2)
                        137)
                       2)
                      818)
                     2))
                   10)
                  479))
                339)
               10)
              289))
            155)
           9)
          677)
         19)))
      2)))
   90565407195785))

(defn reverse-apply [base other]
  (match [other]
         [:humn] base
         [[:+ (a :guard number?) b]] (reverse-apply (- base a) b)
         [[:+ a (b :guard number?)]] (reverse-apply (- base b) a)

         [[:- (a :guard number?) b]] (reverse-apply (- a base) b)
         [[:- a (b :guard number?)]] (reverse-apply (+ base b) a)

         [[:* (a :guard number?) b]] (reverse-apply (/ base a) b)
         [[:* a (b :guard number?)]] (reverse-apply (/ base b) a)

         ;; [[:/ (a :guard number?) b]]
         ;; (reverse-apply (* base a) b)
         [[:/ a (b :guard number?)]] (reverse-apply (* base b) a)
         ))

(defn check-range [start end]
  (some #(and (oracle %) %) (range start end)))

(defn part-two
  ([] (part-two data))
  ([input]
   ;; (some #(and (oracle %) %) (range))
   (->> 1000
        range
        (some (fn [x]
                (some #(and (number? %) %)
                      (pvalues (check-range x (* x 10000))
                               (check-range (* x 10000) (* x 20000))
                               (check-range (* x 20000) (* x 30000))
                               (check-range (* x 30000) (* x 40000))
                               (check-range (* x 40000) (* x 50000))
                               (check-range (* x 50000) (* x 60000))
                               (check-range (* x 60000) (* x 70000))
                               (check-range (* x 70000) (* x 80000))
                               (check-range (* x 80000) (* x 90000))
                               (check-range (* x 90000) (* x 100000)))))))
   ))
