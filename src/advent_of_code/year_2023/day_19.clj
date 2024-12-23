(ns advent-of-code.year-2023.day-19
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2023/inputs/day19.txt"))
(def test-input "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}")

(defn check-condition
  [v op num x m a s]
  (condp = v
    "x" ((eval (read-string op)) x (read-string num))
    "m" ((eval (read-string op)) m (read-string num))
    "a" ((eval (read-string op)) a (read-string num))
    "s" ((eval (read-string op)) s (read-string num))))

(defn get-fn
  ;(:test (fn []
  ;         (is= (get-fn "px{a<2006:qkq,m>2090:A,rfg}")
  ;              {:px (fn [fns x m a s]
  ;                      (cond
  ;                          (< a 2006) ((get fns :qkq) fns x m a s)
  ;                          (> m 2090) :A
  ;                          :else ((get fns :rfg) fns x m a s))
  ;                     )})))
  [line]
  (let [[_ id conditions] (re-find #"(\w+)\{(.*)\}" line)
        conditions (clojure.string/split conditions #",")]
    {(keyword id) (fn [fns x m a s]
                    (loop [[condition & conditions] conditions]
                      (if (empty? conditions)
                        (if (contains? #{:A :R} (keyword condition))
                          (keyword condition)
                          ((get fns (keyword condition)) fns x m a s))
                        (let [[_ v op num res] (re-find #"(\w)(<|>)(\d+):(\w+)" condition)]
                          (if (check-condition v op num x m a s)
                            (if (contains? #{:A :R} (keyword res))
                              (keyword res)
                              ((get fns (keyword res)) fns x m a s))
                            (recur conditions))))))}))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 19114))}
  [input]
  (let [[conditions ratings] (clojure.string/split input #"\n\n")
        fns (reduce (fn [a c]
                      (merge a (get-fn c)))
                    {}
                    (clojure.string/split-lines conditions))]
    (reduce (fn [acc r]
              (let [[x m a s] (map read-string (re-seq #"\d+" r))
                    res ((get fns :in) fns x m a s)]
                (if (= res :A)
                  (+ acc x m a s)
                  acc)))
            0
            (clojure.string/split-lines ratings))))

;(def fns {:in  (fn [x-range m-range a-range s-range]
;                 (let [[truthy-range falsy-range] (get-new-ranges s-range 1351 "<")]
;                   [{:x x-range :m m-range :a a-range :s truthy-range :fn-to-run :px}
;                    {:x x-range :m m-range :a a-range :s falsy-range :fn-to-run :qqz}]))
;          :qqz (fn [x-range m-range a-range s-range]
;                 (let [[truthy-range falsy-range] (get-new-ranges s-range 2770 ">")]
;                   (let [[truthy-range-2 falsy-range-2] (get-new-ranges m-range 181 "<")]
;                     [{:x x-range :m m-range :a a-range :s truthy-range :fn-to-run :qs}
;                      {:x x-range :m truthy-range-2 :a a-range :s falsy-range :fn-to-run :hdj}
;                      {:x x-range :m falsy-range-2 :a a-range :s falsy-range :fn-to-run :R}])))})

; qqz{s>2770:qs,m<1801:hdj,R}
; in{s<1351:px,qqz}
;(defn run-one-fn
;  {:test (fn []
;           (is= (run-one-fn [{:x [1 4000] :m [1 4000] :a [1 4000] :s [1 4000] :fn-to-run :in}])
;                [{:x [1 4000] :m [1 4000] :a [1 4000] :s [1 1350] :fn-to-run :px}
;                 {:x [1 4000] :m [1 4000] :a [1 4000] :s [1351 4000] :fn-to-run :qqz}]))}
;  [intervals]
;  (reduce (fn [a interval]
;            (let [fn-to-run (get fns (:fn-to-run interval))]
;              (concat a (fn-to-run (:x interval) (:m interval) (:a interval) (:s interval)))))
;          []
;          intervals))

(defn get-conditions-state
  [conditions]
  (reduce (fn [a line]
            (let [[_ id conditions] (re-find #"(\w+)\{(.*)\}" line)]
              (assoc a (keyword id) conditions)))
          {}
          (clojure.string/split-lines conditions)))
(defn get-truthy-falsy-ranges
  {:test (fn []
           (is= (get-truthy-falsy-ranges [1 4000] 1351 "<")
                [[1 1350] [1351 4000]])
           (is= (get-truthy-falsy-ranges [1 1000] 1351 ">")
                [nil [1 1000]])
           (is= (get-truthy-falsy-ranges [1 1000] 777 ">")
                [[778 1000] [1 777]]))}
  [[range-start range-end] value op]
  (if (= op "<")
    (cond
      (< range-end value)
      [[range-start range-end] nil]

      (< value range-start)
      [nil [range-start range-end]]

      :else [[range-start (dec value)] [value range-end]])
    (cond
      (> range-start value)
      [[range-start range-end] nil]

      (> value range-end)
      [nil [range-start range-end]]

      :else [[(inc value) range-end] [range-start value]])))

(defn process-one-conditions
  {:test (fn []
           (is= (process-one-conditions "s<1351:px,qqz" {:x [1 4000] :m [1 4000] :a [1 4000] :s [1 4000]})
                [{:x [1 4000] :m [1 4000] :a [1 4000] :s [1 1350] :fn-to-run :px}
                 {:x [1 4000] :m [1 4000] :a [1 4000] :s [1351 4000] :fn-to-run :qqz}])
           (is= (process-one-conditions "s>2770:qs,m<1801:hdj,R" {:x [1 4000] :m [1 4000] :a [1 4000] :s [1351 4000]})
                [{:x [1 4000] :m [1 4000] :a [1 4000] :s [2771 4000] :fn-to-run :qs}
                 {:x [1 4000] :m [1 1800] :a [1 4000] :s [1351 2770] :fn-to-run :hdj}
                 {:x [1 4000] :m [1801 4000] :a [1 4000] :s [1351 2770] :fn-to-run :R}]))}
  [conditions interval]
  (reduce (fn [[finished processing] condition]
            (if (re-find #"^\w+$" condition)
              (conj finished (assoc processing :fn-to-run (keyword condition)))
              (let [[_ v op num res] (re-find #"(\w)(<|>)(\d+):(\w+)" condition)
                    [truthy-range falsy-range] (get-truthy-falsy-ranges (get processing (keyword v)) (read-string num) op)]
                (if-not falsy-range
                  (reduced (conj finished (assoc processing (keyword v) truthy-range) :fn-to-run (keyword res)))

                  [(if truthy-range (conj finished (assoc processing (keyword v) truthy-range :fn-to-run (keyword res))) finished)
                   (assoc processing (keyword v) falsy-range)]))))
          [[] interval]
          (clojure.string/split conditions #",")))

(defn get-number-accepted-from-ranges
  [interval]
  (->> (vals interval)
       (map (fn [[a b]]
              (inc (- b a))))
       (reduce *)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 167409079868000))}
  [input]
  (let [conditions (first (clojure.string/split input #"\n\n"))
        conditions-state (get-conditions-state conditions)]
    (loop [intervals [{:x [1 4000] :m [1 4000] :a [1 4000] :s [1 4000] :fn-to-run :in}]
           num-accepted 0]
      (if (empty? intervals)
        num-accepted
        (let [[interval & intervals] intervals
              fn-to-run (get interval :fn-to-run)]
          (condp = fn-to-run
            :R (recur intervals num-accepted)
            :A (recur intervals (+ num-accepted (get-number-accepted-from-ranges (dissoc interval :fn-to-run))))
            (recur (concat intervals (process-one-conditions (get conditions-state fn-to-run) interval))
                   num-accepted)))))))

(comment
  (time (part-1 input))
  ;; 367602
  ;; "Elapsed time: 18.701917 msecs"

  (time (part-2 input))
  ;; 125317461667458
  ;; "Elapsed time: 25.511445 msecs"
  )
