(ns advent-of-code.year-2019.day-14
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.math]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day14.txt"))
(def test-input "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL")
(def test-input-2 "157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")

(defn parse-input
  [input]
  (->> (clojure.string/split-lines input)
       (reduce (fn [a line]
                 (let [quantities (map read-string (re-seq #"\d+" line))
                       chemicals (re-seq #"[A-Z]+" line)
                       needs (->> (map vector (butlast chemicals) (butlast quantities))
                                  (map (fn [[chemical quantity]]
                                         {:chemical chemical :quantity quantity})))]
                   (assoc a (last chemicals) {:quantity (last quantities)
                                              :needs    needs})))
               {})))

(defn produce-fuel
  ([reactions]
   (produce-fuel reactions 1))
  ([reactions quantity]
   (loop [needs [{:quantity quantity :chemical "FUEL"}]
          excess {}
          ore-needed 0]
     (if (empty? needs)
       ore-needed
       (let [{chemical :chemical quantity :quantity} (first needs)]
         (cond
           (= chemical "ORE")
           (recur (rest needs)
                  excess
                  (+ ore-needed quantity))

           (>= (get excess chemical 0) quantity)
           (recur (rest needs)
                  (update excess chemical - quantity)
                  ore-needed)

           :else
           (let [quantity-needed-to-produce (- quantity (get excess chemical 0))
                 reaction-quantity (get-in reactions [chemical :quantity])
                 reaction-needs (get-in reactions [chemical :needs])
                 number-of-reactions-needed (long (clojure.math/ceil (/ quantity-needed-to-produce reaction-quantity)))
                 quantity-produced (* number-of-reactions-needed reaction-quantity)]
             (recur (concat (rest needs) (map (fn [n]
                                                (update n :quantity * number-of-reactions-needed))
                                              reaction-needs))
                    (assoc excess chemical (- quantity-produced quantity-needed-to-produce))
                    ore-needed))))))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 31)
           (is= (part-1 test-input-2) 13312))}
  [input]
  (-> (parse-input input)
      (produce-fuel)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input-2) 82892753))}
  [input]
  (let [ore-budget 1000000000000
        reactions (parse-input input)
        ore-for-one-fuel (produce-fuel reactions 1)
        low (quot ore-budget ore-for-one-fuel)
        high (->> (range)
                  (rest)
                  (map (fn [factor] (* factor low)))
                  (some (fn [fuel-to-produce]
                          (when (< ore-budget (produce-fuel reactions fuel-to-produce))
                            fuel-to-produce))))]
    (loop [low low
           high high]
      (if (= (inc low) high)
        low
        (let [fuel (quot (+ high low) 2)
              ore (produce-fuel reactions fuel)]
          (if (< ore-budget ore)
            (recur low fuel)
            (recur fuel high)))))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 9.441708 msecs"
  ;=> 378929

  (time (part-2 input))
  ;; "Elapsed time: 154.706 msecs"
  ;=> 3445249
  )
