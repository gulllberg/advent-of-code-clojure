(ns advent-of-code.year-2024.day-24
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day24.txt"))
(def test-input "x00: 1\nx01: 1\nx02: 1\ny00: 0\ny01: 1\ny02: 0\n\nx00 AND y00 -> z00\nx01 XOR y01 -> z01\nx02 OR y02 -> z02")
(def test-input-2 "x00: 1\nx01: 0\nx02: 1\nx03: 1\nx04: 0\ny00: 1\ny01: 1\ny02: 1\ny03: 1\ny04: 1\n\nntg XOR fgs -> mjb\ny02 OR x01 -> tnw\nkwq OR kpj -> z05\nx00 OR x03 -> fst\ntgd XOR rvg -> z01\nvdt OR tnw -> bfw\nbfw AND frj -> z10\nffh OR nrd -> bqk\ny00 AND y03 -> djm\ny03 OR y00 -> psh\nbqk OR frj -> z08\ntnw OR fst -> frj\ngnj AND tgd -> z11\nbfw XOR mjb -> z00\nx03 OR x00 -> vdt\ngnj AND wpb -> z02\nx04 AND y00 -> kjc\ndjm OR pbm -> qhw\nnrd AND vdt -> hwm\nkjc AND fst -> rvg\ny04 OR y02 -> fgs\ny01 AND x02 -> pbm\nntg OR kjc -> kwq\npsh XOR fgs -> tgd\nqhw XOR tgd -> z09\npbm OR djm -> kpj\nx03 XOR y03 -> ffh\nx00 XOR y04 -> ntg\nbfw OR bqk -> z06\nnrd XOR fgs -> wpb\nfrj XOR qhw -> z04\nbqk OR frj -> z07\ny03 OR x01 -> nrd\nhwm AND bqk -> z03\ntgd XOR rvg -> z12\ntnw OR pbm -> gnj")

(defn parse-input
  [input]
  (let [[initial-values-part gates-part] (clojure.string/split input #"\n\n")
        state-without-initial-values (reduce-kv (fn [state idx line]
                                                  (let [[in1 op in2 out] (rest (re-find #"(.*) (.*) (.*) -> (.*)" line))
                                                        gate-id (str "gate-" idx)]
                                                    (-> state
                                                        (assoc-in [:gates gate-id] {:op  op
                                                                                    :in1 in1
                                                                                    :in2 in2
                                                                                    :out out})
                                                        (update-in [:wires in1] (fn [old]
                                                                                  (let [old (or old {:value           nil
                                                                                                     :gets-value-from nil
                                                                                                     :gives-value-to  #{}})]
                                                                                    (update old :gives-value-to conj gate-id))))
                                                        (update-in [:wires in2] (fn [old]
                                                                                  (let [old (or old {:value           nil
                                                                                                     :gets-value-from nil
                                                                                                     :gives-value-to  #{}})]
                                                                                    (update old :gives-value-to conj gate-id))))
                                                        (update-in [:wires out] (fn [old]
                                                                                  (let [old (or old {:value           nil
                                                                                                     :gets-value-from nil
                                                                                                     :gives-value-to  #{}})]
                                                                                    (assoc old :gets-value-from gate-id)))))))
                                                {:gates {}
                                                 :wires {}}
                                                (into [] (clojure.string/split-lines gates-part)))]
    (reduce (fn [state line]
              (let [[wire-id value-string] (rest (re-find #"(.*): (0|1)" line))]
                (-> state
                    (assoc-in [:wires wire-id :value] (read-string value-string))
                    (update :initial-values conj wire-id))))
            state-without-initial-values
            (clojure.string/split-lines initial-values-part))))

(defn and-gate
  {:test (fn []
           (is= (and-gate 1 0) 0)
           (is= (and-gate 1 1) 1))}
  [in1 in2]
  (if (and (= 1 in1) (= 1 in2))
    1
    0))

(defn or-gate
  {:test (fn []
           (is= (or-gate 1 0) 1)
           (is= (or-gate 0 0) 0))}
  [in1 in2]
  (if (or (= 1 in1) (= 1 in2))
    1
    0))

(defn xor-gate
  {:test (fn []
           (is= (xor-gate 1 0) 1)
           (is= (xor-gate 1 1) 0))}
  [in1 in2]
  (if (not= in1 in2)
    1
    0))

(defn gate
  {:test (fn []
           (is= (gate "AND" 1 0) 0))}
  [op in1 in2]
  (case op
    "AND" (and-gate in1 in2)
    "OR" (or-gate in1 in2)
    "XOR" (xor-gate in1 in2)))

(defn simulate-system
  {:test (fn []
           (let [settled-state (simulate-system (parse-input test-input))]
             (is= (get-in settled-state [:wires "z00" :value]) 0)
             (is= (get-in settled-state [:wires "z01" :value]) 0)
             (is= (get-in settled-state [:wires "z02" :value]) 1)))}
  [state]
  (loop [wires-with-change (:initial-values state)
         state (dissoc state :initial-values)]
    (if (empty? wires-with-change)
      state
      (let [[state wires-with-change] (reduce (fn [[state wires-with-change] wire-id-to-check]
                                                (let [gate-ids-to-check (get-in state [:wires wire-id-to-check :gives-value-to])]
                                                  (reduce (fn [[state wires-with-change] gate-id-to-check]
                                                            (let [gate-to-check (get-in state [:gates gate-id-to-check])
                                                                  in1 (get-in state [:wires (:in1 gate-to-check) :value])
                                                                  in2 (get-in state [:wires (:in2 gate-to-check) :value])
                                                                  gate-ready (and in1
                                                                                  in2)]
                                                              (if-not gate-ready
                                                                [state wires-with-change]
                                                                [(assoc-in state [:wires (:out gate-to-check) :value] (gate (:op gate-to-check) in1 in2))
                                                                 (conj wires-with-change (:out gate-to-check))])))
                                                          [state wires-with-change]
                                                          gate-ids-to-check)))
                                              [state #{}]
                                              wires-with-change)]
        (recur wires-with-change state)))))

(defn get-output-binary-string
  [state]
  (->> state
       (:wires)
       (keys)
       (filter (fn [k] (clojure.string/starts-with? k "z")))
       (sort)
       (reverse)
       (reduce (fn [a wire-id]
                 (str a (get-in state [:wires wire-id :value])))
               "")))

(defn get-output-number
  [state]
  (->> state
       (get-output-binary-string)
       (str "2r")
       (read-string)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 4)
           (is= (part-1 test-input-2) 2024))}
  [input]
  (-> input
      (parse-input)
      (simulate-system)
      (get-output-number)))

(defn zero-pad-string
  {:test (fn []
           (is= (zero-pad-string "01" 5) "00001"))}
  [s length]
  (let [padding (apply str (repeat (- length (count s)) "0"))]
    (str padding s)))

(defn number->padded-binary-string
  {:test (fn []
           (is= (number->padded-binary-string 5 5) "00101"))}
  [n length]
  (zero-pad-string (Long/toString n 2) length))

(defn number->padded-string
  {:test (fn []
           (is= (number->padded-string 1 2) "01")
           (is= (number->padded-string 45 2) "45"))}
  [n length]
  (zero-pad-string (str n) length))

(defn clear-values
  [state]
  (update state :wires (fn [wires]
                         (update-vals wires (fn [wire]
                                              (assoc wire :value nil))))))

(defn set-input-values-from-binary-strings
  [state x-binary-string y-binary-string length]
  (let [x-binary-string (zero-pad-string (clojure.string/reverse x-binary-string) length)
        y-binary-string (zero-pad-string (clojure.string/reverse y-binary-string) length)]
    (reduce (fn [state idx]
              (let [x-wire (str "x" (number->padded-string idx 2))
                    y-wire (str "y" (number->padded-string idx 2))]
                (-> state
                    (assoc-in [:wires x-wire :value] (read-string (str (nth x-binary-string idx))))
                    (assoc-in [:wires y-wire :value] (read-string (str (nth y-binary-string idx)))))))
            (clear-values state)
            (range length))))

(defn set-input-values
  [state x y length]
  (let [x-binary-string (number->padded-binary-string x length)
        y-binary-string (number->padded-binary-string y length)]
    (set-input-values-from-binary-strings state x-binary-string y-binary-string length)))

(defn add-numbers
  [state x y length]
  (-> (set-input-values state x y length)
      (simulate-system)
      (get-output-number)))

(defn add-binary-strings
  [state x-binary-string y-binary-string length]
  (-> (set-input-values-from-binary-strings state x-binary-string y-binary-string length)
      (simulate-system)
      (get-output-binary-string)))

(defn swap-output-wires
  [state g1 g2]
  (let [w1 (get-in state [:gates g1 :out])
        w2 (get-in state [:gates g2 :out])]
    (-> state
        (assoc-in [:gates g1 :out] w2)
        (assoc-in [:gates g2 :out] w1)
        (assoc-in [:wires w1 :gets-value-from] g2)
        (assoc-in [:wires w2 :gets-value-from] g1))))

; https://www.101computing.net/binary-additions-using-logic-gates/
(defn parse-half-adder
  [state in1 in2]
  (let [gates (get-in state [:wires in1 :gives-value-to])]
    (cond
      (not= gates (get-in state [:wires in2 :gives-value-to]))
      (println in1 "and" in2 "point to different gates" gates (get-in state [:wires in2 :gives-value-to]))

      (not= (count gates) 2)
      (println in1 "and" in2 "point to wrong number of gates" gates)

      :else
      (let [gate1 (get-in state [:gates (first gates)])
            gate2 (get-in state [:gates (second gates)])
            sum-gate (first (filter (fn [g] (= (:op g) "XOR")) [gate1 gate2]))
            carry-gate (first (filter (fn [g] (= (:op g) "AND")) [gate1 gate2]))]
        (if (or (nil? sum-gate) (nil? carry-gate))
          (println "incorrect gates" in1 in2 gate1 gate2)

          (if (clojure.string/starts-with? (:out carry-gate) "z")
            (println "z wire incorrectly used as carry wire" carry-gate sum-gate)
            {:carry-wire (:out carry-gate)
             :sum-wire   (:out sum-gate)}))))))

(defn parse-full-adder
  [state in1 in2 carry-in]
  (let [in-half-adder (parse-half-adder state in1 in2)]
    (if-not in-half-adder
      (println "incorrect in half-adder for" in1 in2 carry-in)
      (let [sum-half-adder (parse-half-adder state carry-in (:sum-wire in-half-adder))]
        (cond
          (not sum-half-adder)
          (println "incorrect sum half-adder for" in1 in2 carry-in (:sum-wire in-half-adder))

          :else
          (let [carry-wire-1 (get-in state [:wires (:carry-wire in-half-adder)])
                carry-wire-2 (get-in state [:wires (:carry-wire sum-half-adder)])]
            (cond
              (not= (:gives-value-to carry-wire-1) (:gives-value-to carry-wire-2))
              (println "carry wires not pointing to same gate" in1 in2 (:carry-wire in-half-adder) carry-wire-1 (:carry-wire sum-half-adder) carry-wire-2)

              (not= 1 (count (:gives-value-to carry-wire-1)))
              (println "carry wires pointing to more than 1 gate" in1 in2 carry-wire-1 carry-wire-2)

              :else
              (let [carry-gate (get-in state [:gates (first (:gives-value-to carry-wire-1))])]
                (if (not= (:op carry-gate) "OR")
                  (println "carry gate is not OR" in1 in2 carry-gate)
                  {:sum-wire   (:sum-wire sum-half-adder)
                   :carry-wire (:out carry-gate)})))))))))

(defn circuit-diagnostics
  [state]
  (loop [n-to-check 1
         last-result (parse-half-adder state "x00" "y00")]
    (cond
      (= n-to-check 46)
      (println "finished - no errors")

      (not last-result)
      (println "failed previous - terminating" n-to-check)

      (not= (:sum-wire last-result) (str "z" (number->padded-string (dec n-to-check) 2)))
      (println "incorrect sum wire last time" n-to-check last-result)

      :else
      (recur (inc n-to-check)
             (parse-full-adder state
                               (str "x" (number->padded-string n-to-check 2))
                               (str "y" (number->padded-string n-to-check 2))
                               (:carry-wire last-result))))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 6.74 msecs"
  ;=> 58740594706150

  (circuit-diagnostics (parse-input input))
  ; incorrect sum wire last time 15 {:sum-wire hbk, :carry-wire z14}
  (get-in (parse-input input) [:wires "z14"])
  ; :gets-value-from "gate-187"
  (get-in (parse-input input) [:wires "hbk"])
  ; :gets-value-from "gate-6"
  ; -> should they be the other way around?

  (circuit-diagnostics (swap-output-wires (parse-input input) "gate-187" "gate-6"))
  ; z wire incorrectly used as carry wire {:op AND, :in1 y18, :in2 x18, :out z18} {:op XOR, :in1 x18, :in2 y18, :out grp}
  ; incorrect in half-adder for x18 y18 fgr
  (get-in (parse-input input) [:wires "z18"])
  ; {:value nil, :gets-value-from "gate-84", :gives-value-to #{}}
  (get-in (parse-input input) [:wires "grp"])
  ; {:value nil, :gets-value-from "gate-24", :gives-value-to #{"gate-183" "gate-138"}}
  (get-in (parse-input input) [:gates "gate-24"])
  (get-in (parse-input input) [:gates "gate-183"])
  (get-in (parse-input input) [:gates "gate-138"])
  (let [state (swap-output-wires (parse-input input) "gate-187" "gate-6")]
    ;(get-in state [:wires "ffb"])
    (get-in state [:gates "gate-84"])
    )
  ; maybe 84 and 138 are wrong

  (-> (parse-input input)
      (swap-output-wires "gate-187" "gate-6")
      (swap-output-wires "gate-138" "gate-84")
      (circuit-diagnostics))
  ; z wire incorrectly used as carry wire {:op AND, :in1 dvw, :in2 rpg, :out z23} {:op XOR, :in1 dvw, :in2 rpg, :out dbb}
  ; incorrect sum half-adder for x23 y23 dvw rpg
  (let [state (-> (parse-input input)
                  (swap-output-wires "gate-187" "gate-6")
                  (swap-output-wires "gate-138" "gate-84"))]
    (get-in state [:wires "dbb"])
    ;(get-in state [:gates "gate-35"])
    )
  ; -> maybe 55 and 35 are wrong

  (-> (parse-input input)
      (swap-output-wires "gate-187" "gate-6")
      (swap-output-wires "gate-138" "gate-84")
      (swap-output-wires "gate-55" "gate-35")
      (circuit-diagnostics))
  ; mqf and tfn point to different gates #{gate-177 gate-73} #{gate-182}
  ; incorrect sum half-adder for x34 y34 mqf tfn
  (let [state (-> (parse-input input)
                  (swap-output-wires "gate-187" "gate-6")
                  (swap-output-wires "gate-138" "gate-84")
                  (swap-output-wires "gate-55" "gate-35"))]
    (get-in state [:wires "trj"])
    ;(get-in state [:gates "gate-73"])
    )
  ; 180 and 155 wrong

  (-> (parse-input input)
      (swap-output-wires "gate-187" "gate-6")
      (swap-output-wires "gate-138" "gate-84")
      (swap-output-wires "gate-55" "gate-35")
      (swap-output-wires "gate-180" "gate-155")
      (circuit-diagnostics))
  ; finished - no errors

  (let [state (parse-input input)
        gates-with-error ["gate-187"
                          "gate-6"
                          "gate-138"
                          "gate-84"
                          "gate-55"
                          "gate-35"
                          "gate-180"
                          "gate-155"]]
    (->> gates-with-error
         (map (fn [g] (get-in state [:gates g :out])))
         (sort)
         (clojure.string/join ",")))
  ;; Part 2
  ;; cvh,dbb,hbk,kvn,tfn,z14,z18,z23

  (let [state (-> (parse-input input)
                  (swap-output-wires "gate-187" "gate-6")
                  (swap-output-wires "gate-138" "gate-84")
                  (swap-output-wires "gate-55" "gate-35")
                  (swap-output-wires "gate-180" "gate-155"))]
    (let [n1 35184372088831
          n2 1123456]
      (= (add-numbers state n1 n2 45)
         (+ n1 n2)))
    )
  )

