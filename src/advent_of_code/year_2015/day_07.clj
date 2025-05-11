(ns advent-of-code.year-2015.day-07
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day07.txt"))
(def test-input "123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i")

(defn get-or-n
  [state v]
  (let [n (re-find #"\d+" v)]
    (if n
      (read-string n)
      (get state v))))

(defn parse-line
  [line]
  (cond
    ;; LSHIFT
    (re-find #"(\w+) LSHIFT (\d+) -> ([a-zA-Z]+)" line)
    (let [[_ from n to] (re-find #"(\w+) LSHIFT (\d+) -> ([a-zA-Z]+)" line)
          n (read-string n)]
      {:need (filter (fn [f] (re-find #"[a-zA-Z]+" f)) [from])
       :fn   (fn [state]
               (assoc state to (mod (bit-shift-left (get-or-n state from) n)
                                    65536)))})

    ;; RSHIFT
    (re-find #"(\w+) RSHIFT (\d+) -> ([a-zA-Z]+)" line)
    (let [[_ from n to] (re-find #"(\w+) RSHIFT (\d+) -> ([a-zA-Z]+)" line)
          n (read-string n)]
      {:need (filter (fn [f] (re-find #"[a-zA-Z]+" f)) [from])
       :fn   (fn [state]
               (assoc state to (mod (bit-shift-right (get-or-n state from) n)
                                    65536)))})

    ;; NOT
    (re-find #"NOT (\w+) -> ([a-zA-Z]+)" line)
    (let [[_ from to] (re-find #"NOT (\w+) -> ([a-zA-Z]+)" line)]
      {:need (filter (fn [f] (re-find #"[a-zA-Z]+" f)) [from])
       :fn   (fn [state]
               (assoc state to (mod (bit-not (get-or-n state from))
                                    65536)))})

    ;; AND
    (re-find #"(\w+) AND (\w+) -> ([a-zA-Z]+)" line)
    (let [[_ from1 from2 to] (re-find #"(\w+) AND (\w+) -> ([a-zA-Z]+)" line)]
      {:need (filter (fn [f] (re-find #"[a-zA-Z]+" f)) [from1 from2])
       :fn   (fn [state]
               (assoc state to (mod (bit-and (get-or-n state from1) (get-or-n state from2))
                                    65536)))})

    ;; OR
    (re-find #"(\w+) OR (\w+) -> ([a-zA-Z]+)" line)
    (let [[_ from1 from2 to] (re-find #"(\w+) OR (\w+) -> ([a-zA-Z]+)" line)]
      {:need (filter (fn [f] (re-find #"[a-zA-Z]+" f)) [from1 from2])
       :fn   (fn [state]
               (assoc state to (mod (bit-or (get-or-n state from1) (get-or-n state from2))
                                    65536)))})

    ;; value given directly
    :else
    (let [[_ from to] (re-find #"(\w+) -> ([a-zA-Z]+)" line)]
      {:need (filter (fn [f] (re-find #"[a-zA-Z]+" f)) [from])
       :fn   (fn [state]
               (assoc state to (get-or-n state from)))})))

(defn instruction-ready?
  [state instruction]
  (every? (fn [k]
            (contains? state k))
          (:need instruction)))

(defn do-instructions
  {:test (fn []
           (let [test-circuit (do-instructions test-input)]
             (is= (get test-circuit "d") 72)
             (is= (get test-circuit "h") 65412)
             (is= (get test-circuit "y") 456)))}
  [input]
  (let [instructions (->> (clojure.string/split-lines input)
                          (map parse-line))]
    (loop [state {}
           instructions instructions
           tested-instructions []]
      (cond (and (empty? instructions)
                 (empty? tested-instructions))
            state

            (empty? instructions)
            (recur state tested-instructions [])

            :else
            (let [[instruction & instructions] instructions]
              (if-not (instruction-ready? state instruction)
                (recur state instructions (conj tested-instructions instruction))
                (recur ((:fn instruction) state) instructions tested-instructions)))))))

(defn part-1
  [input]
  (-> (do-instructions input)
      (get "a")))

(defn part-2
  [input]
  (let [a-value (part-1 input)
        input-with-override (clojure.string/replace input #"\w+ -> b(?!\w)" (str a-value " -> b"))]
    (part-1 input-with-override)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 22.675041 msecs"
  ;=> 46065

  (time (part-2 input))
  ;; "Elapsed time: 26.60925 msecs"
  ;=> 14134
  )
