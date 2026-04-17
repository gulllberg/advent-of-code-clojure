(ns advent-of-code.year-2019.intcode
  (:require [ysera.test :refer [is= is is-not]]))

(defn third [[_ _ z]] z)

(def opcode->number-of-parameters {1  3
                                   2  3
                                   3  1
                                   4  1
                                   5  2
                                   6  2
                                   7  3
                                   8  3
                                   9  1
                                   99 0})

(def opcode->parameter-types {1  [:read :read :write]
                              2  [:read :read :write]
                              3  [:write]
                              4  [:read]
                              5  [:read :read]
                              6  [:read :read]
                              7  [:read :read :write]
                              8  [:read :read :write]
                              9  [:read]
                              99 []})

(defn parse-program
  [input]
  (->> (re-seq #"-?\d+" input)
       (map read-string)
       (into [])))

(defn program->memory
  [program]
  (zipmap (range) program))

(defn read-from-memory
  [memory position]
  (if (neg? position)
    (println "Error: Cannot read negative memory position: " position)
    (get memory position 0)))

(defn write-to-memory
  [memory position value]
  (assoc memory position value))

(defn get-opcode
  {:test (fn []
           (is= (get-opcode 1002) 2)
           (is= (get-opcode 1099) 99))}
  [instruction]
  (mod instruction 100))

;; 0 - position
;; 1 - immediate (value)
;; 2 - relative (like position, but with relative base)
(defn get-parameter-modes
  {:test (fn []
           (is= (get-parameter-modes 1002) [0 1 0])
           (is= (get-parameter-modes 1102) [1 1 0]))}
  [instruction]
  (let [opcode (get-opcode instruction)
        remaining (quot instruction 100)]
    (->> (opcode->number-of-parameters opcode)
         (range)
         (reduce (fn [[parameter-modes remaining] _]
                   [(conj parameter-modes (mod remaining 10)) (quot remaining 10)])
                 [[] remaining])
         (first))))

(defn get-parameters
  {:test (fn []
           (is= (get-parameters (program->memory [1002 4 3 4 33]) 1002 0 0) [33 3 4])
           (is= (get-parameters (program->memory [109 1 204 -1]) 204 2 1) [109]))}
  [memory instruction instruction-pointer relative-base]
  (let [opcode (get-opcode instruction)
        parameter-modes (get-parameter-modes instruction)
        parameter-types (opcode->parameter-types opcode)]
    (->> (range (count parameter-modes))
         (map (fn [index]
                (let [parameter-mode (nth parameter-modes index)
                      parameter-type (nth parameter-types index)
                      memory-value (read-from-memory memory (+ instruction-pointer 1 index))
                      relative-base-adjusted-memory-value (if (= 2 parameter-mode) (+ memory-value relative-base) memory-value)]
                  (if (or (= :write parameter-type)
                          (= 1 parameter-mode))
                    relative-base-adjusted-memory-value
                    (read-from-memory memory relative-base-adjusted-memory-value))))))))

(defn run-intcode-program
  {:test (fn []
           ;; Adds 1+1 and puts in first memory position
           (is= (-> (run-intcode-program [1 0 0 0 99])
                    (:memory)
                    (read-from-memory 0))
                2)
           ;; Addition and multiplication
           (let [memory (-> (run-intcode-program [1 1 1 4 99 5 6 0 99])
                            (:memory))]
             ;; [30 1 1 4 2 5 6 0 99]
             (is= (read-from-memory memory 0) 30)
             (is= (read-from-memory memory 4) 2))
           ;; Takes an input and outputs it
           (is= (-> (run-intcode-program [3 0 4 0 99] [42])
                    (:program-output)
                    (first))
                42)
           ;; Multiplies 3 and 33
           (is= (-> (run-intcode-program [1002 4 3 4 33])
                    (:memory)
                    (read-from-memory 4))
                ;; [1002 4 3 4 99]
                99)
           ;; Checks if input is equal to 8 (using position mode) and outputs 1 or 0
           (let [program [3 9 8 9 10 9 4 9 99 -1 8]]
             (is= (-> (run-intcode-program program [8])
                      (:program-output))
                  [1])
             (is= (-> (run-intcode-program program [42])
                      (:program-output))
                  [0]))
           ;; Checks if input is less than 8 (using position mode) and outputs 1 or 0
           (let [program [3 9 7 9 10 9 4 9 99 -1 8]]
             (is= (-> (run-intcode-program program [5])
                      (:program-output))
                  [1])
             (is= (-> (run-intcode-program program [8])
                      (:program-output))
                  [0]))
           ;; Checks if input is equal to 8 (using immediate mode) and outputs 1 or 0
           (let [program [3 3 1108 -1 8 3 4 3 99]]
             (is= (-> (run-intcode-program program [8])
                      (:program-output))
                  [1])
             (is= (-> (run-intcode-program program [42])
                      (:program-output))
                  [0]))
           ;; Checks if input is less than 8 (using immediate mode) and outputs 1 or 0
           (let [program [3 3 1107 -1 8 3 4 3 99]]
             (is= (-> (run-intcode-program program [5])
                      (:program-output))
                  [1])
             (is= (-> (run-intcode-program program [8])
                      (:program-output))
                  [0]))
           ;; Checks if the input was zero (using position mode) and outputs 0 or 1
           (let [program [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]]
             (is= (-> (run-intcode-program program [0])
                      (:program-output))
                  [0])
             (is= (-> (run-intcode-program program [42])
                      (:program-output))
                  [1]))
           ;; Checks if the input was zero (using immediate mode) and outputs 0 or 1
           (let [program [3 3 1105 -1 9 1101 0 0 12 4 12 99 1]]
             (is= (-> (run-intcode-program program [0])
                      (:program-output))
                  [0])
             (is= (-> (run-intcode-program program [42])
                      (:program-output))
                  [1]))
           ;; Checks a single input if smaller, equal to or larger than 8 and outputs 999, 1000 or 1001 respectively.
           (let [program [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                          1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                          999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]]
             (is= (-> (run-intcode-program program [5])
                      (:program-output))
                  [999])
             (is= (-> (run-intcode-program program [8])
                      (:program-output))
                  [1000])
             (is= (-> (run-intcode-program program [42])
                      (:program-output))
                  [1001]))
           ;; Takes a setting 5-9 and an input, then runs and wait for more input
           (let [program [3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26 27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5]
                 result (run-intcode-program program [5 0])]
             (is= (:reason result) :waiting-for-input)
             (is (:memory result))
             (is (:program-output result))
             (is (:instruction-pointer result)))
           ;; Outputs a copy of itself
           (let [program [109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99]]
             (is= (-> (run-intcode-program program)
                      (:program-output))
                  program))
           ;; Outputs 16-digit number
           (is= (-> (run-intcode-program [1102 34915192 34915192 7 4 7 99 0])
                    (:program-output)
                    (first)
                    (str)
                    (count))
                16)
           ;; Outputs the large number in program
           (is= (-> (run-intcode-program [104 1125899906842624 99])
                    (:program-output)
                    (first))
                1125899906842624))}
  ([program]
   (run-intcode-program program []))
  ([program program-input]
   (run-intcode-program (program->memory program) 0 program-input []))
  ([memory instruction-pointer program-input program-output]
   (loop [memory memory
          instruction-pointer instruction-pointer
          program-input program-input
          program-output program-output
          relative-base 0]
     (let [instruction (read-from-memory memory instruction-pointer)
           opcode (get-opcode instruction)
           parameters (get-parameters memory instruction instruction-pointer relative-base)]
       (condp = opcode
         99 {:memory         memory
             :program-output program-output
             :reason         :halted}
         1 (recur (write-to-memory memory (third parameters) (+ (first parameters) (second parameters)))
                  (+ 4 instruction-pointer)
                  program-input
                  program-output
                  relative-base)
         2 (recur (write-to-memory memory (third parameters) (* (first parameters) (second parameters)))
                  (+ 4 instruction-pointer)
                  program-input
                  program-output
                  relative-base)
         3 (if-let [input (first program-input)]
             (recur (write-to-memory memory (first parameters) input)
                    (+ 2 instruction-pointer)
                    (rest program-input)
                    program-output
                    relative-base)
             {:memory              memory
              :program-output      program-output
              :instruction-pointer instruction-pointer
              :reason              :waiting-for-input})
         4 (recur memory
                  (+ 2 instruction-pointer)
                  program-input
                  (conj program-output (first parameters))
                  relative-base)
         5 (recur memory
                  (if (not (zero? (first parameters)))
                    (second parameters)
                    (+ 3 instruction-pointer))
                  program-input
                  program-output
                  relative-base)
         6 (recur memory
                  (if (zero? (first parameters))
                    (second parameters)
                    (+ 3 instruction-pointer))
                  program-input
                  program-output
                  relative-base)
         7 (recur (write-to-memory memory (third parameters) (if (< (first parameters) (second parameters)) 1 0))
                  (+ 4 instruction-pointer)
                  program-input
                  program-output
                  relative-base)
         8 (recur (write-to-memory memory (third parameters) (if (= (first parameters) (second parameters)) 1 0))
                  (+ 4 instruction-pointer)
                  program-input
                  program-output
                  relative-base)
         9 (recur memory
                  (+ 2 instruction-pointer)
                  program-input
                  program-output
                  (+ relative-base (first parameters)))
         (println "Invalid instruction" instruction))))))
