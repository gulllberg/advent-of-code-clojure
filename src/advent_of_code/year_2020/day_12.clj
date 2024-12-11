(ns advent-of-code.year-2020.day-12)

(def input (slurp "src/advent_of_code/year_2020/inputs/day12.txt"))

(defn create-state
  []
  {:east   0
   :north  0
   :facing 0})

(defn execute-movement
  [state direction number]
  (condp = direction
    "E" (update state :east + number)
    "W" (update state :east - number)
    "N" (update state :north + number)
    "S" (update state :north - number)))

(defn get-movement-direction
  [facing]
  (condp = (mod facing 360)
    0 "E"
    90 "N"
    180 "W"
    270 "S"))

(defn get-next-state
  [state line]
  (let [parts (re-seq #"[A-Z]+|\d+" line)
        instruction (first parts)
        number (read-string (second parts))]
    (condp = instruction
      "E" (execute-movement state instruction number)
      "W" (execute-movement state instruction number)
      "N" (execute-movement state instruction number)
      "S" (execute-movement state instruction number)
      "L" (update state :facing + number)
      "R" (update state :facing - number)
      "F" (execute-movement state (get-movement-direction (:facing state)) number))))

(defn part-1
  []
  (let [final-state (reduce get-next-state (create-state) (clojure.string/split-lines input))]
    (+ (Math/abs (:north final-state)) (Math/abs (:east final-state)))))

(comment
  (time (part-1))
  ; 2057
  ; "Elapsed time: 3.166042 msecs"
  )

(defn create-state-2
  []
  {:east           0
   :north          0
   :waypoint-east  10
   :waypoint-north 1})

(defn execute-waypoint-movement
  [state direction number]
  (condp = direction
    "E" (update state :waypoint-east + number)
    "W" (update state :waypoint-east - number)
    "N" (update state :waypoint-north + number)
    "S" (update state :waypoint-north - number)))

(defn execute-waypoint-rotation
  [state direction number]
  (if (= direction "R")
    (execute-waypoint-rotation state "L" (- number))
    (condp = (mod number 360)
      0 state
      ; Could have done these recursively
      90 (assoc state :waypoint-north (:waypoint-east state) :waypoint-east (- (:waypoint-north state)))
      180 (assoc state :waypoint-north (- (:waypoint-north state)) :waypoint-east (- (:waypoint-east state)))
      270 (assoc state :waypoint-north (- (:waypoint-east state)) :waypoint-east (:waypoint-north state)))))

(defn execute-ship-movement
  [state number]
  (assoc state :east (+ (:east state) (* number (:waypoint-east state)))
               :north (+ (:north state) (* number (:waypoint-north state)))))

(defn get-next-state-2
  [state line]
  (let [parts (re-seq #"[A-Z]+|\d+" line)
        instruction (first parts)
        number (read-string (second parts))]
    (condp = instruction
      "E" (execute-waypoint-movement state instruction number)
      "W" (execute-waypoint-movement state instruction number)
      "N" (execute-waypoint-movement state instruction number)
      "S" (execute-waypoint-movement state instruction number)
      "L" (execute-waypoint-rotation state instruction number)
      "R" (execute-waypoint-rotation state instruction number)
      "F" (execute-ship-movement state number))))

(defn part-2
  []
  (let [final-state (reduce get-next-state-2 (create-state-2) (clojure.string/split-lines input))]
    (+ (Math/abs (:north final-state)) (Math/abs (:east final-state)))))

(comment
  (time (part-2))
  ; 71504
  ; "Elapsed time: 3.219834 msecs"
  )
