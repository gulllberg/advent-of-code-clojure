(ns advent-of-code.year-2021.day-17)

(def input (slurp "src/advent_of_code/year_2021/inputs/day17.txt"))

(defn parse-input
  [input]
  (map read-string (re-seq #"-?\d+" input)))

(defn one-step
  [x dx y dy]
  [(+ x dx) (max 0 (dec dx)) (+ y dy) (dec dy)])

(defn in-target?
  [x y xfrom xto yfrom yto]
  (and (<= xfrom x xto) (<= yfrom y yto)))

(defn get-dx-min
  [xfrom xto]
  (loop [dx 0]
    (let [xfinal (/ (* dx (inc dx)) 2)]
      (if (<= xfrom xfinal xto)
        dx
        (recur (inc dx))))))

(defn get-dx-max
  [xto]
  xto)

(defn get-y-max
  [dy]
  (loop [y 0
         dy dy]
    (if (= 0 dy)
      y
      (recur (+ y dy) (dec dy)))))

(defn hits-target?
  [dx dy xfrom xto yfrom yto]
  (loop [x 0
         dx dx
         y 0
         dy dy]
    (if (in-target? x y xfrom xto yfrom yto)
      true
      (if (or (< y yfrom) (> x xto))
        false
        (let [[x dx y dy] (one-step x dx y dy)]
          (recur x dx y dy))))))

(defn find-highest-trajectory
  [xfrom xto yfrom yto]
  (loop [dy 0
         dx (get-dx-min xfrom xto)
         ymax 0]
    (if (= dy 1000)
      ymax
      (if (> dx (get-dx-max xto))
        (recur (inc dy) (get-dx-min xfrom xto) ymax)
        (if (hits-target? dx dy xfrom xto yfrom yto)
          (recur dy (inc dx) (max ymax (get-y-max dy)))
          (recur dy (inc dx) ymax))))))

(defn solve-a
  []
  (apply find-highest-trajectory (parse-input input)))

(comment
  (solve-a)
  ; 4560
  )

(defn find-number-of-trajectories
  [xfrom xto yfrom yto]
  (loop [dy -1000
         dx (get-dx-min xfrom xto)
         number-of-working-trajectories 0]
    (if (= dy 1000)
      number-of-working-trajectories
      (if (> dx (get-dx-max xto))
        (recur (inc dy) (get-dx-min xfrom xto) number-of-working-trajectories)
        (if (hits-target? dx dy xfrom xto yfrom yto)
          (recur dy (inc dx) (inc number-of-working-trajectories))
          (recur dy (inc dx) number-of-working-trajectories))))))

(defn solve-b
  []
  (apply find-number-of-trajectories (parse-input input)))

(comment
  (solve-b)
  ; 3344
  )
