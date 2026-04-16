(ns advent-of-code.year-2019.day-08
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day08.txt"))

(defn part-1
  [input]
  (->> (seq input)
       (partition (* 25 6))
       (map frequencies)
       (sort-by (fn [x] (get x \0)))
       (first)
       ((fn [x] (* (get x \1) (get x \2))))))

(defn print-row
  [layers row-number]
  (->> (range 25)
       (map (fn [index]
              (some (fn [layer]
                      (let [c (-> layer
                                  (nth row-number)
                                  (nth index))]
                        (when (not= c \2)
                          c)))
                    layers)))
       (map (fn [c] (if (= c \1) "#" " ")))
       (apply str)
       (println)))

(defn part-2
  [input]
  (let [layers (->> (seq input)
                    (partition (* 25 6))
                    (map (fn [layer] (partition 25 layer))))]
    (doseq [row-number (range 6)]
      (print-row layers row-number))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 2.219416 msecs"
  ;=> 2016

  (time (part-2 input))
  ;; "Elapsed time: 7.500709 msecs"
  ;=> nil
  ;#  # ####  ##  #### #  #
  ;#  #    # #  #    # #  #
  ;####   #  #      #  #  #
  ;#  #  #   #     #   #  #
  ;#  # #    #  # #    #  #
  ;#  # ####  ##  ####  ##
  )
