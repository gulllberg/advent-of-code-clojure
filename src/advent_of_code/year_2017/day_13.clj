(ns advent-of-code.year-2017.day-13
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2017/inputs/day13.txt"))

(defn update-firewall
  [firewall]
  (reduce (fn [firewall layer-number]
            (update firewall layer-number (fn [layer]
                                            (cond
                                              ;; Going up and reaching start
                                              (and (= (:current-position layer) 1)
                                                   (= (:direction layer) -1))
                                              (merge layer
                                                     {:current-position 2
                                                      :direction        1})
                                              ;; Going down and reaching end
                                              (and (= (:current-position layer) (:depth layer))
                                                   (= (:direction layer) 1))
                                              (merge layer
                                                     {:current-position (dec (:current-position layer))
                                                      :direction        -1})
                                              :else
                                              (merge layer
                                                     {:current-position (+ (:direction layer) (:current-position layer))})))))
          firewall
          (keys firewall)))

;; Doesn't need to simulate everything - see part 2
(defn part-1
  {:test (fn []
           (is= (part-1 "0: 3\n1: 2\n4: 4\n6: 4")
                24))}
  [input]
  (let [firewall (reduce (fn [firewall layer]
                           (assoc firewall (-> (clojure.string/split layer #":")
                                               (first)
                                               (read-string))
                                           {:depth            (-> (clojure.string/split layer #":")
                                                                  (second)
                                                                  (clojure.string/trim)
                                                                  (read-string))
                                            :current-position 1
                                            :direction        1}))
                         {}
                         (clojure.string/split input #"\n"))]
    (-> (reduce (fn [[firewall sum] position]
                  (if (= (get-in firewall [position :current-position])
                         1)
                    [(update-firewall firewall) (+ sum (* position (get-in firewall [position :depth])))]
                    [(update-firewall firewall) sum]))
                [firewall 0 0]
                (range (inc (last (sort (keys firewall))))))
        (last))))

(defn hits-firewall?
  {:test (fn []
           (is (hits-firewall? {0 3
                                1 2
                                4 4
                                6 4}
                               0)))}
  [firewall delay]
  (some (fn [layer-number]
          (= (mod (+ delay layer-number) (* 2 (dec (get firewall layer-number))))
             0))
        (keys firewall)))

(defn part-2
  {:test (fn []
           (is= (part-2 "0: 3\n1: 2\n4: 4\n6: 4")
                10))}
  [input]
  (let [firewall (reduce (fn [firewall layer]
                           (assoc firewall (-> (clojure.string/split layer #":")
                                               (first)
                                               (read-string))
                                           (-> (clojure.string/split layer #":")
                                               (second)
                                               (clojure.string/trim)
                                               (read-string))))
                         {}
                         (clojure.string/split input #"\n"))]
    (some (fn [delay]
            (if (hits-firewall? firewall delay)
              false
              delay))
          (range))))

(comment
  (time (part-1 input))
  ;; 1632
  ;; "Elapsed time: 7.941209 msecs"

  (time (part-2 input))
  ;; 3834136
  ;; "Elapsed time: 1543.504916 msecs"
  )