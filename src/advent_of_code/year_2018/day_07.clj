(ns advent-of-code.year_2018.day_07
  (:require [clojure.test]
            [clojure.set]))

(def input (slurp "src/advent_of_code/year_2018/inputs/day07.txt"))

(def test-input "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin.")

(defn parse-line
  {:test (fn []
           (clojure.test/is (= (parse-line "Step S must be finished before step G can begin.") ["S" "G"])))}
  [line]
  (let [words (clojure.string/split line #" ")]
    [(second words) (nth words 7)]))

(defn get-dependencies
  {:test (fn []
           (clojure.test/is (= (get-dependencies test-input)
                               {"C" #{}
                                "A" #{"C"}
                                "F" #{"C"}
                                "B" #{"A"}
                                "D" #{"A"}
                                "E" #{"B" "D" "F"}})))}
  [input]
  (reduce (fn [dependencies line]
            (let [[enabler depender] (parse-line line)]
              (-> dependencies
                  (update depender (fn [deps]
                                     (if (nil? deps)
                                       #{enabler}
                                       (conj deps enabler))))
                  (update enabler (fn [deps]
                                    (if (nil? deps)
                                      #{}
                                      deps))))))
          {}
          (clojure.string/split-lines input)))

(defn get-next-job
  {:test (fn []
           (clojure.test/is (= (get-next-job (get-dependencies test-input))
                               "C")))}
  [dependencies]
  (->> dependencies
       (keys)
       (sort)
       (some (fn [job]
               (when (= (get dependencies job) #{})
                 job)))))

(defn update-dependencies
  {:test (fn []
           (clojure.test/is (= (update-dependencies (get-dependencies test-input) "C")
                               {"A" #{}
                                "F" #{}
                                "B" #{"A"}
                                "D" #{"A"}
                                "E" #{"B" "D" "F"}}))
           (clojure.test/is (= (update-dependencies {"A" #{"C"}
                                                     "F" #{"C"}
                                                     "B" #{"A"}
                                                     "D" #{"A"}
                                                     "E" #{"B" "D" "F"}} "C")
                               {"A" #{}
                                "F" #{}
                                "B" #{"A"}
                                "D" #{"A"}
                                "E" #{"B" "D" "F"}})))}
  [dependencies job]
  (let [dependencies (dissoc dependencies job)]
    (reduce (fn [dependencies key]
              (update dependencies key disj job))
            dependencies
            (keys dependencies))))

(defn solve-a
  {:test (fn []
           (clojure.test/is (= (solve-a test-input)
                               "CABDFE")))}
  [input]
  (let [dependencies (get-dependencies input)]
    (first (reduce (fn [[order dependencies] _]
                     (let [next-job (get-next-job dependencies)]
                       [(str order next-job) (update-dependencies dependencies next-job)]))
                   ["" dependencies]
                   (range (count (keys dependencies)))))))

(comment
  (solve-a input)
  ;; FDSEGJLPKNRYOAMQIUHTCVWZXB
  )

(defn get-completion-time
  {:test (fn []
           (clojure.test/is (= (get-completion-time "A" 0)
                               61))
           (clojure.test/is (= (get-completion-time "A" 60)
                               1)))}
  [job time-subtraction]
  (-> job
      (first)
      (int)
      (- 4 time-subtraction)))

(defn can-assign-job?
  [dependencies workers]
  (and (get-next-job dependencies)
       (not (empty? (filter nil? workers)))))

(defn finished?
  [dependencies workers]
  (and (empty? dependencies)
       (empty? (remove nil? workers))))

(defn solve-b
  {:test (fn []
           (clojure.test/is (= (solve-b test-input 60 2)
                               15)))}
  [input time-subtraction number-of-workers]
  (reduce (fn [[dependencies workers time] _]
            (let [[dependencies workers] (reduce (fn [[dependencies workers] worker]
                                                   (if (nil? worker)
                                                     [dependencies (conj workers nil)]
                                                     (let [job (first worker)
                                                           remaining-time (dec (second worker))]
                                                       (if (= remaining-time 0)
                                                         [(update-dependencies dependencies job) (conj workers nil)]
                                                         [dependencies (conj workers [job remaining-time])]))))
                                                 [dependencies []]
                                                 workers)]
              (cond (finished? dependencies workers)
                    (reduced time)
                    (can-assign-job? dependencies workers)
                    (let [[dependencies workers] (reduce (fn [[dependencies workers] worker]
                                                           (if-not (nil? worker)
                                                             [dependencies (conj workers worker)]
                                                             (if-let [next-job (get-next-job dependencies)]
                                                               [(dissoc dependencies next-job) (conj workers [next-job (get-completion-time next-job time-subtraction)])]
                                                               [dependencies (conj workers worker)])))
                                                         [dependencies []]
                                                         workers)]
                      [dependencies workers (inc time)])
                    :else
                    [dependencies workers (inc time)])))
          [(get-dependencies input) (repeat number-of-workers nil) 0]
          (range)))

(comment
  (solve-b input 0 5)
  ;; 1000
  )
