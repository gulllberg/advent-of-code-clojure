(ns advent-of-code.year-2024.day-05
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day05.txt"))
(def test-input "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47")

(defn parse-input
  [input]
  (let [[h1 h2] (clojure.string/split input #"\n\n")
        page-ordering-rules (->> (clojure.string/split-lines h1)
                                 (reduce (fn [a line]
                                           (let [[before after] (->> (re-seq #"\d+" line)
                                                                     (map read-string))]
                                             (update a after (fn [old]
                                                               (if old
                                                                 (conj old before)
                                                                 #{before})))))
                                         {}))
        all-pages-to-produce (->> (clojure.string/split-lines h2)
                              (map (fn [line]
                                     (->> (re-seq #"\d+" line)
                                          (map read-string)))))]
    [page-ordering-rules all-pages-to-produce]))

(defn right-order?
  [page-ordering-rules pages-to-produce]
  (loop [produced #{}
         pages-to-produce pages-to-produce]
    (if (empty? pages-to-produce)
      true
      (let [page (first pages-to-produce)
            later-pages (rest pages-to-produce)]
        (if (and (contains? page-ordering-rules page)
                 (some (get page-ordering-rules page) later-pages))
          false
          (recur (conj produced page)
                 later-pages))))))

(defn get-middle-page-number
  [pages-to-produce]
  (nth pages-to-produce (/ (dec (count pages-to-produce)) 2)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 143))}
  [input]
  (let [[page-ordering-rules all-pages-to-produce] (parse-input input)]
    (->> all-pages-to-produce
         (keep (fn [pages-to-produce]
                 (when (right-order? page-ordering-rules pages-to-produce)
                   (get-middle-page-number pages-to-produce))))
         (reduce +))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 42))}
  [input]
  42)

(comment
  ;; "Elapsed time: 9.340125 msecs"
  ;=> 5651
  (time (part-1 input))

  ;;
  (time (part-2 input))
  )
