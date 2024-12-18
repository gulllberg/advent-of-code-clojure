(ns advent-of-code.year-2024.day-09
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day09.txt"))
(def test-input "2333133121414131402")

(defn initialize-drive
  [input]
  (->> input
       (re-seq #"\d")
       (map read-string)
       (reduce (fn [[file-or-space id a] d]
                 (if (= file-or-space :space)
                   [:file id (reduce conj a (repeat d nil))]
                   [:space (inc id) (reduce conj a (repeat d id))]))
               [:file 0 []])
       (last)))

;; https://stackoverflow.com/questions/8641305/find-index-of-an-element-matching-a-predicate-in-clojure/8642069
(defn find-first-index
  {:test (fn []
           (is= (find-first-index pos? [-1 0 99 100 101]) 2)
           (is= (find-first-index pos? [-1 -2 -3]) 3))}
  [pred coll]
  (or (first (keep-indexed #(when (pred %2) %1) coll)) (count coll)))

(defn move-all-files-by-part
  [drive]
  (loop [drive drive
         min-space-index (find-first-index nil? drive)
         max-file-index (- (count drive) 1 (find-first-index (fn [v] (not (nil? v))) (reverse drive)))]
    (cond
      (<= max-file-index min-space-index) drive
      (nil? (nth drive max-file-index)) (recur drive min-space-index (dec max-file-index))
      (not (nil? (nth drive min-space-index))) (recur drive (inc min-space-index) max-file-index)
      :else (recur (-> drive
                       (assoc min-space-index (nth drive max-file-index))
                       (assoc max-file-index nil))
                   (inc min-space-index)
                   (dec max-file-index)))))

(defn calculate-checksum
  [drive]
  (reduce-kv (fn [a i v]
               (if (nil? v)
                 a
                 (+ a (* i v))))
             0
             drive))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 1928))}
  [input]
  (-> input
      (initialize-drive)
      (move-all-files-by-part)
      (calculate-checksum)))

(defn get-free-space-positions
  [drive file-size file-start-position]
  (loop [free-space-positions nil
         i 0]
    (cond
      (= (count free-space-positions) file-size) free-space-positions
      (= i file-start-position) nil
      (not (nil? (nth drive i))) (recur nil (inc i))
      (nil? free-space-positions) (recur [i] (inc i))
      :else (recur (conj free-space-positions i) (inc i)))))

(defn move-entire-file
  [drive file-id]
  (let [file-positions (reduce-kv (fn [a i v]
                                    (cond
                                      (= v file-id) (conj a i)
                                      (empty? a) a
                                      :else (reduced a)))
                                  []
                                  drive)
        file-size (count file-positions)
        free-space-positions (get-free-space-positions drive file-size (first file-positions))]
    (if-not free-space-positions
      drive
      (as-> drive $
            (reduce (fn [a i]
                      (assoc a i nil))
                    $
                    file-positions)
            (reduce (fn [a i]
                      (assoc a i file-id))
                    $
                    free-space-positions)))))

(defn move-all-files-by-entire-files
  [drive]
  (loop [drive drive
         file-id-to-move (some (fn [v] (when (not (nil? v)) v)) (reverse drive))]
    (if (zero? file-id-to-move)
      drive
      (recur (move-entire-file drive file-id-to-move) (dec file-id-to-move)))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 2858))}
  [input]
  (-> input
      (initialize-drive)
      (move-all-files-by-entire-files)
      (calculate-checksum)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 76.324708 msecs"
  ;=> 6337921897505

  (time (part-2 input))
  ;; "Elapsed time: 11573.763584 msecs"
  ;=> 6362722604045
  )
