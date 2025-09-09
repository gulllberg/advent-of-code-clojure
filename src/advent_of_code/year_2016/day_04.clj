(ns advent-of-code.year-2016.day-04
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2016/inputs/day04.txt"))
(def test-input "aaaaa-bbb-z-y-x-123[abxyz]\na-b-c-d-e-f-g-h-987[abcde]\nnot-a-real-room-404[oarel]\ntotally-real-room-200[decoy]")

(defn sort-letter-frequencies
  [freqs]
  (sort (fn [a b]
          (if (not= (get freqs a) (get freqs b))
            (- (get freqs b) (get freqs a))
            (compare a b)))
        (keys freqs)))

(defn real-room?
  {:test (fn []
           (is (real-room? "aaaaa-bbb-z-y-x-123[abxyz]"))
           (is (real-room? "a-b-c-d-e-f-g-h-987[abcde]"))
           (is (real-room? "not-a-real-room-404[oarel]"))
           (is-not (real-room? "totally-real-room-200[decoy]")))}
  [line]
  (let [parts (re-seq #"[a-z]+" line)
        letters (apply str (drop-last parts))
        checksum (last parts)]
    (= checksum
       (->> (frequencies letters)
            (sort-letter-frequencies)
            (take 5)
            (apply str)))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 1514))}
  [input]
  (reduce (fn [a line]
            (if (real-room? line)
              (+ a (read-string (re-find #"\d+" line)))
              a))
          0
          (clojure.string/split-lines input)))

(defn decrypt-room-name
  {:test (fn []
           (is= (decrypt-room-name "qzmt-zixmtkozy-ivhz-343[abcde]") "very encrypted name"))}
  [line]
  (let [sector-id (read-string (re-find #"\d+" line))
        encrypted-name-parts (->> (re-seq #"[a-z]+" line)
                                  (drop-last))]
    (->> encrypted-name-parts
         (map (fn [p]
                (->> p
                     (map (fn [c]
                              (-> (int c)
                                  (+ sector-id)
                                  (- (int \a))
                                  (mod 26)
                                  (+ (int \a))
                                  (char))))
                     (apply str))))
         (clojure.string/join " "))))

(defn part-2
  [input]
  (reduce (fn [a v]
            ;; check all room names manually to see which one sounds correct
            (println (decrypt-room-name v) v))
          0
          (clojure.string/split-lines input)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 22.283125 msecs"
  ;=> 137896

  (time (part-2 input))
  ;; "Elapsed time: 24.822875 msecs"
  ;; answer was 501
  )
