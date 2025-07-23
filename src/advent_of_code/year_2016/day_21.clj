(ns advent-of-code.year-2016.day-21
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2016/inputs/day21.txt"))

(def password "abcdefgh")
(def scrambled-password "fbgdceah")

(defn get-letter-at-index
  {:test (fn []
           (is= (get-letter-at-index {"a" 0 "b" 1} 0) "a"))}
  [s i]
  (loop [ks (keys s)]
    (let [[k & ks] ks]
      (if (= (get s k) i)
        k
        (recur ks)))))

(defn swap-position
  {:test (fn []
           (is= (swap-position {"a" 0 "b" 1} 0 1) {"a" 1 "b" 0}))}
  [s x y]
  (let [x-letter (get-letter-at-index s x)
        y-letter (get-letter-at-index s y)]
    (assoc s x-letter y
             y-letter x)))

(defn swap-letter
  {:test (fn []
           (is= (swap-letter {"a" 0 "b" 1} "a" "b") {"a" 1 "b" 0}))}
  [s x y]
  (assoc s x (get s y)
           y (get s x)))

(defn rotate-right
  {:test (fn []
           (is= (rotate-right {"a" 0 "b" 1 "c" 2} 2) {"a" 2 "b" 0 "c" 1}))}
  [s steps]
  (let [mod-v (inc (apply max (vals s)))]
    (reduce-kv (fn [a k v]
                 (assoc a k (mod (+ v steps) mod-v)))
               {}
               s)))

(defn rotate-left
  {:test (fn []
           (is= (rotate-left {"a" 0 "b" 1 "c" 2} 1) {"a" 2 "b" 0 "c" 1}))}
  [s steps]
  (rotate-right s (- steps)))

(defn rotate-on-letter-position
  {:test (fn []
           (is= (rotate-on-letter-position {"a" 0 "b" 1 "c" 2} "b") {"a" 2 "b" 0 "c" 1}))}
  [s x]
  (let [index (get s x)
        steps (if (>= index 4)
                (+ 2 index)
                (inc index))]
    (rotate-right s steps)))

(defn reverse-x-to-y
  {:test (fn []
           (is= (reverse-x-to-y {"a" 0 "b" 1 "c" 2} 0 2) {"a" 2 "b" 1 "c" 0})
           (is= (reverse-x-to-y {"a" 7 "b" 5 "c" 6 "d" 0 "e" 3 "g" 1 "h" 2 "f" 4} 0 3) {"a" 7 "b" 5 "c" 6 "d" 3 "e" 0 "g" 2 "h" 1 "f" 4}))}
  [s x y]
  (let [midpoint (/ (+ x y) 2)]
    (reduce-kv (fn [a k v]
                 (if (or (< v x)
                         (> v y))
                   (assoc a k v)
                   (let [new-position (if (<= v midpoint)
                                        (- y (- v x))
                                        (+ x (- y v)))]
                     (assoc a k new-position))))
               {}
               s)))

(defn remove-letter
  {:test (fn []
           (is= (remove-letter {"a" 0 "b" 1 "c" 2} "b") {"a" 0 "c" 1}))}
  [s x]
  (let [remove-index (get s x)]
    (reduce-kv (fn [a k v]
                 (cond
                   (= k x) a
                   (< v remove-index) (assoc a k v)
                   :else (assoc a k (dec v))))
               {}
               s)))

(defn insert-letter
  {:test (fn []
           (is= (insert-letter {"a" 0 "c" 1} "b" 1) {"a" 0 "b" 1 "c" 2}))}
  [s x index]
  (-> (reduce-kv (fn [a k v]
                   (if (< v index)
                     (assoc a k v)
                     (assoc a k (inc v))))
                 {}
                 s)
      (assoc x index)))

(defn move
  {:test (fn []
           (is= (move {"a" 0 "b" 1 "c" 2} 0 2) {"a" 2 "b" 0 "c" 1})
           (is= (move {"a" 0 "b" 1 "c" 2} 2 0) {"a" 1 "b" 2 "c" 0}))}
  [s x y]
  (let [letter-to-move (get-letter-at-index s x)]
    (-> s
        (remove-letter letter-to-move)
        (insert-letter letter-to-move y))))

(defn create-password-state
  {:test (fn []
           (is= (create-password-state "abc") {"a" 0 "b" 1 "c" 2}))}
  [password]
  (reduce (fn [a i]
            (assoc a (str (nth password i)) i))
          {}
          (range (count password))))

(defn reconstruct-password
  {:test (fn []
           (is= (reconstruct-password {"e" 7 "f" 2 "h" 1 "c" 4 "g" 0 "b" 6 "a" 3 "d" 5}) "ghfacdbe"))}
  [s]
  (->> (keys s)
       (sort (fn [a b]
               (- (get s a) (get s b))))
       (reduce str))
  )

(defn do-instruction
  [s line]
  (cond
    ;; swap by position
    (re-find #"swap position (\d+) with position (\d+)" line)
    (let [[_ x y] (re-find #"swap position (\d+) with position (\d+)" line)]
      (swap-position s (read-string x) (read-string y)))

    ;; swap by letter
    (re-find #"swap letter (\w) with letter (\w)" line)
    (let [[_ x y] (re-find #"swap letter (\w) with letter (\w)" line)]
      (swap-letter s x y))

    ;; rotate right
    (re-find #"rotate right (\d+) steps?" line)
    (let [[_ x] (re-find #"rotate right (\d+) steps?" line)]
      (rotate-right s (read-string x)))

    ;; rotate left
    (re-find #"rotate left (\d+) steps?" line)
    (let [[_ x] (re-find #"rotate left (\d+) steps?" line)]
      (rotate-left s (read-string x)))

    ;; rotate (right) based on letter position
    (re-find #"rotate based on position of letter (\w)" line)
    (let [[_ x] (re-find #"rotate based on position of letter (\w)" line)]
      (rotate-on-letter-position s x))

    ;; reverse
    (re-find #"reverse positions (\d+) through (\d+)" line)
    (let [[_ x y] (re-find #"reverse positions (\d+) through (\d+)" line)]
      (reverse-x-to-y s (read-string x) (read-string y)))

    ;; move
    (re-find #"move position (\d+) to position (\d+)" line)
    (let [[_ x y] (re-find #"move position (\d+) to position (\d+)" line)]
      (move s (read-string x) (read-string y)))

    :else (throw (Exception. (str "NO REGEXP MATCH FOR LINE " line)))))

(defn part-1
  [input]
  (let [s (create-password-state password)]
    (-> (reduce do-instruction s (clojure.string/split-lines input))
        (reconstruct-password))))

(defn reverse-rotate-on-letter-position
  {:test (fn []
           (is= (reverse-rotate-on-letter-position {"a" 0 "b" 1 "c" 2} "b") {"a" 2 "b" 0 "c" 1}))}
  [s x]
  ;; There is a 1-to-1 mapping between where letter x starts before a rotate-on-letter-position and where it ends up
  ;; [0 1 2 3 4 5 6 7] -> [1 3 5 7 2 4 6 0]
  ;; So based on the position in string to reverse, it would be possible to look up (or maybe calculate somehow?) how many steps to rotate left to undo.
  ;; But trial and error is easy too :)
  (loop [steps 0]
    (let [maybe-s (rotate-left s steps)]
      (if (= (rotate-on-letter-position maybe-s x) s)
        maybe-s
        (recur (inc steps))))))

(defn do-instruction-reverse
  [s line]
  (cond
    ;; reverse swap by position -> change position arguments
    (re-find #"swap position (\d+) with position (\d+)" line)
    (let [[_ x y] (re-find #"swap position (\d+) with position (\d+)" line)]
      (swap-position s (read-string y) (read-string x)))

    ;; reverse swap by letter -> change letter arguments
    (re-find #"swap letter (\w) with letter (\w)" line)
    (let [[_ x y] (re-find #"swap letter (\w) with letter (\w)" line)]
      (swap-letter s y x))

    ;; reverse rotate right -> rotate left
    (re-find #"rotate right (\d+) steps?" line)
    (let [[_ x] (re-find #"rotate right (\d+) steps?" line)]
      (rotate-left s (read-string x)))

    ;; reverse rotate left -> rotate right
    (re-find #"rotate left (\d+) steps?" line)
    (let [[_ x] (re-find #"rotate left (\d+) steps?" line)]
      (rotate-right s (read-string x)))

    ;; reverse rotate (right) based on letter position -> rotate left, and figure out number of steps (new function)
    (re-find #"rotate based on position of letter (\w)" line)
    (let [[_ x] (re-find #"rotate based on position of letter (\w)" line)]
      (reverse-rotate-on-letter-position s x))

    ;; reverse reverse -> reverse again
    (re-find #"reverse positions (\d+) through (\d+)" line)
    (let [[_ x y] (re-find #"reverse positions (\d+) through (\d+)" line)]
      (reverse-x-to-y s (read-string x) (read-string y)))

    ;; reverse move -> change position arguments
    (re-find #"move position (\d+) to position (\d+)" line)
    (let [[_ x y] (re-find #"move position (\d+) to position (\d+)" line)]
      (move s (read-string y) (read-string x)))

    :else (throw (Exception. (str "NO REGEXP MATCH FOR LINE " line)))))

(defn part-2
  {:test (fn []
           ;; unscrambling part-1 result should give part-1 starting password
           (is= (part-2 input "ghfacdbe") password))}
  [input scrambled-password]
  (let [s (create-password-state scrambled-password)]
    (-> (reduce do-instruction-reverse s (reverse (clojure.string/split-lines input)))
        (reconstruct-password))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 0.868292 msecs"
  ;=> "ghfacdbe"

  (time (part-2 input scrambled-password))
  ;; "Elapsed time: 0.939458 msecs"
  ;=> "fhgcdaeb"
  )
