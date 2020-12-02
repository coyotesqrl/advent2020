(ns advent2020
  (:require [clojure.math.combinatorics :as comb]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(defn read-input
  [filename]
  (->> filename
       io/resource
       io/reader
       line-seq))

(defn extract-addends
  [num-addends exp-sum input]
  (->> (comb/combinations input num-addends)
       (filter #(= exp-sum (apply + %)))
       first))

(defn count-chars
  [c s]
  (->> (char-array s)
       (filter #{c})
       count))

(defn password-rule-match-1
  [[min max ltr pwd]]
  (<= min (count-chars ltr pwd) max))

(defn password-rule-match-2
  [[idx1 idx2 ltr pwd]]
  (let [pwd-arr (char-array pwd)
        ch1 (get pwd-arr (- idx1 1))
        ch2 (get pwd-arr (- idx2 1))]
    (or
      (and
        (= ch1 ltr) (not= ch2 ltr))
      (and
        (= ch2 ltr) (not= ch1 ltr)))))

(defn read-password-line
  "Reads password line from db and returns min, max, ltr, pwd"
  [line]
  (let [mtc (re-matches #"(\d*)-(\d*)\s*(.):\s*(.*)$" line)]
    (list (Integer/parseInt (second mtc))
          (Integer/parseInt (nth mtc 2))
          (first (char-array (nth mtc 3)))
          (nth mtc 4))))

;
; Main entry functions below this line
;
(defn advent-1
  "Extract `num-addends` addends from expense report list that together sum to 2020, then
  multiplies them together."
  [num-addends input]
  (->> (read-input input)
       (map #(Integer/parseInt %))
       (extract-addends num-addends 2020)
       (apply *)))

(defn advent-2
  "Parses each line of the input and counts passwords that are valid given the `pw-match-fn`
  rule function."
  [pw-match-fn input]
  (->> (read-input input)
       (filter #(pw-match-fn (read-password-line %)))
       count))

(comment
  (advent-1 2 "day1.txt")
  (advent-1 3 "day1.txt")
  (advent-2 password-rule-match-1 "day2.txt")
  (advent-2 password-rule-match-2 "day2.txt")
  )

;
; Tests below this line
;
(defn -main []
  (run-tests 'advent2020))

(deftest find-addends
  (are [exp-result num-addends exp-sum input]
    (= exp-result (set (extract-addends num-addends exp-sum input)))
    #{1000 1020} 2 2020 '(1010 999 1020 100 1000)
    #{5 6 7} 3 18 '(18 8 10 9 5 19 6 20 7)))

(deftest test-password-parse
  (is (= '(1 3 \a "abcde") (read-password-line "1-3 a: abcde"))))

(deftest test-password-rule-matchers
  (are [matchfn pw valid] (= (matchfn pw) valid)
                          password-rule-match-1 '(1 3 \a "defaghi") true
                          password-rule-match-1 '(1 3 \a "defaghia") true
                          password-rule-match-1 '(1 3 \a "defaghiaxda") true
                          password-rule-match-1 '(1 3 \a "defaghiacaca") false
                          password-rule-match-2 '(3 6 \b "axbhkklaid") true
                          password-rule-match-2 '(3 6 \b "axb") true
                          password-rule-match-2 '(3 6 \b "xyakeb") true
                          password-rule-match-2 '(3 6 \b "xyakebbbb") true
                          password-rule-match-2 '(3 6 \b "xybkebbbb") false
                          password-rule-match-2 '(3 6 \b "xyckecccc") false
                          password-rule-match-2 '(3 6 \b "xyckecd") false))
