(ns coyotesqrl.advent2020
  (:require [clojure.math.combinatorics :as comb]
            [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :refer [difference intersection union map-invert]]
            [clojure.math.numeric-tower :as math]
            [clojure.spec.alpha :as s]
            [loom.graph :as g]
            [instaparse.core :as insta]))

;
; Common functions. Functions here will be generally useful for all/many advent puzzles
;
(defn input->seq
  [filename]
  (->> (io/resource filename)
       io/reader
       line-seq))

(defn input->groups
  [input]
  (as-> (io/resource input) d
        (slurp d)
        (str/split d #"(\n){2}")))

(defn -main []
  (run-tests 'coyotesqrl.advent2020))

; ************
; Day 1
; ************
(defn extract-addends
  [num-addends exp-sum input]
  (->> (comb/combinations input num-addends)
       (filter #(= exp-sum (apply + %)))
       first))

(deftest find-addends
  (are [exp-result num-addends exp-sum input]
    (= exp-result (set (extract-addends num-addends exp-sum input)))
    #{1000 1020} 2 2020 '(1010 999 1020 100 1000)
    #{5 6 7} 3 18 '(18 8 10 9 5 19 6 20 7)))

(defn advent-1
  "Extract `num-addends` addends from expense report list that together sum to 2020, then
  multiplies them together."
  [num-addends input]
  (->> (input->seq input)
       (map #(Integer/parseInt %))
       (extract-addends num-addends 2020)
       (apply *)))

; ************
; Day 2
; ************
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
  (let [[_ min max ltr pwd] (re-matches #"(\d*)-(\d*)\s*(.):\s*(.*)$" line)]
    (list (Integer/parseInt min)
          (Integer/parseInt max)
          (first (char-array ltr))
          pwd)))

(deftest test-password-parse
  (is (= '(1 3 \a "abcde") (read-password-line "1-3 a: abcde"))))

(deftest test-password-rule-matchers
  (are [matchfn pw test-fn] (test-fn (matchfn pw))
                            password-rule-match-1 '(1 3 \a "defaghi") true?
                            password-rule-match-1 '(1 3 \a "defaghia") true?
                            password-rule-match-1 '(1 3 \a "defaghiaxda") true?
                            password-rule-match-1 '(1 3 \a "defaghiacaca") not
                            password-rule-match-2 '(3 6 \b "axbhkklaid") true?
                            password-rule-match-2 '(3 6 \b "axb") true?
                            password-rule-match-2 '(3 6 \b "xyakeb") true?
                            password-rule-match-2 '(3 6 \b "xyakebbbb") true?
                            password-rule-match-2 '(3 6 \b "xybkebbbb") not
                            password-rule-match-2 '(3 6 \b "xyckecccc") not
                            password-rule-match-2 '(3 6 \b "xyckecd") not))

(defn advent-2
  "Parses each line of the input and counts passwords that are valid given the `pw-match-fn`
  rule function."
  [pw-match-fn input]
  (->> (input->seq input)
       (filter #(pw-match-fn (read-password-line %)))
       count))

; ************
; Day 3
; ************
(defn line->num
  "Reads and flips an input line, interpreting . as 0 and # as 1."
  [line]
  (as-> (str/reverse line) s
        (str/replace s #"\." "0")
        (str/replace s #"#" "1")
        (Integer/parseInt s 2)))

(defn check-toboggan-row
  [x line]
  (not= 0 (bit-and (line->num line) (math/expt 2 x))))

(defn count-toboggan-collisions
  [x-step y-step input]
  (let [red (fn [[col cnt] b]
              (list (mod (+ col x-step) 31)
                    (if (check-toboggan-row col b)
                      (inc cnt)
                      cnt)))]
    (->> input
         (take-nth y-step)
         (reduce red '(0 0)))))

(deftest test-check-toboggan-row
  "Tests for positions within int boundary length. Does not handle modulo."
  (are [x row test-fn] (test-fn (check-toboggan-row x row))
                       0 ".#...#....#...#.#..........#.#." not
                       1 ".#...#....#...#.#..........#.#." true?
                       4 ".#...#....#...#.#..........#.#." not
                       5 ".#...#....#...#.#..........#.#." true?
                       28 ".#...#....#...#.#..........#.#." not
                       29 ".#...#....#...#.#..........#.#." true?
                       30 ".#...#....#...#.#..........#.#." not
                       31 ".#...#....#...#.#..........#.#." not))

(deftest test-toboggan-collisions
  (is (= '(24 4) (count-toboggan-collisions 3 2 (input->seq "test-day3.txt")))))

(defn advent-3
  [steps input]
  (let [input (input->seq input)]
    (->> (map #(count-toboggan-collisions (first %) (second %) input) steps)
         (map second)
         (apply *))))

; ************
; Day 4
; ************
(defn line->mapentries
  [l]
  (let [[ks vs]
        (->> (str/split l #"\s")
             (map #(str/split % #"[:]"))
             (apply map vector))]
    (zipmap (map keyword ks) vs)))

(defn input->passport
  [input]
  (reduce (fn
            [acc val]
            (if (str/blank? val)
              (conj acc {})
              (let [mp (first acc)]
                (conj (pop acc) (merge mp (line->mapentries val))))))
          '({})
          input))

(defn year-in-range?
  [min max input]
  (if (re-matches #"\d+" input)
    (as-> (Integer/parseInt input) yr
          (<= min yr max))
    false))

(defn hgt-in-range?
  [input]
  (when-let [[_ val unit] (re-matches #"(\d*)(cm|in)" input)]
    (if (= unit "cm")
      (<= 150 (Integer/parseInt val) 193)
      (<= 59 (Integer/parseInt val) 76))))

;
; Doubling up the key specs and using unqualified keys in my two map specs
; to allow for the two different sets of validation rules (puzzle #1 vs puzzle #2)
;
(s/def :coyotesqrl-p1/byr string?)
(s/def :coyotesqrl-p1/iyr string?)
(s/def :coyotesqrl-p1/eyr string?)
(s/def :coyotesqrl-p1/hgt string?)
(s/def :coyotesqrl-p1/hcl string?)
(s/def :coyotesqrl-p1/ecl string?)
(s/def :coyotesqrl-p1/pid string?)
(s/def :coyotesqrl-p1/cid string?)

(s/def :coyotesqrl-p2/byr #(year-in-range? 1920 2002 %))
(s/def :coyotesqrl-p2/iyr #(year-in-range? 2010 2020 %))
(s/def :coyotesqrl-p2/eyr #(year-in-range? 2020 2030 %))
(s/def :coyotesqrl-p2/hgt hgt-in-range?)
(s/def :coyotesqrl-p2/hcl #(re-matches #"#[0-9a-f]{6}" %))
(s/def :coyotesqrl-p2/ecl #(contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} %))
(s/def :coyotesqrl-p2/pid #(re-matches #"\d{9}" %))
(s/def :coyotesqrl-p2/cid string?)

(s/def :coyotesqrl-p1/passport (s/keys :req-un [:coyotesqrl-p1/byr :coyotesqrl-p1/iyr :coyotesqrl-p1/eyr
                                                :coyotesqrl-p1/hgt :coyotesqrl-p1/hcl :coyotesqrl-p1/ecl
                                                :coyotesqrl-p1/pid]
                                       :opt-un [:coyotesqrl-p1/cid]))

(s/def :coyotesqrl-p2/passport (s/keys :req-un [:coyotesqrl-p2/byr :coyotesqrl-p2/iyr :coyotesqrl-p2/eyr
                                                :coyotesqrl-p2/hgt :coyotesqrl-p2/hcl :coyotesqrl-p2/ecl
                                                :coyotesqrl-p2/pid]
                                       :opt-un [:coyotesqrl-p2/cid]))

(deftest test-year-in-range?
  (are [min max input test-fn] (test-fn (year-in-range? min max input))
                               1920 2000 "1950" true?
                               1920 2000 "2950" not
                               1920 2000 "1920" true?
                               1920 2000 "2000" true?
                               1920 2000 "1919" not
                               1920 2000 "195a" not))

(deftest test-hgt-in-range?
  (are [input test-fn] (test-fn (hgt-in-range? input))
                       "150cm" true?
                       "193cm" true?
                       "149cm" not
                       "194cm" not
                       "59in" true?
                       "76in" true?
                       "58in" not
                       "77in" not
                       "70xy" not
                       "70" not))

(defn advent-4
  [input spec]
  (->> (input->seq input)
       input->passport
       (filter #(s/valid? spec %))
       count))

; ************
; Day 5
; ************
(comment
  "Possible functions needed for day 6 if it extends on day 5"
  (defn get-plane-row
    [code]
    (bit-shift-right code 3))
  (defn get-plane-aisle
    [code]
    (bit-and 7 code)))

(defn boarding-pass->seat
  [bp]
  (as-> (seq bp) v
        (map {\F \0 \B \1 \L \0 \R \1} v)
        (apply str v)
        (Integer/parseInt v 2)))

(def all-seats
  (set (for [code (range 0 1031)]
         code)))

(defn advent-5-1
  [input]
  (->> (input->seq input)
       (map boarding-pass->seat)
       (apply max)))

(defn advent-5-2
  [input]
  (let [input (input->seq input)
        assigned (set (map boarding-pass->seat input))
        missing (difference all-seats assigned)]
    (first (filter #(and (contains? assigned (inc %))
                         (contains? assigned (dec %))) missing))))

; ************
; Day 6
; ************
(comment
  ; Day six ugly as usual. Would have improved solution had I realized
  (set "abc")
  ; decomposes the string to a set of characters.
  ; Would also have helped had I thought to use reduce more, as opposed to
  ; my endless stream of maps and applies.
  )

(defn customs-count-any
  [d]
  (->> (map #(apply str %) d)
       (map #(set (seq %)))
       (map count)
       (reduce +)))

(defn customs-count-all
  [d]
  (->> (map #(map (fn [param1] (seq param1)) %) d)
       (map #(map set %))
       (map #(apply intersection %))
       (map count)
       (reduce +)))

(defn advent-6
  [cnt-fn input]
  (->> (input->groups input)
       (map #(str/split % #"\n"))
       (cnt-fn)))

; ************
; Day 7
; ************

(defn line->bag-rules
  [l]
  (let [[k v] (str/split l #" bags contain ")
        v (->> (str/split v #" bag[s]?[,|.]\s?")
               (map #(re-matches #"(\d*) (.*)" %))
               (remove nil?)
               (map (fn [[_ v k]] {k (Integer/parseInt v)}))
               (into {}))]
    {k v}))

(defn input->bag-rules
  [input]
  (->> (input->seq input)
       (map line->bag-rules)
       (into {})))

(defn count-container-bags
  [bag-def input]
  (reduce-kv (fn [acc k v]
               (if (contains? (set (keys v)) bag-def)
                 (union acc #{k} (count-container-bags k input))
                 acc))
             #{} input))

(defn how-many-bags
  [bag-def input]
  (->> (get input bag-def)
       (reduce-kv #(+ %1 %3 (* %3 (how-many-bags %2 input))) 0)))

(defn advent-7-1
  [bag-def input]
  (->> (input->bag-rules input)
       (count-container-bags bag-def)
       count))

(defn advent-7-2
  [bag-def input]
  (->> (input->bag-rules input)
       (how-many-bags bag-def)))

; ************
; Day 8
; ************

(defn process-game-line
  "Processes a single instruction, updating the accumulator as appropriate
  and returning a tuple of it, and the next index to be processed."
  [line idx [visited sum]]
  (let [[_ inst op d] (re-matches #"([^\s]*) ([+|-])(\d*)" line)
        op (resolve (symbol op))
        d (Integer/parseInt d)
        visited (conj visited idx)]
    (cond
      (= inst "acc") [(inc idx) [visited (op sum d)]]
      (= inst "jmp") [(op idx d) [visited sum]]
      :else [(inc idx) [visited sum]])))

(defn run-game
  [input]
  (let [last-idx (dec (count input))]
    (loop [[next-idx acc] [0 [#{} 0]]]
      (if (or (= next-idx last-idx) (contains? (first acc) next-idx))
        (if (= next-idx last-idx)
          (->> (process-game-line (get input next-idx) next-idx acc) second second)
          (second acc))
        (recur (process-game-line (get input next-idx) next-idx acc))))))

(defn filter-inst-indexes
  [input]
  (->> (keep-indexed
         #(when (or (str/starts-with? %2 "nop") (str/starts-with? %2 "jmp"))
            %1)
         input)
       (into [])))

(defn swap-instr
  [input swap-idx]
  (let [line (get input swap-idx)]
    (->>
      (if (str/starts-with? line "nop")
        (str/replace line #"nop" "jmp")
        (str/replace line #"jmp" "nop"))
      (assoc input swap-idx))))

(defn walk-instr
  [input]
  (let [last-idx (dec (count input))
        swap (filter-inst-indexes input)]
    (loop [test-input input
           test-idx (first swap)
           test-swap swap
           next-idx 0
           visited []]
      (cond
        (= next-idx last-idx) test-idx
        (contains? visited next-idx) (let [[x & xs] test-swap]
                                       (recur (swap-instr input x) x xs 0 []))
        :else (let [[next [visited] _] (process-game-line (get test-input next-idx) next-idx [visited 0])]
                (recur test-input test-idx test-swap next visited))))))

(defn advent-8-1
  [input]
  (->> (input->seq input)
       vec
       run-game))

(defn advent-8-2
  [input]
  (let [input (vec (input->seq input))
        bad-inst (walk-instr input)]
    (->> (swap-instr input bad-inst)
         run-game)))

(deftest test-process-game-line
  (are [exp-result line idx acc]
    (= exp-result (process-game-line line idx acc))
    [7 [#{0 6} 3]] "nop +4" 6 [#{0} 3]
    [3 [#{0 1 8 6} 3]] "jmp -3" 6 [#{0 1 8} 3]
    [7 [#{0 4 5 6} 2]] "acc -2" 6 [#{0 4 5} 4]))

(deftest test-swap-instr
  (is (= ["acc +1" "jmp +2" "nop +3" "acc -1"]
         (swap-instr ["acc +1" "jmp +2" "jmp +3" "acc -1"] 2))))

(deftest test-filter-instructions
  (is (= [1 2] (filter-inst-indexes ["acc +1" "jmp +2" "nop +3" "acc -1"]))))

(deftest test-8-1
  (is (= 5 (advent-8-1 "test-day8.txt"))))

(deftest test-bad-instr
  (is (= 7 (->> (input->seq "test-day8.txt") vec walk-instr))))

(deftest test-8-2
  (is (= 8 (advent-8-2 "test-day8.txt"))))

; ************
; Day 9
; ************

(defn xmas-window
  [input]
  (let [input (vec input)]
    (reduce-kv (fn [acc k v] (conj acc [v (vec (map #(+ % v) (drop (inc k) input)))]))
               []
               input)))

(defn update-window
  [window x]
  (as-> (rest window) d
        (map #(vector (first %) (conj (second %) (+ x (first %)))) d)
        (concat d [[x []]])
        (vec d)))

(defn non-sum
  [wind-len input]
  (loop [input (vec input)
         window (xmas-window (take wind-len input))]
    (let [x (get input wind-len)
          val-sums (flatten (map second window))]
      (if (some #(= x %) val-sums)
        (recur (vec (rest input)) (update-window window x))
        x))))

(defn xmas-contiguous-sum
  [input exp-sum]
  (reduce (fn f [a v]
            (let [[sum coll] a]
              (if (>= (+ sum v) exp-sum)
                (if (= (+ sum v) exp-sum)
                  (reduced (conj coll v))
                  (reduced false))
                [(+ sum v) (conj coll v)])))
          [0 []]
          input))

(defn xmas-contiguous-sum-recur
  [input exp-sum]
  "Loops through `input` collection until it finds a contiguous set of values that
  sum to the `exp-sum`.
  If found, returns the collection; otherwise, nil."
  (loop [input input]
    (let [result (xmas-contiguous-sum input exp-sum)]
      (if result
        (if (some vector? result)
          nil
          result)
        (recur (rest input))))))

(defn advent-9-1
  [win-len input]
  (->> (input->seq input)
       (map #(Long/parseLong %))
       (non-sum win-len)))

(defn advent-9-2
  [win-len input]
  (let [input (->> (input->seq input) (map #(Long/parseLong %)) vec)]
    (as-> (non-sum win-len input) d
          (xmas-contiguous-sum-recur input d)
          (+ (apply min d) (apply max d)))))

(deftest test-9-1
  (is (= 127 (non-sum 5 (map #(Long/parseLong %) (input->seq "test-day9.txt"))))))

(deftest test-contiguous-sum
  (are [result coll sum] (= result (xmas-contiguous-sum coll sum))
                         false [19 2 3 4 5] 9
                         [2 3 4] [2 3 4 5] 9
                         [4 5] [4 5] 9))

(deftest test-contiguous-sum-recur
  (are [result coll sum] (= result (xmas-contiguous-sum-recur coll sum))
                         [2 3 4] [19 2 3 4 5] 9
                         [2 3 4] [1 2 3 4 5] 9
                         [3 4 5] [19 2 3 4 5] 12
                         nil [1 2 3] 100))

; ************
; Day 10
; ************
(defn count-all-adapters
  [input]
  (let [inc-fn #(assoc %1 %2 (inc (%2 %1)))]
    (->>
      (map-indexed #(if (= 0 %1) 0 (- %2 (get input (dec %1)))) input)
      (filter #(< 0 %))
      (reduce #(if (= 1 %2) (inc-fn %1 :1) (inc-fn %1 :3))
              {:1 1 :3 1}))))

(defn build-graph
  "Builds a DAG of the inputs. Each node includes its descendent paths as tuples, with
  the first value being the adjacent node and the second the target node. This is to aid
  in determining when a node is the target."
  [input]
  (let [graph (->> (for [x input
                         y input
                         :when (<= 1 (- y x) 3)]
                     [x y])
                   (apply g/digraph))
        target (apply max input)
        succ-fn (g/successors graph)]
    (reduce (fn [a v] (assoc a v (map #(vector % target) (succ-fn v))))
            {}
            input)))

(defn count-node-paths
  [acc entry]
  (let [foobar (map #(let [[a b] %]
                       (if (= a b) 1 (get acc a))) entry)]
    (apply + (flatten foobar))))

(defn count-all-paths
  [mp]
  (let [mp (into (sorted-map-by >) mp)]
    (-> (reduce-kv #(if (empty? %3)
                      %1
                      (assoc %1 %2 (count-node-paths %1 %3)))
                   {}
                   mp)
        (get 0))))

(defn advent-10-1
  [input]
  (->> (input->seq input)
       (map #(Long/parseLong %))
       sort
       vec
       count-all-adapters
       vals
       (reduce *)))

(defn advent-10-2
  [input]
  (as-> (input->seq input) d
        (map #(Long/parseLong %) d)
        (conj d 0 (+ 3 (apply max d)))
        (sort d)
        (vec d)
        (build-graph d)
        (count-all-paths d)))

; ************
; Day 11
; ************
(defn seat-key
  [r c]
  (keyword (str r "-" c)))

(defn get-next-neighbors
  [_ row col]
  (->> (for [x [(dec row) row (inc row)]
             y [(dec col) col (inc col)]]
         [x y])
       (remove #(= [row col] %))))

(def neighbor-walk-fns [(fn [r c] [r (dec c)])
                        (fn [r c] [r (inc c)])
                        (fn [r c] [(dec r) c])
                        (fn [r c] [(inc r) c])
                        (fn [r c] [(dec r) (dec c)])
                        (fn [r c] [(dec r) (inc c)])
                        (fn [r c] [(inc r) (dec c)])
                        (fn [r c] [(inc r) (inc c)])])

(defn get-all-neighbor-seats
  "Gets the collection of visible neighbor seats in the eight directions."
  [grid row col]
  (vec (for [neigh-fn neighbor-walk-fns]
         (loop [[row col] (neigh-fn row col)]
           (cond (< row 0) nil
                 (< col 0) nil
                 (>= row (count grid)) nil
                 (>= col (count (get grid 0))) nil
                 (= \# (get (get grid row) col)) [row col]
                 (= \L (get (get grid row) col)) [row col]
                 (= \. (get (get grid row) col)) (recur (neigh-fn row col)))))))

(defn count-occupied-neighbors
  [grid neighbors]
  (->>
    (for [[r c] neighbors]
      (get (get grid r) c))
    (filter #(= \# %))
    count))

(defn seatgrid-one-gen-one-row
  [grid row-idx all-neighbors occmax]
  (let [row (get grid row-idx)]
    (->>
      (for [c (range 0 (count row))
            :let [seat (get row c)
                  neigh-cnt (count-occupied-neighbors grid ((seat-key row-idx c) all-neighbors))]]
        (cond (= seat \.) \.
              (= seat \L) (if (= 0 neigh-cnt) \# \L)
              (= seat \#) (if (<= occmax neigh-cnt) \L \#)))
      (apply str)
      )))

(defn populate-neighbormap
  "Populates the map of all neighbors for each seat in the grid. The provided
  function takes three arguments, grid, row, and col."
  [grid f]
  (->>
    (for [r (range 0 (count grid))]
      (for [c (range 0 (count (get grid r)))]
        [{(seat-key r c) (f grid r c)}]))
    flatten
    (into {})))

(defn seatgrid-one-gen
  [grid neighbors occmax]
  (let [grid (vec grid)]
    (reduce-kv (fn [a k _] (conj a (seatgrid-one-gen-one-row grid k neighbors occmax)))
               [] grid)))

(defn seatgrid-evolve
  [occmax pop-fn grid]
  (let [neighbors (populate-neighbormap grid pop-fn)]
    (loop [prev-grid grid]
      (let [grid (seatgrid-one-gen prev-grid neighbors occmax)]
        (if (= grid prev-grid)
          grid
          (recur grid))))))

(defn advent-11
  [occmax pop-fn input]
  (->> (input->seq input)
       vec
       (seatgrid-evolve occmax pop-fn)
       (apply str)
       frequencies
       ))

; ************
; Day 12
; ************
(def simple-ferry-moves {:E (fn [[x y] d m] [[(+ x m) y] d])
                         :S (fn [[x y] d m] [[x (- y m)] d])
                         :W (fn [[x y] d m] [[(- x m) y] d])
                         :N (fn [[x y] d m] [[x (+ y m)] d])})

(defn turn-ferry
  [d turn deg]
  (let [x {:E 0 :S 90 :W 180 :N 270}
        y (map-invert x)
        f (if (= turn :R) + -)]
    (get y (-> (d x)
               (f deg)
               (mod 360)))))

(defn move-ferry
  [[[x y] d] instr]
  (let [inst-key (->> (first instr) str keyword)
        inst-fn (inst-key simple-ferry-moves)
        m (->> (rest instr) (apply str) Integer/parseInt)]
    (cond inst-fn (inst-fn [x y] d m)
          (= inst-key :F) (((keyword d) simple-ferry-moves) [x y] d m)
          :else [[x y] (turn-ferry d inst-key m)])))

(defn turn-waypoint
  [[wx wy] turn deg]
  (let [deg (if (= turn :L) (* -1 deg) deg)
        turns (/ (mod deg 360) 90)]
    (reduce (fn [[a b] _]
              [b (* -1 a)])
            [wx wy]
            (range turns))))

(def waypoint-ferry-moves {:E (fn [[x y] [wx wy] m] [[x y] [(+ wx m) wy]])
                           :S (fn [[x y] [wx wy] m] [[x y] [wx (- wy m)]])
                           :W (fn [[x y] [wx wy] m] [[x y] [(- wx m) wy]])
                           :N (fn [[x y] [wx wy] m] [[x y] [wx (+ wy m)]])
                           :R (fn [[x y] [wx wy] m] [[x y] (turn-waypoint [wx wy] :R m)])
                           :L (fn [[x y] [wx wy] m] [[x y] (turn-waypoint [wx wy] :L m)])
                           :F (fn [[x y] [wx wy] m] [[(+ x (* m wx)) (+ y (* m wy))] [wx wy]])})

(defn move-with-waypoint
  [[[x y] [wx wy]] instr]
  (let [inst-key (->> (first instr) str keyword)
        inst-fn (inst-key waypoint-ferry-moves)
        m (->> (rest instr) (apply str) Integer/parseInt)]
    (inst-fn [x y] [wx wy] m)))

(defn advent-12-1
  [input]
  (let [init [[0 0] :E]]
    (->> (input->seq input)
         (reduce #(move-ferry %1 %2) init))))

(defn advent-12-2
  [input]
  (let [init [[0 0] [10 1]]]
    (->> (input->seq input)
         (reduce #(move-with-waypoint %1 %2) init))))

; ************
; Day 13
; ************
(defn shuttle-wait
  [[time shuttles]]
  (let [time (Integer/parseInt time)
        shuttles (str/split shuttles #",")]
    (->> (filter #(re-matches #"\d*" %) shuttles)
         (map #(Integer/parseInt %))
         (map #(vector (- % (mod time %)) %))
         (sort-by first)
         first
         (apply *))))

(defn solve-congruence
  [a n]
  (loop [i 1]
    (if (= 0 (mod (- (* a i) 1) n))
      i
      (recur (inc i)))))

(defn get-little-m
  [coll idx [a b]]
  (let [coll (vec coll)
        factors (vec (map second coll))
        l (subvec factors 0 idx)
        r (subvec factors (inc idx))]
    (as-> (concat l r) d
          (apply * d)
          (assoc {:a a} :m d)
          (assoc d :y (solve-congruence (:m d) b))
          (vals d)
          (apply * d))))

(defn get-chinese-rem
  [coll]
  (->> (reduce-kv (fn [a k v] (into a [(get-little-m coll k v)]))
                  [] coll)
       (reduce +)))

(defn chinese-remainder
  [[_ shuttles]]
  (let [shuttles (->> (str/split shuttles #",")
                      (map-indexed (fn [k v] [k v]))
                      (filter (fn [[_ v]] (re-matches #"\d*" v)))
                      (map (fn [[k v]] [k (bigint v)]))
                      (map (fn [[k v]] [(- v k) v]))
                      (sort-by first))
        m (apply * (map second shuttles))
        chinese-rem (get-chinese-rem (vec shuttles))]
    (mod chinese-rem m)))

(defn advent-13-1
  [input]
  (->> (input->seq input)
       (shuttle-wait)))

(defn advent-13-2
  [input]
  (->> (input->seq input)
       (chinese-remainder)))

; ************
; Day 14
; ************
(defn conj-floating-bit
  [acc]
  (let [data (:data acc)
        x-cnt (/ (:x-cnt acc) 2)]
    (->>
      (for [x [\0 \1]]
        (map #(conj % x) data))
      (apply concat)
      vec
      (assoc {:x-cnt x-cnt} :data))))

(defn pad-left
  [coll pad n]
  (-> (repeat n pad)
      (cons coll)
      flatten
      vec))

(defn apply-floating-bit
  [mask input]
  (let [input (->> (Integer/toString input 2) vec)
        mask (->> (vec mask) (drop-while #(= \0 %)) vec)
        input (if (>= (count input) (count mask))
                input
                (pad-left input \0 (- (count mask) (count input))))
        mask (if (= (count mask) (count input))
               mask
               (pad-left mask \0 (- (count input) (count mask))))
        x-cnt (math/expt 2 (count (filter #(= \X %) mask)))]
    (->> (reduce-kv (fn [a k v] (cond (= \X (get mask k)) (conj-floating-bit a)
                                      (= \1 (get mask k)) (assoc a :data (map #(conj % 1) (:data a)))
                                      :else (assoc a :data (map #(conj % v) (:data a)))))
                    {:x-cnt x-cnt :data [[]]}
                    input)
         :data
         (map #(apply str %))
         (map #(Long/parseLong % 2)))))

(defn xstr->masks
  [mask]
  {:and-mask (-> (str/replace mask #"X" "1") (Long/parseLong 2))
   :or-mask  (-> (str/replace mask #"X" "0") (Long/parseLong 2))
   :x-mask   mask})

(defn process-docking
  [input]
  (let [m-mask #"mask = (.*)$"
        m-mem #"mem\[(\d*)\] = (\d*)$"]
    (apply + (-> (reduce (fn [a v] (let [[mask? mask] (re-matches m-mask v)
                                         [mem? loc val] (re-matches m-mem v)]
                                     (cond mask? (into a (xstr->masks mask))
                                           mem? (->> (Long/parseLong val)
                                                     (bit-and (:and-mask a))
                                                     (bit-or (:or-mask a))
                                                     (assoc a loc)))))
                         {} input)
                 (dissoc :and-mask :or-mask :x-mask)
                 vals))))

(defn process-docking-v2
  [input]
  (let [m-mask #"mask = (.*)$"
        m-mem #"mem\[(\d*)\] = (\d*)$"]
    (as-> (reduce (fn [a v] (let [[mask? mask] (re-matches m-mask v)
                                  [mem? loc val] (re-matches m-mem v)]
                              (cond mask? (into a (xstr->masks mask))
                                    mem? (as-> (Long/parseLong loc) d
                                               (apply-floating-bit (:x-mask a) d)
                                               (reduce #(assoc %1 %2 val) a d)))))
                  {} input) d
          (dissoc d :and-mask :or-mask :x-mask)
          (vals d)
          (map #(Integer/parseInt %) d)
          (apply + d)
          )))


(defn advent-14-1
  [input]
  (->> (input->seq input)
       process-docking
       ))

(defn advent-14-2
  [input]
  (->> (input->seq input)
       process-docking-v2))

; ************
; Day 15
; ************
(defn mem-append
  [cache l n]
  (let [prev (first l)
        next-val (if (nil? prev)
                   0
                   (- n prev 1))
        prev-entry (get cache next-val)
        next-entry (if (nil? prev-entry)
                     [nil n]
                     [(second prev-entry) n])
        cache (assoc cache next-val next-entry)]
    [(inc n) cache next-entry]))

(defn advent-15
  [n input]
  (let [cache (reduce-kv #(assoc %1 %3 [nil %2]) {} (vec input))
        n (- n (count input))
        end-n (+ (count input) n)]
    (->>
      (loop [n (count input)
             cache cache
             l (get cache (last input))]
        (if (>= n end-n)
          cache
          (let [[x y z] (mem-append cache l n)] (recur x y z)))
        )
      (reduce-kv (fn [a k [_ v]] (if (> v (:max-idx a))
                                   (assoc a :max-idx v :val k)
                                   a))
                 {:max-idx -1})
      :val)))

(deftest test-mem-game
  "N.B. This improved solution is still slow, so only this subset is run by default during a build."
  (are [n input exp] (= exp (advent-15 n input))
                     2020 '(0 3 6) 436
                     2020 '(1 3 2) 1
                     2020 '(2 1 3) 10
                     2020 '(1 2 3) 27
                     2020 '(2 3 1) 78
                     2020 '(3 2 1) 438
                     2020 '(3 1 2) 1836))

(comment
  (deftest test-mem-game-slow
    "N.B. This improved solution is still slow, so this subset is not run by default during a build."
    (are [n input exp] (= exp (advent-15 n input))
                       30000000 '(0 3 6) 175594
                       30000000 '(1 3 2) 2578
                       30000000 '(2 1 3) 3544142
                       30000000 '(1 2 3) 261214
                       30000000 '(2 3 1) 6895259
                       30000000 '(3 2 1) 18
                       30000000 '(3 1 2) 362))
  )


; ************
; Day 16
; ************
(defn ticket-rules
  [line]
  (let [line-parse
        (fn [l] (let [[_ rule x1 x2 x3 x4]
                      (re-matches #"([^:]*):\s*(\d*)-(\d*)[^\d]*(\d*)-(\d*)" l)]
                  {:field rule
                   :r1    [(Integer/parseInt x1) (Integer/parseInt x2)]
                   :r2    [(Integer/parseInt x3) (Integer/parseInt x4)]}))]
    (->> (str/split line #"\n")
         (map line-parse))))

(defn read-tix
  [line]
  (->> (str/split line #"\n")
       rest
       (map #(str/split % #","))
       (map (fn [v] (map #(Integer/parseInt %) v)))))

(defn read-tickets
  [input]
  (let [[r m n] (input->groups input)]
    {:r (ticket-rules r) :m (->> (read-tix m) first) :n (read-tix n)}))

(defn rules->testfn
  [rules]
  (for [rule rules
        :let [[ra1 ra2] (:r1 rule)
              [rb1 rb2] (:r2 rule)]]
    (fn [t] (or (<= ra1 t ra2) (<= rb1 t rb2)))))

(defn rules->fieldfn
  [rules]
  (for [rule rules
        :let [field (:field rule)
              [ra1 ra2] (:r1 rule)
              [rb1 rb2] (:r2 rule)]]
    (fn [t] (if (or (<= ra1 t ra2) (<= rb1 t rb2))
              field
              nil))))

(defn tix-scanning-err-rate
  [input]
  (let [rules (->> (rules->testfn (:r input)) (apply some-fn))
        near-tix (:n input)]
    (->> (map #(remove rules %) near-tix)
         flatten
         (apply +))))

(defn discard-invalid-tix
  [input]
  (let [rule-cnt (count (:r input))
        rules (->> (rules->testfn (:r input)) (apply some-fn))
        near-tix (:n input)]
    (->> (map #(filter rules %) near-tix)
         (filter #(= (count %) rule-cnt)))))

(defn clean-input
  [input acc]
  (->> (map (fn [[k v]] [k (apply disj v (map #(first (second %)) acc))]) input)
       (remove #(empty? (second %)))))

(defn remove-invalid-fields
  [input]
  (loop [input input
         acc []]
    (if (empty? input)
      acc
      (as-> (reduce (fn [a v] (if (= (count (second v)) 1)
                                (conj a v)
                                a))
                    acc input) d
            (recur (clean-input input d) d)))))

(defn determine-tix-fields
  [input prefix]
  (let [rules (rules->fieldfn (:r input))
        valid-tix (discard-invalid-tix input)]
    (->> (for [t valid-tix]
           (reduce-kv (fn [ta tk tv]
                        (reduce (fn [a v] (update a tk #(conj % (v tv))))
                                ta rules))
                      {} (vec t)))
         (reduce (fn [a v]
                   (reduce-kv (fn [ar kr vr] (update ar kr #(conj % (set vr))))
                              a v))
                 {})
         (reduce-kv (fn [a k v] (assoc a k (apply intersection v))) {})

         remove-invalid-fields
         (map (fn [[k v]] [(first v) k]))
         (filter #(str/starts-with? (first %) prefix))
         (map second))))

(defn sum-tix-field
  [input fields]
  (let [my-tix (vec (:m input))]
    (->> (for [idx fields]
           (get my-tix idx))
         (apply *))))

(defn advent-16-1
  [input]
  (->> (read-tickets input)
       tix-scanning-err-rate))

(defn advent-16-2
  [input prefix]
  (let [input (read-tickets input)
        tix-fields (determine-tix-fields input prefix)]
    (sum-tix-field input tix-fields)))

; ************
; Day 17
; ************
(defn input->conway
  [n input]
  "Converts 2d input slice to n-d Conway space starting at [100 100...100]."
  (let [origin 100
        extra-dim (for [_ (range 0 (- n 2))] origin)]
    (->> (reduce (fn [acc val]
                   (-> (reduce-kv (fn [a k v] (if (= v \#)
                                                (update a :data #(conj % (into [(+ 100 k) (:row a)] extra-dim)))
                                                a))
                                  acc (vec val))
                       (update :row inc)
                       ))
                 {:row origin :data []} input)
         :data)))

(defn cartesian
  [colls]
  (if (empty? colls)
    '(())
    (for [more (cartesian (rest colls))
          x (first colls)]
      (cons x more))))

(defn conway->nextgen-extents
  [dim data]
  "Gets the extents of the next generation as range functions."
  (->> (for [pos (range 0 dim)
             f [(comp dec min) (comp #(+ 2 %) max)]]
         (apply f (map #(nth % pos) data)))
       (partition 2)
       (map (fn [[a b]] (range a b)))
       cartesian))

(defn conway-count-neighbors
  [cube data]
  (let [preds (for [d (range 0 (count cube))]
                (fn [c] (-> (- (nth cube d) (nth c d)) math/abs (<= 1))))
        pred-self #(not= cube %)
        pred (every-pred (apply every-pred preds) pred-self)]
    (->> (filter pred data)
         count)))

(defn conway-cube-state
  [cube data]
  (let [neighbor-cnt (conway-count-neighbors cube data)
        active (some #(= cube %) data)]
    (cond
      (= 3 neighbor-cnt) cube
      (and active (= 2 neighbor-cnt)) cube
      :else nil)))

(defn conway-one-generation
  [n data]
  (let [pts (conway->nextgen-extents n data)]
    (->> (for [pt pts]
           (conway-cube-state pt data))
         (remove #(nil? %)))))

(defn conway-generations
  [n dim data]
  (loop [n n
         data data]
    (if (= 0 n)
      data
      (recur (dec n) (conway-one-generation dim data)))))

(defn advent-17
  [n dim input]
  (->> (input->seq input)
       (input->conway dim)
       (conway-generations n dim)
       count))

; ************
; Day 18
; ************
(def new-math (insta/parser (io/resource "day18-no-precedence.grammar")))

(def new-math-2 (insta/parser (io/resource "day18-plus-precedence.grammar")))

(def transform-options
  {:add    +
   :mul    *
   :number clojure.edn/read-string
   :expr   identity})

(deftest test-18-no-precedence
  (are [input exp] (= (->> (new-math input) (insta/transform transform-options)) exp)
                   "1 + (2 * 3) + (4 * (5 + 6))" 51
                   "2 * 3 + (4 * 5)" 26
                   "5 + (8 * 3 + 9 + 3 * 4 * 3)" 437
                   "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" 12240
                   "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" 13632))

(deftest test-18-plus-precedence
  (are [input exp] (= (->> (new-math-2 input) (insta/transform transform-options)) exp)
                   "1 + (2 * 3) + (4 * (5 + 6))" 51
                   "2 * 3 + (4 * 5)" 46
                   "5 + (8 * 3 + 9 + 3 * 4 * 3)" 1445
                   "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" 669060
                   "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" 23340))

(defn advent-18
  [input m]
  (->> (input->seq input)
       (map m)
       (map #(insta/transform transform-options %))
       (apply +)))

; ************
; Day 19
; ************
(defn advent-19
  [input]
  (let [[grammar input] (input->groups input)
        parser (insta/parser grammar :start :0)
        parsable? #(->> (insta/parses parser %) insta/failure? not)]
    (->> (str/split input #"\n")
         (filter parsable?)
         count)))

; ************
; Day 20
; ************
(defn debug [x] (println x) x)

(defn binstr->numpair
  [s]
  (as-> (str/replace s #"#" "1") d
        (str/replace d #"\." "0")
        (vector d (str/reverse d))
        (mapv #(Integer/parseInt % 2) d)))

(defn maptile
  [input]
  (let [[_ tileno] (->> (first input) (re-matches #"Tile (\d*):"))
        buffer (rest input)
        top (first buffer)
        right (apply str (map last buffer))
        bottom (last buffer)
        left (apply str (map first buffer))]
    (into {:tile tileno}
          (->> (list top right bottom left)
               (map (fn [v] {:border (binstr->numpair v)}))
               (zipmap '(:top :right :bottom :left))))))

(defn edge-count
  [t tiles]
  (let [othr-edges (->> (filter #(not= (:tile t) (:tile %)) tiles)
                        (map #(dissoc % :tile))
                        (map vals)
                        flatten
                        (map vals)
                        flatten
                        frequencies)
        up-cnt #(assoc % :match-cnt (get othr-edges (first (:border %))))]
    (-> (update t :top up-cnt)
        (update :bottom up-cnt)
        (update :left up-cnt)
        (update :right up-cnt))))

(defn count-nil-matched-edges
  [tile]
  (->> (dissoc tile :tile)
       vals
       (map :match-cnt)
       (filter nil?)
       count))

(defn get-corner-tiles
  [tiles]
  (->> (map #(edge-count % tiles) tiles)
       (map (fn [v] (into v {:nil-edge (count-nil-matched-edges v)})))
       (filter #(>= (:nil-edge %) 2))))

(defn tile->unmatched-edge-cnt
  [tiles]
  (->> (get-corner-tiles tiles)
       (map :tile)
       (map #(Integer/parseInt %))
       (apply *)))

(defn stitch-tiles
  [tiles]
  (let [corners (get-corner-tiles tiles)
        tl (first corners)
        tiles (filter #(not= (:tile tl) (:tile %)) tiles)
        size (math/sqrt (count tiles))]

    (loop [corners corners
           grid [[]]]
      )
    corners
    )
  )


(defn advent-20-1
  [input]
  (->> (input->groups input)
       (map str/split-lines)
       (mapv maptile)
       tile->unmatched-edge-cnt))

; ************
; Day 21
; ************
(defn input->allergens
  [input]
  (let [rfn (fn [[f a]] (vector (str/split f #" ")
                                (if a
                                  (as-> (re-matches #"contains ([^\)]*)\)" a) d
                                        (second d)
                                        (str/split d #",")
                                        (mapv #(str/trim %) d))
                                  [])))]
    (->> (input->seq input)
         (map #(str/split % #"\("))
         (map rfn))))

(defn get-all-allergens
  [input]
  (->> (map second input)
       flatten
       set))

(defn get-potential-foods
  [input allergens]
  (reduce (fn [a v]
            (assoc a v
                     (->> (filter #(some (fn [f] (= v f)) (second %)) input)
                          (map first)
                          (map set)
                          (apply intersection))))
          {}
          allergens))

(defn remove-matches
  [input matched]
  (let [ing (apply union (map second matched))
        allergens (set (map first matched))]
    (map (fn [r] (vector (into [] (difference (set (first r)) ing))
                         (into [] (difference (set (second r)) allergens))))
         input)))

(defn match-allergens
  [input]
  (let [extract-single-match (fn [p] (filter #(= 1 (count (second %))) p))]
    (loop [input input
           acc {}]
      (let [allergens (get-all-allergens input)
            potentials (get-potential-foods input allergens)]
        (if (empty? potentials)
          acc
          (let [sing (extract-single-match potentials)
                input (remove-matches input sing)]
            (recur input (into acc sing))))))))

(defn advent-21-1
  [input]
  (let [input (input->allergens input)
        allergens (match-allergens input)
        all-ing (apply union (map second allergens))]
    (->> (map first input)
         (map set)
         (map #(difference % all-ing))
         (map count)
         (apply +))))

(defn advent-21-2
  [input]
  (->> (match-allergens (input->allergens input))
       (into (sorted-map))
       (map second)
       (map seq)
       flatten
       (str/join ",")))

; ************
; Day 22
; ************
(defn deck->winscore
  [d]
  (let [d (vec d)
        decksize (count d)]
    (->> (map-indexed #(* %2 (- decksize %1)) d)
         (apply +))))

(defn combat-concat
  [d1 d2 &ignore]
  (let [c1 (first d1)
        c2 (first d2)
        d1 (rest d1)
        d2 (rest d2)]
    (if (> c1 c2)
      [(concat d1 (list c1 c2)) d2]
      [d1 (concat d2 (list c2 c1))])))

(declare play-combat)
(declare combat-recurse)

(defn play-subgame
  [d1 d2 c1 c2 prev-games]
  (let [winner (first (play-combat (take c1 d1) (take c2 d2) combat-recurse))]
    (if (= :d1 winner)
      [(concat d1 (list c1 c2)) d2 prev-games]
      [d1 (concat d2 (list c2 c1)) prev-games])))

(defn combat-recurse
  [d1 d2 prev-games]
  (let [this-game (str (str/join "-" d1) "/" (str/join "-" d2))]
    (if (contains? prev-games this-game)
      [d1 '() prev-games]
      (let [prev-games (conj prev-games this-game)
            c1 (first d1)
            c2 (first d2)
            d1 (rest d1)
            d2 (rest d2)]
        (cond (and (<= c1 (count d1)) (<= c2 (count d2))) (play-subgame d1 d2 c1 c2 prev-games)
              (> c1 c2) [(concat d1 (list c1 c2)) d2 prev-games]
              :else [d1 (concat d2 (list c2 c1)) prev-games])))))

(defn play-combat
  ([d1 d2 f] (play-combat d1 d2 f #{}))
  ([d1 d2 f prev-games]
   (as-> (loop [[d1 d2 prev-games] [d1 d2 prev-games]]
           (cond (empty? d1) [:d2 d2]
                 (empty? d2) [:d1 d1]
                 :else (recur (f d1 d2 prev-games)))) d
         [(first d) (deck->winscore (second d))])))

(defn advent-22
  [input f]
  (as-> (input->groups input) d
        (map str/split-lines d)
        (map rest d)
        (map (fn [v] (map #(Integer/parseInt %) v)) d)
        (play-combat (first d) (second d) f)))

(deftest test-play-combat
  (are [result d1 d2 f] (= result (play-combat d1 d2 f))
                        [:d2 306] '(9 2 6 3 1) '(5 8 4 7 10) combat-concat
                        [:d1 105] '(43 19) '(2 29 14) combat-recurse
                        [:d2 291] '(9 2 6 3 1) '(5 8 4 7 10) combat-recurse))

(comment
  (advent-1 2 "day1.txt")
  (advent-1 3 "day1.txt")
  (advent-2 password-rule-match-1 "day2.txt")
  (advent-2 password-rule-match-2 "day2.txt")
  (advent-3 [[3 1]] "day3.txt")
  (advent-3 [[1 1] [3 1] [5 1] [7 1] [1 2]] "day3.txt")
  (advent-4 "day4.txt" :coyotesqrl-p1/passport)
  (advent-4 "day4.txt" :coyotesqrl-p2/passport)
  (advent-5-1 "day5.txt")
  (advent-5-2 "day5.txt")
  (advent-6 customs-count-any "day6.txt")
  (advent-6 customs-count-all "day6.txt")
  (advent-7-1 "shiny gold" "day7.txt")
  (advent-7-2 "shiny gold" "day7.txt")
  (advent-8-1 "day8.txt")
  (advent-8-2 "day8.txt")
  (advent-9-1 25 "day9.txt")
  (advent-9-2 25 "day9.txt")
  (advent-10-1 "day10.txt")
  (advent-10-2 "day10.txt")
  (advent-11 4 get-next-neighbors "day11.txt")
  (advent-11 5 get-all-neighbor-seats "day11.txt")
  (advent-12-1 "day12.txt")
  (advent-12-2 "day12.txt")
  (advent-13-1 "day13.txt")
  (advent-13-2 "day13.txt")
  (advent-14-1 "day14.txt")
  (advent-14-2 "day14.txt")
  (advent-15 2020 '(7 14 0 17 11 1 2))
  (advent-15 30000000 '(7 14 0 17 11 1 2))
  (advent-16-1 "day16.txt")
  (advent-16-2 "day16.txt" "departure")
  (advent-17 6 3 "day17.txt")
  (advent-17 6 4 "day17.txt")
  (advent-18 "day18.txt" new-math)
  (advent-18 "day18.txt" new-math-2)
  (advent-19 "day19.txt")
  (advent-19 "day19-2.txt")

  (advent-21-1 "day21.txt")
  (advent-21-2 "day21.txt")
  (advent-22 "day22.txt" combat-concat)
  (advent-22 "day22.txt" combat-recurse)
  )
