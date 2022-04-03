(ns aoc2018.day2
  (:require [common.common :as common]))

(def input (common/read-file "src/aoc2018/day2_input.txt"))

; Part One

(defn has-char-frequencies [text]
  (loop [text text
         hash {}]
    (if text
      (let [char (first text)
            count (get hash char 0)]
        (recur (next text)
               (assoc hash char (+ count 1))))
      (let [values (vals hash)]
        (loop [values values
               hasTwo false
               hasThree false]
          (if values
            (let [firstValue (first values)]
              (recur (next values)
                     (if (== firstValue 2) true hasTwo)
                     (if (== firstValue 3) true hasThree)))
            [hasTwo hasThree]))))))

(defn part1 [input]
  (loop [two-count 0
         three-count 0
         inputs input]
    (if inputs
      (do
        (let [has-char (has-char-frequencies (first inputs))]
          (recur (+ two-count (if (first has-char) 1 0))
                 (+ three-count (if (last has-char) 1 0))
                 (next inputs))))
      (* two-count three-count))))

; Part One - Remake

(defn has-number? [n list]
  (seq (filter #(= n %) list)))

(defn find-number [n list]
  (->> list
       (map frequencies)
       (map vals)
       (filter #(has-number? n %))
       count))

(defn part1-remake [input]
  (* (find-number 2 input)
     (find-number 3 input)))

(comment
  (part1 input)
  (part1-remake input)
  (find-number 2 [[2 3 4 2] [2 3 4 5]])
  (has-number? 2 [1 2 3 4 2]))

; Part Two

(defn get-same-chars
  "두가지 string에서 같은 부분의 chars를 return 한다."
  [str1 str2]
  (when (= (count str1) (count str2))
    (->> (range (count str1))
         (reduce (fn [same index]
                   (if (= (nth str1 index) (nth str2 index))
                     (str same (nth str1 index))
                     same
                     ))
                 ""))))

(defn part2
  "여러 개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오."
  [input]
  (->> (for [target-index (range (count input))
             search-index (range (count input))
             :let [target-str (nth input target-index)
                   search-str (nth input search-index)]
             :when (< target-index search-index)]
         (get-same-chars target-str search-str))
       (filter #(= (inc (count %)) (count (first input))))
       first))

(comment
  (part2 ["abcd" "abcf" "bbbc" "bbbe"])
  (part2 input)
  (get-same-chars "abxc" "abdc")
  (reduce = ["test" "test"])
  (not= (inc 1) 1))