(ns aoc2018.day1
  (:require
    [clojure.string :as string]))

; FIXME: slurp로 한번 만들어 보자. 완료

(defn read-file [file-name]
  (->> (slurp file-name)
       string/split-lines))

(def input
  (->> (read-file "day1_input.txt")
       (map #(Integer/parseInt %))))

; Part One

(defn part1 [input]
  (->> input
       (reduce +)))

; Part Two - Loop

(defn part2-loop [input]
  (loop [history #{}
         before 0
         inputs (seq input)]
    (let [sum (+ before (first inputs))]
      (if (not (history sum))
        (recur (conj history sum) sum (if (nil? (next inputs)) input (next inputs)))
        sum
        ))))

; Part Two - reduce

(defn part2-reduce [input]
  (reduce (fn [history currentSum]
            (if (history currentSum)
              (reduced currentSum)
              (conj history currentSum)))
          #{}
          (reductions + (cycle input))))
