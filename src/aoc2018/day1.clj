(ns aoc2018.day1
  (:require [common.common :as common]))

(def input
  (->> (common/read-file "src/aoc2018/day1_input.txt")
       (map #(Integer/parseInt %))))

; Part One

(defn part1 [input]
  (->> input
       (reduce +)))

; Part Two - Loop

(defn part2-loop [input]
  (loop [history #{}
         before 0
         inputs input]
    (let [sum (+ before (first inputs))]
      (if (history sum)
        sum
        (recur (conj history sum)
               sum
               (or (next inputs) input))))))

; Part Two - reduce

(defn part2-reduce [input]
  (reduce (fn [history current-sum]
            (if (history current-sum)
              (reduced current-sum)
              (conj history current-sum)))
          #{}
          (reductions + (cycle input))))

(comment
  (part2-loop input)
  (part2-reduce input))
