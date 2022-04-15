(ns aoc2020.day1
  (:require [common.common :as common]))

(def real-file "resources/aoc2020/day1_input")
(def test-file "resources/aoc2020/day1_test")

(defn read-file
  [file-name]
  (->> file-name
       common/read-file
       (map #(Integer/parseInt %))))

; Part1

(defn combination-2-sum
  [numbers]
  (let [number-count (count numbers)]
    (for [index1 (range number-count)
          index2 (range number-count)
          :let [target (nth numbers index1)
                search (nth numbers index2)]
          :when (and
              (< index1 index2)
              (= (+ target search) 2020))]
      (* target search))))

(defn part1
  [file-name]
  (->> file-name
       read-file
       combination-2-sum
       first))

; Part2

(defn combination-3-sum
  [numbers]
  (let [number-count (count numbers)]
    (for [index1 (range number-count)
          index2 (range number-count)
          index3 (range number-count)
          :let [target (nth numbers index1)
                search1 (nth numbers index2)
                search2 (nth numbers index3)]
          :when (and
                  (< index1 index2)
                  (< index2 index3)
                  (= (+ target search1 search2) 2020))]
      (apply * [target search1 search2]))))

(defn part2
  [file-name]
  (->> file-name
       read-file
       combination-3-sum
       first))

(comment
  (part1 test-file) ; 514579
  (part1 real-file) ; 719796
  (part2 test-file) ; 241861950
  (part2 real-file)) ; 144554112