(ns aoc2020.day1
  (:require [common.common :as common]))

(def real-file "resources/aoc2020/day1_input")
(def test-file "resources/aoc2020/day1_test")

(defn read-file
  [file-name]
  (->> file-name
       common/read-file
       (map #(Integer/parseInt %))))

(defn find-two-sum
  [{:keys [target mores]}]
  (let [second (->> mores
                    (filter #(= 2020 (+ target %)))
                    first)]
    (when (not (nil? second)) [target second])))

(defn divide-target-mores
  [data]
  {:target (first data)
   :mores (next data)})

(defn find-combination
  [{:keys [target-mores]}]
  {:target-mores (divide-target-mores (target-mores :mores))
   :combination (find-two-sum target-mores)})

(defn has-combination?
  [data]
  (let [combination (data :combination)]
    (< (count combination) 2)))

(defn part1
  [data]
  (let [init (divide-target-mores data)]
    (->> {:target-mores init
          :combination '[]}
         (iterate find-combination)
         (drop-while has-combination?)
         first
         :combination
         (apply *))))

; Part1

(comment
  (->> test-file
       read-file
       part1) ; 514579
  (->> real-file
       read-file
       part1)) ; 719796

(comment
  (->> test-file
       read-file
       divide-target-mores
       find-two-sum))
