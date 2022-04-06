(ns aoc2018.day4
  (:require [common.common :as common]))

(defn get-minute-action-id
  "행동에서 minute와 id를 가져온다."
  [data]
  (->> data
       ((fn [[_ _ _ _ minute action id]]
          {:minute (Integer/parseInt minute)
           :action action
           :id (if (nil? id) nil (Integer/parseInt id))}))))

(defn parse
  "Line 기준으로 string을 parse 한다."
  [string]
  (->> string
       (re-matcher #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (wakes up|falls asleep|Guard #(\d+) begins shift)")
       re-find
       rest
       get-minute-action-id))

(defn read-file
  "파일을 load 한다. 조건상에 정렬된 데이터가 아니었으므로 sort로 정렬하여 준다."
  [file-name]
  (->> (common/read-file (str "src/aoc2018/" file-name ".txt"))
       sort))

(def data (->> "day4_input" read-file))
(def test-data (->> "day4_test" read-file))

; Part 1

(defn get-most-sleep-min
  "가장 많은 frequency를 구한다."
  [minutes]
  (let [id (key minutes)
        most-sleep-min (->> (val minutes)
                            frequencies
                            (sort-by val)
                            last)]
    [id most-sleep-min]))

(defn reduce-sleep-times
  [[hash-map falls-asleep last-id]
   {:keys [minute action id]}]
  (cond
    (not (nil? id)) [hash-map falls-asleep id]
    (= action "falls asleep") [hash-map minute last-id]
    (= action "wakes up") [(assoc hash-map last-id (concat (hash-map last-id) (range falls-asleep minute)))
                           falls-asleep
                           last-id]))

(defn part1
  [data]
  (let [value (->> data
                   (map parse)
                   (reduce reduce-sleep-times [{} 0 0])
                   first
                   (sort-by #(count (val %)))
                   last
                   get-most-sleep-min)]
    (apply * [(first value) (key (last value))])))

; Part 2

(defn part2
  [data]
  (let [most-sleep-min (->> data
                             (map parse)
                             (reduce reduce-sleep-times [{} 0 0])
                             first
                             (map #(get-most-sleep-min %))
                             (sort-by #(val (last %)))
                             last)]
    (apply * [(first most-sleep-min) (key (last most-sleep-min))])))

(comment
  (part1 data)
  (part2 data))

(comment
  (test-data)
  (->> test-data
       (sort-by #(last %)))
  (apply * 2 4))