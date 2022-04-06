(ns aoc2018.day4
  (:require [common.common :as common]))

; File load 하는것에 대한 부분들

(defn get-minute-action-id
  "행동에서 minute와 id를 가져온다."
  [[_ _ _ _ minute action id]]
  {:minute (Integer/parseInt minute)
   :action action
   :id (when-not (nil? id) (Integer/parseInt id))}) ; when-not !nil condition.

(defn line->data
  "Line 기준으로 string을 parse 한다."
  [string]
  (->> string
       (re-matcher #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (wakes up|falls asleep|Guard #(\d+) begins shift)")
       re-find
       rest
       get-minute-action-id))

(defn read-file
  "파일을 load 한다."
  [file-name]
  (->> (common/read-file (str "src/aoc2018/" file-name ".txt"))))

(defn read-file->data
  "파일을 read하고 원하는 형태로 parse 한다.
  조건상에 정렬된 데이터가 아니라서 시간순으로 정렬하여주고 line 기준으로 parsing 한다."
  [file-name]
  (->> file-name
       read-file
       sort
       (map line->data)))

(def data (read-file->data "day4_input"))
(def test-data (read-file->data "day4_test"))

; Part 1

(defn get-most-sleep-min
  "입력 데이터중에 가장 많은 빈도의 데이터를 return 하여준다.
  Input: {1 [1 2 3 4 5 6 6 3 ...] 2 [1 2 3 3 3...]}
  Output: {:id NUMBER :most-sleep-min HASHMAP}"
  [minutes]
  (let [id (key minutes)
        most-sleep-min (->> (val minutes)
                            frequencies
                            (sort-by val)
                            last)]
    {:id id
     :most-sleep-min most-sleep-min}))

(defn data->sleep-times-reduce
  "입력 데이터를 재조정한다. Action에 따라서 이전에 전달된 값을 이용하여 sleep-times hash를 만든다.
  Input: [minute action id]
  Output: {1 [1 2 3 4 5 6 6 3 ...] 2 [1 2 3 3 3...]}"
  [[sleep-times last-falls-asleep last-id]
   {:keys [minute action id]}]
  (cond
    id [sleep-times 0 id]
    (= action "falls asleep") [sleep-times minute last-id]
    (= action "wakes up") [(assoc sleep-times last-id (concat (sleep-times last-id) (range last-falls-asleep minute)))
                           last-falls-asleep
                           last-id]))

(defn data->sleep-times
  " data를 받아서 sleep times 들을 return 한다.
  Input : line data
  Output : {1 [1 2 3 4 5 6 6 3 ...] 2 [1 2 3 3 3...]}"
  [data]
  (->> data
       (reduce data->sleep-times-reduce [{} 0 0]) ; [{1 [1 2 3 4 5 6 6 3 ...] 2 [1 2 3 3 3...]}, last-falls-asleep last-id]
       first)) ; FIXME: first 말고 다르게 할수 있는 방법이 없을까?

(defn part1
  "가장 많이 잔 id에서 빈도수가 제일 많은 minute를 구하고 id * minute를 하여준다."
  [data]
  (let [{:keys [id most-sleep-min]} (->> data
                                         data->sleep-times
                                         (sort-by #(count (val %)))
                                         last
                                         get-most-sleep-min)]
    (* id (key most-sleep-min))))

; Part 2

(defn part2
  "같은 시간에 가장 많이 잠든 minute를 구하고 이것의 id * minute를 하여준다."
  [data]
  (let [{:keys [id most-sleep-min]} (->> data
                                         data->sleep-times
                                         (map #(get-most-sleep-min %))
                                         (sort-by #(val (:most-sleep-min %)))
                                         last)]
    (* id (key most-sleep-min))))

; Test Part1 : 240, Part2 : 4455
; Real Part1 : 19025, Part2 : 23776
(comment
  (part1 test-data)
  (part2 test-data)
  (part1 data)
  (part2 data)
  (when-not (nil? nil) "test")
  )
