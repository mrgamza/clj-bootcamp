(ns aoc2018.day3
  (:require [common.common :as common]))

(defn find-match
  [string]
  (->> string
       (re-matcher #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
       re-find))

(defn line->seq
  "Input을 parsing하여 (id x y width height) 형태로 return 합니다."
  [input]
  (->> input
       find-match
       rest
       (map #(Integer/parseInt %))))

(defn seq->hashmap
  "array의 형태를 hashmap으로 변환합니다. {:id :x :y :width :height}"
  [input]
  (->> input
       ((fn [[id x y w h]]
          {:id id :x x :y y :w w :h h}))))

(def input-data
  (->> "resources/aoc2018/day3_input.txt"
       common/read-file
       (map line->seq)
       (map seq->hashmap)))

; Part 1

(defn hashmap->positions
  "hashmap의 data를 position의 형태로 만들어서 return"
  [{:keys [x y w h]}]
  (for [x-position (range x (+ x w))
        y-position (range y (+ y h))]
    [x-position y-position]))

(defn get-fabric-frequencies
  "fabric의 frequeicies를 구한다."
  [input-data]
  (->> input-data
       (mapcat hashmap->positions)
       frequencies))

(defn get-overlap-count
  "frequency count가 1보다 크면 겹친것"
  [frequency-list]
  (->> frequency-list
       vals
       (filter #(> % 1))
       count))

(defn solution1
  "공간이 두번이상 겹쳐지는 영역의 갯수를 구하시오"
  [input-data]
  (->> input-data
       get-fabric-frequencies
       get-overlap-count))

(comment
  (input-data)
  (find-match "#1 @ 749,666: 27x15")
  (line->seq "#1 @ 749,666: 27x15")
  (seq->hashmap (line->seq "#1 @ 749,666: 27x15"))
  (hashmap->positions (seq->hashmap (line->seq "#1 @ 749,666: 27x15")))
  (->> input-data
       (mapcat hashmap->positions)
       frequencies))

(comment
  (solution1 input-data))

; Part 2

(defn not-overlap?
  "중복되지 않은 공간을 구한다."
  [once-list input-data]
  (->> input-data
       hashmap->positions
       (every? once-list)))

(defn find-not-overlap-id
  "Frequencies가 1인 공간을 구한다."
  [input-data]
  (let [frequencies-list (get-fabric-frequencies input-data)
        once-list #(= 1 (frequencies-list %))
        filter-not-overlap? (partial not-overlap? once-list)]
    (->> input-data
         (filter filter-not-overlap?)
         first
         :id)))

(defn solution2
  "입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
  위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력. 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)"
  [input-data]
  (->> input-data
       find-not-overlap-id))

(comment
  (solution2 input-data)
  (find-not-overlap-id input-data))