(ns aoc2018.day6
  (:require [common.common :as common]))

(defn data->point
  "line 기준 data를 point로 변환"
  [[_ x y]]
  {:x (Integer/parseInt x)
   :y (Integer/parseInt y)})

(defn line->point
  "Line 기준으로 string을 parse 한다."
  [line]
  (->> line
       (re-matcher #"(\d+), (\d+)")
       re-find
       data->point)) ; Data 구조는 1, 1

(defn read-file
  [file-name]
  (->> file-name
       common/read-file
       (map line->point)
       (map-indexed (fn [idx point] (assoc point :id idx))))) ; 편의를 위해 index를 넣어준다.

(def data (->> "resources/aoc2018/day6_input.txt"
               read-file))
(def test-data (->> "resources/aoc2018/day6_test.txt"
                    read-file))

; Convert

(defn convert-id-distance
  [distance {:keys [id]}]
  {:id id
   :distance distance})

(defn convert-id-point
  [id point]
  {:id id
   :point point})

; Logic

(defn get-boundary
  "무한의 공간중에 목표가 되는 공간의 크기이다."
  [points]
  (let [xs (map :x points)
        ys (map :y points)]
    {:start-x (apply min xs)
     :end-x   (apply max xs)
     :start-y (apply min ys)
     :end-y   (apply max ys)}))

(defn get-points-in-bounded-area
  "boundary의 공간을 point로 변환하여 준다."
  [{:keys [start-x end-x start-y end-y]}]
  (for [x (range start-x (inc end-x))
        y (range start-y (inc end-y))]
    {:x x
     :y y}))

(defn include-boundary-outside?
  "Boundary안에 위치하는지 비교한다. 만약 바운더리의 경계선에 닿은 부분이 있는지 확인하는 용도이다."
  [{:keys [start-x end-x start-y end-y]} {:keys [x y]}]
   (or (= start-x x)
       (= end-x x)
       (= start-y y)
       (= end-y y)))

(defn manhattan-distance
  "두 지점의 Manhattan distance를 구한다."
  [point target]
  (+ (abs (- (:x point) (:x target)))
     (abs (- (:y point) (:y target)))))

(defn get-distances-to-targets
  "Targets기준으로 point의 거리를 넣어준다."
  [targets point]
  (->> targets
       (map #(convert-id-distance (manhattan-distance point %) %)))) ; 못줄이나?

(defn get-closest-target-ids
  [targets point]
  (->> (get-distances-to-targets targets point)
       (group-by :distance)
       (apply min-key key)
       val
       (map :id)))

(defn data->limit-areas
  [targets]
  (let [boundary (get-boundary targets)
        points-in-bounded-area (get-points-in-bounded-area boundary)
        point-in-boundary? (partial include-boundary-outside? boundary)]
    (->> points-in-bounded-area
         (map #(convert-id-point (get-closest-target-ids targets %) %)) ; 못주이나?
         (filter #(= 1 (count (:id %))))
         (group-by :id)
         (map val)
         (map #(map :point %))
         (remove #(some point-in-boundary? %)))))

(defn max-limit-area
  [areas]
  (->> areas
       (map #(count %))
       (apply max)))

; Part1

(defn part1
  [data]
  (->> data
       data->limit-areas
       max-limit-area))

(comment
  (part1 test-data)
  (part1 data))

; testing

(comment
  (part1 test-data)
  (->> test-data
       (map-indexed (fn [id point] (assoc point :id id))))
  (->> [{:id 0, :point {:x 1, :y 1}} {:id 0, :point {:x 1, :y 2}}]
       (map (fn [value] (map :point value)))))