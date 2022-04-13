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
       ; (char (+ index 65))를 하였으나 이렇게 만들었을 경우에는 Z가 넘어가고 본문처럼 알파벳을 이용할 필요가 없어서 그냥 id를 이용한다.
       (map-indexed (fn [index point] (assoc point :id index)))))

(def data (read-file "resources/aoc2018/day6_input.txt"))
(def test-data (read-file "resources/aoc2018/day6_test.txt"))

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
  "무한의 공간중에 목표가 되는 공간의 크기이다.
  이유는 가장 작고 큰 x, y의 좌표의 공간이 최적화된 공간이 되는것이다."
  [points]
  (let [xs (map :x points)
        ys (map :y points)]
    {:start-x (apply min xs) ; (apply min [...])
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

; 참고 : https://needjarvis.tistory.com/455
(defn manhattan-distance
  "두 지점의 Manhattan distance를 구한다."
  [point target]
  (+ (abs (- (:x point) (:x target)))
     (abs (- (:y point) (:y target)))))

(defn distance->targets
  "Targets기준으로 point의 거리를 넣어준다.
  targets: input {:x 1 :y 1}... point {:x 1 :y 1}
  "
  [targets point]
  (->> targets
       (map (fn [target]
              (convert-id-distance (manhattan-distance point target) target)))))

(defn min-distance-ids
  "bounded area를 순회하면서 target들과 비교하여 가장 가까운 위치의 target을 가져간다.
  값에는 0, 1과 같이 여러개가 들어갈수 있다."
  [targets point]
  (->> (distance->targets targets point)
       (group-by :distance)
       (apply min-key key)
       val
       (map :id)))

(defn limit-areas
  "target을 입력 받아서 확장을 진행하였을 경우에 더 넓어지지 못하는 최대 크기를 구하도록 한다.
  Input. [targets: File의 line data {:x 1 :y 1}]
  Output. (({:x 2, :y 3} {:x 2, :y 4}))
  "
  [targets]
  (let [boundary (get-boundary targets)
        points-in-bounded-area (get-points-in-bounded-area boundary)
        has-remove-area? (partial include-boundary-outside? boundary)]
    (->> points-in-bounded-area
         (map #(convert-id-point (min-distance-ids targets %) %)) ; 못줄이나?
         (filter #(= 1 (count (:id %)))) ; 1보다 큰것은 두가지 포지션이 함께 차지할수 있는 공간이므로 제외한다.
         (group-by :id) ; id별로 차지할수 있는 공간을 묶어준다.
         (map val) ; id는 이제 필요 없으므로 position 값들만 필요하다.
         (map #(map :point %)) ; out map [{:id :point}], in map {:id :point} hash에서 point 카운트만 구할것이다.
         (remove #(some has-remove-area? %))
         (common/debug "Output")))) ; %는 ({:X :y} ...) 이것을 이용해서 bounded area의 가장자리의 값을 가진지 판단한다. 가질 경우에는 삭제한다.

(defn max-limit-area
  [areas]
  (->> areas
       (map count)
       (apply max)))

(defn point->distance-sum
  "Target들과 point의 distance의 합을 구한다."
  [targets point]
  (->> (distance->targets targets point)
       (map :distance)
       (apply +)))

(defn point-distance-sum
  "Target point를 입력 받아서 boundary를 만들고 각 포인트에서 distance의 합을 구한다."
  [targets]
  (->> targets
       get-boundary
       get-points-in-bounded-area
       (map #(point->distance-sum targets %))))

; Solution

(defn part1
  [data]
  (-> data
      limit-areas
      max-limit-area))

(defn part2
  "boundary안에서 target과의 distance의 합이 주어진 값보다 작다면 만족하는 point이다. 이 포인트들의 count를 구하는 문제"
  [data max-distance-sum]
  (->> data
       point-distance-sum
       (filter #(< % max-distance-sum))
       count))

(comment
  (part1 test-data) ;17
  (part1 data) ;3293
  (part2 test-data 10000) ;72
  (part2 data 10000)) ;45176

; testing

(comment
  (part1 test-data)
  (->> test-data
       (map-indexed (fn [id point] (assoc point :id id))))
  (->> [{:id 0, :point {:x 1, :y 1}} {:id 0, :point {:x 1, :y 2}}]
       (map (fn [value] (map :point value))))
  (->> (manhattan-distance {:x 1 :y 1} {:x 2 :y 3}))
  (apply min-key [1 2 3 4])
  (->> '({:id 0, :distance 15} {:id 1, :distance 10} {:id 2, :distance 6} {:id 3, :distance 10} {:id 4, :distance 7} {:id 5, :distance 0})
       (group-by :id)
       (apply min-key key)
       val)
  (->> '({:id 0, :distance 15} {:id 1, :distance 10} {:id 2, :distance 6} {:id 3, :distance 10} {:id 4, :distance 7} {:id 5, :distance 0})
       (group-by :distance))
  (->> '([{:id 0 :point {:x 1 :y 1}} {:id 1 :point {:x 2 :y 2}}])
       (map (fn [value]
              (println value)
              (map :point value))))
  (->> [1 2 3 4 5]
       (map-indexed (fn [index value] {:id (char (+ index 65))
                                       :value value})))
  (->> [1 2 3 4 5]
       (map inc)
       ))