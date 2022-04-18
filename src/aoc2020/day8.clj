(ns aoc2020.day8
  (:require [common.common :as common]
            [clojure.string :as s]))

(def real-file "resources/aoc2020/day8_input")
(def test-file "resources/aoc2020/day8_test")

(defn parsing-data
  [data]
  (->> data
       (map #(s/split % #" "))
       (map (fn [[k v]] [(keyword k) (Integer/parseInt v)]))
       (into [])))

(defn read-file
  [file-name]
  (->> file-name
       common/read-file))

; Logic

(def init-operation
  {:program-counter 0
   :acc             0
   :visited         []
   :second-visit?   false
   :replace-exit?   false})

(defn get-action-number
  [data program-counter]
  (let [[action number] (nth data program-counter)]
    {:action action
     :number number}))

(defn update-visited
  [program-counter operation]
  (update operation :visited #(conj % program-counter)))

(defn update-nop
  [operation]
  (update operation :program-counter inc))

(defn update-acc
  [number operation]
  (update
    (update operation :acc #(+ % number))
    :program-counter inc))

(defn update-jmp
  [number operation]
  (update operation :program-counter #(+ % number)))

(defn second-visit?
  [visited]
  (->> visited
       frequencies
       (some #(= (val %) 2))
       true?))

(defn update-second-visit
  [{:as operation :keys [visited]}]
  (assoc operation :second-visit? (second-visit? visited)))

(defn replace-exit?
  [data-size program-counter]
  (or (< (dec data-size) program-counter)
      (neg? program-counter)))

(defn update-replace-exit
  [data {:as operation :keys [program-counter]}]
  (let [data-size (count data)]
    (assoc operation :replace-exit? (replace-exit? data-size program-counter))))

(defn execute-operation
  [data {:as operation :keys [program-counter]}]
  (let [{:keys [action number]} (get-action-number data program-counter)]
    (->> (case action
           :nop (->> operation
                     (update-visited program-counter)
                     update-nop)
           :acc (->> operation
                     (update-visited program-counter)
                     (update-acc number))
           :jmp (->> operation
                     (update-visited program-counter)
                     (update-jmp number)))
         update-second-visit
         (update-replace-exit data))))

(defn element-visit-twice?
  [{:keys [second-visit?]}]
  (not second-visit?))

(defn modify-acc-val
  [data {:keys [acc visited]}]
  (let [{:keys [action number]} (get-action-number data (last visited))]
    (if (= action :acc)
      (- acc number)
      acc)))

(defn part1
  [file-name]
  (let [data (->> file-name
                  read-file
                  parsing-data)]
    (->> init-operation
         (iterate #(execute-operation data %))
         (drop-while #(element-visit-twice? %))
         first
         (modify-acc-val data))))

; Part 2

(defn generate-exit-condition-candidate-data
  "Input : [[:nop 0] [:acc 1] [:jmp 4] [:acc 3] [:jmp -3] [:acc -99] [:acc 1] [:jmp -4] [:acc 6]]
  Output : ([[:jmp 0] [:acc 1] [:jmp 4] [:acc 3] [:jmp -3] [:acc -99] [:acc 1] [:jmp -4] [:acc 6]] [[:nop 0] [:acc 1] [:nop 4] [:acc 3] [:jmp -3] [:acc -99] [:acc 1] [:jmp -4] [:acc 6]] ..."
  [data]
  (->> data
       (map-indexed (fn [index [action number]]
                      (case action
                        :nop (assoc data index [:jmp number])
                        :jmp (assoc data index [:nop number])
                        :acc nil)))
       (remove nil?)))

(defn exit-when-value-changes?
  [{:keys [second-visit? replace-exit?]}]
  (and
    (not replace-exit?)
    (not second-visit?)))

(defn part2-core
  [data]
  (->> init-operation
       (iterate #(execute-operation data %))
       (drop-while #(exit-when-value-changes? %))
       first))

(defn part2
  [file-name]
  (->> file-name
       read-file
       parsing-data
       generate-exit-condition-candidate-data
       (map part2-core)
       (filter :replace-exit?)
       first
       :acc))

(comment
  (part1 test-file) ; 5
  (part1 real-file)) ; 1818

(comment
  (part2 test-file) ; 8
  (part2 real-file)) ;631