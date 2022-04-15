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

(defn read-data
  [file-name]
  (->> (read-file file-name)
       parsing-data))

; Logic

(def init-operation
  {:index         0
   :acc-val       0
   :visited       []
   :second-visit? false})

(defn get-action-number
  [data index]
  (let [[action number] (nth data index)]
    {:action action
     :number number}))

(defn update-visited
  [index operation]
  (update operation :visited #(conj % index)))

(defn update-nop
  [operation]
  (update operation :index inc))

(defn update-acc
  [number operation]
  (update
    (update operation :acc-val #(+ % number))
    :index inc))

(defn update-jmp
  [number operation]
  (update operation :index #(+ % number)))

(defn update-second-visit
  [{:as operation :keys [visited]}]
  (let [second-visit (->> visited
                          frequencies
                          vals
                          (some #(= % 2)))]
    (assoc operation :second-visit? second-visit)))

(defn execute-operation
  [data {:as operation :keys [index]}]
  (let [{:keys [action number]} (get-action-number data index)]
    (-> (case action
          :nop (->> operation
                    (update-visited index)
                    update-nop)
          :acc (->> operation
                    (update-visited index)
                    (update-acc number))
          :jmp (->> operation
                    (update-visited index)
                    (update-jmp number)))
        update-second-visit)))

(defn work
  [data operation]
  (->> operation
       (execute-operation data)))

(defn stop-operation?
  [{:keys [second-visit?]}]
  (not second-visit?))

(defn modify-acc-val
  [data {:keys [acc-val visited]}]
  (let [{:keys [action number]} (get-action-number data (last visited))]
    (if (= action :acc)
      (- acc-val number)
      acc-val)))

(defn part1
  [file-name]
  (let [data (->> file-name
                  read-file
                  parsing-data)]
    (->> init-operation
         (iterate #(work data %))
         (drop-while #(stop-operation? %))
         first
         (modify-acc-val data))))

(comment
  (part1 test-file)
  (part1 real-file))

(comment
  (->> test-file
       read-file
       parsing-data))

(comment
  (->> (- 1 0)))

