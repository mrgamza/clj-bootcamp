(ns aoc2018.day1
  (:require
    [clojure.java.io :as io]))

(defn read-file [file-name]
  (->> (io/reader file-name)
       line-seq))

; Part One

(def input (map #(Integer/parseInt %) (read-file "day1_input.txt")))
(println (reduce + input))

; Part Two

(println (reduce (fn [history currentSum]
                   (if (history currentSum)
                     (reduced currentSum)
                     (conj history currentSum)))
                 #{}
                 (reductions + (cycle input))))
