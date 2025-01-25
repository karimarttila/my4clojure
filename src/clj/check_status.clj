#!/usr/bin/env bb

(require '[clojure.string :as str])
(require '[clojure.java.io :as io])
(require '[clojure.pprint :as pp])


(def mymap_orig {:a 1 :b 2 :c {:d 3 :e {:f 4}}})
(def mymap {:a 1 :b 2 :c {:d 3 :e {:f 4}}})
mymap
mymap_orig

(def JEE "Java EE")


(def process-file
  (fn [file]
    (let [lines (drop 1 (str/split-lines (slurp file)))
          items (map (fn [line]
                       (str/split line #";"))
                     lines)
          ; Categories (easy, elementary... column)
          ctgr (fn [line] (get line 2))
          grouped (group-by ctgr items)]
      (println (str "Starting to process file: " file))
      (reduce (fn [acc [k v]]
                (let [solved (filter (fn [item]
                                       (let [solved? (get item 3)]
                                         (str/includes? solved? "Passed"))) v)]
                  (assoc acc k {:count (count v)
                                :solved (count solved)
                                :percentage (int (* 100 (double (/ (count solved) (count v)))))})))
              {}
              grouped))))

; When debugging in Clojure JVM REPL.
;(pp/pprint (process-file "misc/status_2022-10-26.csv"))


(comment
  JEE
  JEE
  mymap

; Run with babashka.
  #_(let [[file] *command-line-args*]
      (when (empty? file)
        (println "Usage: <file>")
        (System/exit 1))
      (when (not (.exists (io/file file)))
        (println (str "File does not exist: " file))
        (System/exit 1))
      (pp/pprint (process-file file))))

