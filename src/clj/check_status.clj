#!/usr/bin/env bb

(require '[clojure.string :as str])
(require '[clojure.java.io :as io])
(require '[clojure.pprint :as pp])


(defn file-empty? [file]
    (zero? (.length (io/file file))))

(defn process-file [file]
  (let [file-exists (if (not (.exists (io/file file)))
                       (println (str "File does not exist: " file))
                       true)
        not-empty (if (and file-exists (file-empty? file))
                    (println (str "File is empty: " file))
                    true)
        ]
    (when (and not-empty file-exists)
      (let [lines (drop 1 (str/split-lines (slurp file)))
            items (map (fn [line]
                         (str/split line #";"))
                       lines)
          ; Categories (easy, elementary... column)
            ctgr (fn [line] (get line 2))
            grouped (group-by ctgr items)]
        ;(println (str "Starting to process file: " file))
        (reduce (fn [acc [k v]]
                  (let [solved (filter (fn [item]
                                         (let [solved? (get item 3)]
                                           (str/includes? solved? "Passed"))) v)]
                    (assoc acc k {:count (count v)
                                  :solved (count solved)
                                  :percentage (int (* 100 (double (/ (count solved) (count v)))))})))
                {}
                grouped)))))

(comment
  ; When debugging in Clojure JVM REPL.
  (def file-exists "misc/status_2022-10-26.csv")
  (def file-not-exists "misc/status_2022-10-26.csvXXX")
  (pp/pprint (process-file file-exists))
  (pp/pprint (process-file file-not-exists))
  (pp/pprint (process-file "misc/status_2022-10-26.csv"))

  )



; Run with babashka in the main directory as:
; bb src/clj/check_status.clj misc/status_2022-10-26.csv
(let [[file] *command-line-args*]
  (pp/pprint (process-file file)))
  


  
  
    

