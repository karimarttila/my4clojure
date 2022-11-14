(ns misc1
  (:require [clojure.string :as str]))

"asdf"

*ns*


(def myatom (atom {:a 1 :b 2}))
(swap! myatom assoc :b 25)
@myatom

(def jee 1)
jee

String
(.toUpperCase "fred")
(System/getProperty "java.vm.version")

(def foo 1)
foo

(def bar 5)
(def bar2 25)
bar

misc1/jee



(def jee2 3)
misc1/jee2

(ns-publics 'misc1)

(all-ns)

(->> (all-ns)
     (map ns-name)
     (map name)
     (filter #(clojure.string/ends-with? % "misc1")))

(def status (slurp "misc/status_2022-10-26.csv"))
status
(def lines (drop 1 (str/split-lines status)))
lines
(take 10 lines)
(take 10 (-> lines reverse))
(def items (map (fn [line]
                  (str/split line #";"))
                lines))
items
(count items)
(def ctgr (fn [line] (get line 2)))
(ctgr (first items))
(ctgr (first (drop 10 items)))
(map ctgr items)
(def grouped (group-by ctgr items))
(map (fn [[k v]]
       (let [solved (filter (fn [item]
                              (let [solved? (get item 3)]
                                (str/includes? solved? "Passed"))) v)]
         {k {:count (count v) :solved (count solved) :percentage (int (* 100 (double (/ (count solved) (count v)))))}}))
     grouped)

(map inc
     (map
      inc
      (range 3)))

(map inc (map inc (range 3)))



