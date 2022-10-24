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

