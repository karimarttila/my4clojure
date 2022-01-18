(ns nodesolutions1)

; First:
; just shadow-node
; ... wait till you see "Build completed", then:
; just run-node
; Then start node REPL in Cursive (connect to .shadowin this window
; (shadow.cljs.devtools.api/repl :app)

"Hello world!"

; P29
; Javascript
; Not quite working, since `, !` characters not removed.
; But demonstrates that node repl works (toUpperCase) is a Javascript method.
(def P29 (fn [s] (apply str (filter (fn [c] (= c (.toUpperCase c))) s))))
; The same code works both in JVM Clojure and Node Clojurescript.
(def P29 (fn [s] (apply str (filter (set (map char (range 65 91))) s))))
(P29 "HeLlO, WoRlD!")
;
(= (P29 "HeLlO, WoRlD!") "HLOWRD")
(empty? (P29 "nothing"))
(= (P29 "$#A(*&987Zf") "AZ")



