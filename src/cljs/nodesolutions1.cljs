(ns nodesolutions1)


; First:
; just shadow-node
; ... wait till you see "Build completed", then:
; just run-node
; Then start node REPL in Cursive (connect to .shadowin this window
; (shadow.cljs.devtools.api/repl :app)
; HUOM! Älä ota pois tätä main! koska shadow-cljs vaatii sen.
(defn main! [])

"Hello world!"
"Hello world!"

;; P29
;; Javascript
;; Not quite working, since `, !` characters not removed.
;; But demonstrates that node repl works (toUpperCase) is a Javascript method.
;(def P29 (fn [s] (apply str (filter (fn [c] (= c (.toUpperCase c))) s))))
;; The same code works both in JVM Clojure and Node Clojurescript.
;(def P29 (fn [s] (apply str (filter (set (map char (range 65 91))) s))))
;(P29 "HeLlO, WoRlD!")
;;
;(= (P29 "HeLlO, WoRlD!") "HLOWRD")
;(empty? (P29 "nothing"))
;(= (P29 "$#A(*&987Zf") "AZ")
;
;
;; P99
;(def P99 (fn [x1 x2] (mapv (fn [x] (-> x int )) (seq (str (* x1 x2))))))
;; Muiden
;
;(= (P99 1 1) [1])
;(= (P99 99 9) [8 9 1])
;(= (P99 999 99) [9 8 9 0 1])
;; Scratch
;(char \8)
;(int \0)
;(int \9)
;(mapv (fn [x] (-> x int )) (seq (str 891)))
;(list 7 5/7 2 3/5)

(def P126 "asdf")
(let [x P126] (and (= (class x) x) x))
