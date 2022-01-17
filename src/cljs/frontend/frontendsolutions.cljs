(ns frontend.frontendsolutions)

; just backend ja siinä: (reset)
; just frontend
; Open browser: http://localhost:7581/index.html
; Tässä tiedostossa: (shadow.cljs.devtools.api/repl :app)
; Nyt frontend-repl pitäisi toimia


; P29
; Javascript
; Not quite right.
(def PF29 (fn [s] (apply str (filter (fn [c] (= c (.toUpperCase c))) s))))
(def PF29 (fn [s] (apply str (filter (set (map char (range 65 91))) s))))
(= (PF29 "HeLlO, WoRlD!") "HLOWRD")
(empty? (PF29 "nothing"))
(= (PF29 "$#A(*&987Zf") "AZ")
