(ns frontend.frontendsolutions)

; just backend ja siinä: (reset)
; just frontend
; Open browser: http://localhost:7581/index.html
; Tässä tiedostossa: (shadow.cljs.devtools.api/repl :app)
; Nyt frontend-repl pitäisi toimia


; P29
; Javascript
(def P29 (fn [s] (apply str (filter (fn [c] (= c (.toUpperCase c))) s))))
(= (P29 "HeLlO, WoRlD!") "HLOWRD")
(empty? (P29 "nothing"))
(= (P29 "$#A(*&987Zf") "AZ")
