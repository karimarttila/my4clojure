(ns solutions1)

; P1
(def P1 true)
(= P1 true)

; P2
(def P2 4)
(= (- 10 (* 2 3)) P2)

; P3
(def P3 "HELLO WORLD")
(= P3 (.toUpperCase "hello world"))

; P4
;(= (list ___) '(:a :b :c))
(= (list :a :b :c) '(:a :b :c))

; P5
(def P5 '(1 2 3 4))
(= P5 (conj '(2 3 4) 1))
(= P5 (conj '(3 4) 2 1))

; P6
(= [:a :b :c] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))

; P7
(def P7 [1 2 3 4])
(= P7 (conj [1 2 3] 4))
(= P7 (conj [1 2] 3 4))

; P8
(def P8 #{:a :b :c :d})
(= P8 (set '(:a :a :b :c :c :c :c :d :d)))
(= P8 (clojure.set/union #{:a :b :c} #{:b :c :d}))



