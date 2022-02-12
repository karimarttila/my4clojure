(ns elementary-solutions
  (:require [hashp.core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://4clojure.oxal.org/
;;
;; NOTE: In this namespace are the elementary solutions.
;;
;; NOTE!
;; If some problem page does not work, try to hard refresh (ctrl + refresh-button), then supply
;; the answer again!
;;
;; NOTE: There is some Finnish occasionally - this repo is just for my own learning purposes.
;; E.g. "muiden" = others, i.e. once I finished my own solution, I copy-pasted best
;; others' solutions and examined those solutions (e.g. using hashp) to learn from them.
;; There is a scratch area after each solution paragraph. If I realized that I need more space
;; for scratch I experimented in the myscratch namespace.

*ns*
"asdf"
(require '[hashp.core])

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
;(= (list P4) '(:a :b :c))
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

; P9
(def P9 2)
(= #{1 2 3 4} (conj #{1 4 3} P9))

; P10
(def P10 20)
(= P10 ((hash-map :a 10, :b 20, :c 30) :b))
(= P10 (:b {:a 10, :b 20, :c 30}))

; P11
(def P11 [:b 2])
(= {:a 1, :b 2, :c 3} (conj {:a 1} P11 [:c 3]))

; P12
(def P12 3)
(= P12 (first '(3 2 1)))
(= P12 (second [2 3 4]))
(= P12 (last (list 1 2 3)))

; P13
(def P13 '(20 30 40))
(= P13 (rest [10 20 30 40]))

; P14
(def P14 8)
(= P14 ((fn add-five [x] (+ x 5)) 3))

; P15
(def P15 (fn [x] (* 2 x)))
(= (P15 2) 4)

; P16
(def P16 (fn [x] (str "Hello, " x "!")))
(= (P16 "Dave") "Hello, Dave!")

; P17
(def P17 '(6 7 8))
(= P17 (map #(+ % 5) '(1 2 3)))

; P18
(def P18 '(6 7))
(= P18 (filter #(> % 5) '(3 4 5 6 7)))


; P35
(def P35 7)
(= P35 (let [x 5] (+ 2 x)))

; P36
(def P36 '[z 1 y 3 x 7])
(= 10 (let [z 1 y 3 x 7] (+ x y)))
(= 4 (let [z 1 y 3 x 7] (+ y z)))
(= 1 (let [z 1 y 3 x 7] z))

; P37
(def P37 "ABC")
(= P37 (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))

; P52
(= [2 4] (let [[a b c d e f g] (range)] [c e]))
; scratch
(take 10 (range))


; P57
(def P57 '(5 4 3 2 1))
(= P57 ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))

; P64
(def P64 +)
(= 15 (reduce P64 [1 2 3 4 5]))

; P68
(def P68 '[7 6 5 4 3])
(= P68
   (loop [x 5
          result []]
     (if (> x 0)
       (recur (dec x) (conj result (+ 2 x)))
       result)))

; P71
(def P71 last)
(= (P71 (sort (rest (reverse [2 5 4 1 3 6]))))
   (-> [2 5 4 1 3 6] reverse rest sort P71)
   5)


; P72
(= (reduce + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
   (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (reduce +))
   11)

; P134
(def P134 (fn [k m] (and (contains? m k) (nil? (k m)))))
(true? (P134 :a {:a nil :b 2}))
(false? (P134 :b {:a nil :b 2}))
(false? (P134 :c {:a nil :b 2}))

; P145
(def P145 '(1 5 9 13 17 21 25 29 33 37))
(= P145 (for [x (range 40)
              :when (= 1 (rem x 4))]
          x))
(= P145 (for [x (iterate #(+ 4 %) 0)
              :let [z (inc x)]
              :while (< z 40)]
          z))
(= P145 (for [[x y] (partition 2 (range 20))]
          (+ x y)))
; Scratch
(take 10 (iterate #(+ 4 %) 0))
(take 10 (partition 2 (range 20)))

; P156
(def P156 (fn [v keys] (into {} (map (fn [k] {k v}) keys))))
; Muiden:
; HYVÄ!
(def P156 #(reduce into (for [k %2] {k %})))
(def P156 #(zipmap %2 (repeat %1)))
;
(= (P156 0 [:a :b :c]) {:a 0 :b 0 :c 0})
(= (P156 "x" [1 2 3]) {1 "x" 2 "x" 3 "x"})
(= (P156 [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})
; scratch
(= 2 (:foo {:bar 0, :baz 1} 2))

; P161
(def P161 #{1 2})
(clojure.set/superset? P161 #{2})
(clojure.set/subset? #{1} P161)
(clojure.set/superset? P161 #{1 2})
(clojure.set/subset? #{1 2} P161)

; P162
(def P162 1)
(= P162 (if-not false 1 0))
(= P162 (if-not nil 1 0))
(= P162 (if true 1 0))
(= P162 (if [] 1 0))
(= P162 (if [0] 1 0))
(= P162 (if 0 1 0))
(= P162 (if 1 1 0))
