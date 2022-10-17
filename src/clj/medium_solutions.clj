(ns medium-solutions
  (:require [hashp.core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NOTE: In this solution1 namespace the elementary and easy solutions.
;;
;; NOTE!
;; If some problem page does not work, try to hard refresh (ctrl + refresh-button), then supply
;; the answer again!
;;
;; NOTE: There is some Finnish occasionally - this repo is just for my own learning purposes.
;; "Other developers' solutions": i.e. once I finished my own solution, I copy-pasted best
;; others' solutions and examined those solutions (e.g. using hashp) to learn from them.
;; There is a scratch area after each solution paragraph. If I realized that I need more space
;; for scratch I experimented in the myscratch namespace.



; P43
(def P43 (fn [xs n]
           (let [tuples (map (fn [z] [z n]) (range n))]
             (map (fn [[y x]]
                    (->> xs (drop y) (take-nth x))) tuples))))
; Other developers' solutions:
(def P43
  (fn [s n]
    (for [i (range n)]
      (take-nth n (drop i s)))))

(= (P43 [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
(= (P43 (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
(= (P43 (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))

(comment
  ((fn [xs n]
     (let [tuples (map (fn [z] [z n]) (range n))]
       (map (fn [[y x]]
              (let [_ #p y
                    _ #p x]
                (->> xs (drop y) (take-nth x)))) tuples)))
   [1 2 3 4 5 6] 2)

  ((fn [x1 y1 xs1]
     (->> xs1 (drop x1) (take-nth y1)))
   0 2 [1 2 3 4 5 6])

  (map (fn [z] [z 2]) (range 2))

  ((fn [z] [z 0]) 2))



; P44
; NOTE: In Clojurescript you need to use Math/abs - see the nodesolutions1.cljs.
(def P44 (fn [n xs]
           (let
            [c (count xs)
             n2 (abs n)
             x (if (<= n2 c) n2 (mod n2 c))
             p? (pos? n)
             xs1 (if p?
                   (take x xs)
                   (take-last (abs x) xs))
             xs2 (if p?
                   (drop x xs)
                   (drop-last (abs x) xs))]
             (if p?
               (concat xs2 xs1)
               (concat xs1 xs2)))))
; Other developers' solutions:
(def P44 (fn [n c] 
           (let [idx (mod n (count c))] (concat (drop idx c) (take idx c)))))

(def P44 (fn [n c] 
           (let [idx (mod n (count c))
                 _ #p idx] (concat (drop idx c) (take idx c)))))
; Damn, I didn't realize modulo rolls like that. 

(= (P44 2 [1 2 3 4 5]) '(3 4 5 1 2)) 
(= (P44 -2 [1 2 3 4 5]) '(4 5 1 2 3))
(= (P44 6 [1 2 3 4 5]) '(2 3 4 5 1)) 
(= (P44 1 '(:a :b :c)) '(:b :c :a)) 
(= (P44 -4 '(:a :b :c)) '(:c :a :b)) 

(comment

  ((fn [n xs]
     (let
      [c (count xs)
       n2 (abs n)
       x (if (<= n2 c) n2 (mod n2 c))
       p? (pos? n)
       xs1 (if p?
             (take x xs)
             (take-last (abs x) xs))
       xs2 (if p?
             (drop x xs)
             (drop-last (abs x) xs))
       _ #p n
       _ #p x
       _ #p xs1
       _ #p xs2]
       (if p?
         (concat xs2 xs1)
         (concat xs1 xs2)))) -4 '(:a :b :c))

  (count [1 2 3 4 5])

  (let [xs1 (take 2 [1 2 3 4 5])
        xs2 (drop 2 [1 2 3 4 5])]
    (concat xs2 xs1))

  (mod 6 5)
  (mod 5 2)
  (mod 9 4)
  )



; P46
(def P46)

; P50
(def P50)

; P54
(def P54)

; P55
(def P55)

; P56
(def P56)

; P58
(def P58)

; P59
(def P59)

; P60
(def P60)

; P65
(def P65)


; P67
(def P67)


; P69
(def P69)

; P70
(def P70)

; P74
(def P74)

; P75
(def P75)

; P76
(def P76)

; P77
(def P77)

; P78
(def P78)

; P80
(def P80)

; P85
(def P85)

; P86
(def P86)


; P93
(def P93)

; P98
(def P98)

; P102
(def P102)

; P103
(def P103)

; P104
(def P104)

; P105
(def P105)


; P108
(def P108)


; P110
(def P110)

; P112
(def P112)

; P114
(def P114)

; P115
(def P115)

; P116
(def P116)

; P121
(def P121)

; P131
(def P131)

; P132
(def P132)

; P137
(def P137)

; P141
(def P141)

; P144
(def P144)

; P148
(def P148)

; P150
(def P150)

; P158
(def P158)

; P168
(def P168)

; P171
(def P171)

; P177
(def P177)

; P195
(def P195)

