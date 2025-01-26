(ns hard-solutions
  ; Turn off, since I provide many different solutions using the same def.
  {:clj-kondo/config '{:linters {:redefined-var {:level :off}}}} 
  (:require [hashp.core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://4clojure.oxal.org/
;;
;; NOTE: In this namespace are the hard solutions.
;;
;; If some problem page does not work, try to hard refresh (ctrl + refresh-button), then supply
;; the answer again!
;;
;; NOTE: There is some Finnish occasionally - this repo is just for my own learning purposes.
;; "Other developers' solutions": i.e. once I finished my own solution, I copy-pasted best
;; others' solutions and examined those solutions (e.g. using hashp) to learn from them.
;; There is a scratch area after each solution paragraph. If I realized that I need more space
;; for scratch I experimented in the myscratch namespace.


*ns*
"asdf"
(require '[hashp.core])



; P53
(def P53 (fn [xs] (let [ret (reduce (fn [acc x]
                                      (let [prev (:previous-item acc)
                                            acc (if (nil? prev)
                                                  (-> acc
                                                      (assoc :candidate-list [x])
                                                      (assoc :previous-item x))
                                                  (let [acc (assoc acc :previous-item x)]
                                                    (if (> x prev)
                                                      (update acc :candidate-list conj x)
                                                      (assoc acc :candidate-list [x]))))]
                                        (if (> (count (:candidate-list acc)) (count (:longest-list acc)))
                                          (assoc acc :longest-list (:candidate-list acc))
                                          acc)))
                                    {:longest-list []
                                     :candidate-list []
                                     :previous-item nil}
                                    xs)
                        ret (:longest-list ret)]
                    (if (> (count ret) 1)
                      ret
                      []))))

(= (P53 [1 0 1 2 3 0 4 5]) [0 1 2 3])
(= (P53 [5 6 1 3 2 7]) [5 6])
(= (P53 [2 3 3 4 5]) [3 4 5])
(= (P53 [7 6 5 4]) [])

(comment



  ((fn [xs] (let [ret (reduce (fn [acc x]
                                (let [prev (:previous-item acc)
                                           ;_ #p x
                                           ;_ #p prev
                                           ;_ #p acc
                                      acc (if (nil? prev)
                                            (-> acc
                                                (assoc :candidate-list [x])
                                                (assoc :previous-item x))
                                            (let [acc (assoc acc :previous-item x)]
                                              (if (> x prev)
                                                (update acc :candidate-list conj x)
                                                (assoc acc :candidate-list [x]))))]
                                  (let [_ #p x
                                        _ #p acc
                                        new-acc (if (> (count (:candidate-list acc)) (count (:longest-list acc)))
                                                  (-> acc
                                                      (assoc :longest-list (:candidate-list acc)))
                                                  acc)
                                        _ #p new-acc]
                                    new-acc)))
                              {:longest-list []
                               :candidate-list []
                               :previous-item nil}
                              xs)
                  ret (:longest-list ret)]
              (if (> (count ret) 1)
                ret
                [])))
   [2 3 3 4 5]
   #_[1 0 1 2 3 0 4 5]
   #_[7 6 5 4])

  )            

; P73
(def P73)

; P79
(def P79)


; P82
(def P82)


; P84
(def P84)


; P87
(def P87)


; P89
(def P89)

; P91
(def P91)

; P92
(def P92)

; P94
(def P94)

; P101
(def P101)

; P106
(def P106)

; P111
(def P111)

; P117
(def P117)

; P119
(def P119)

; P124
(def P124)

; P125
(def P125)

; P127
(def P127)

; P140
(def P140)

