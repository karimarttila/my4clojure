(ns myscratch
  (:require [clojure.test :refer [deftest use-fixtures is testing]]
            [hashp.core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A bigger scratch area for experimentation.


(comment
  
  (use 'clojure.pprint)
  (pprint (for [x (range 10)] (range x)))
  
  )

(+ 1 2)

String

(defn date? [d] (instance? java.util.Date d))
(date? (java.util.Date.))
(date? "asdf")


(require '[hashp.core])



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




(def p23
  (fn [lst]
    (reduce (fn [acc item]
              (cons item acc))
            '() lst)))

(def p23-old (fn [sq]
               (loop [myseq sq newSq '()]
                 (if (empty? myseq)
                   newSq (recur (rest myseq) (conj newSq (first myseq)))))))


; #156 ****************************************
(def p156 (fn [d v]
            (reduce (fn [m i] (assoc m i d)) {} v)))

(deftest p156-test
  (testing "p156"
    (is (= (p156 0 [:a :b :c]) {:a 0 :b 0 :c 0}))
    (is (= (p156 "x" [1 2 3]) {1 "x" 2 "x" 3 "x"}))
    (is (= (p156 [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]}))))

; #22 ****************************************
(def p22
  (fn [lst]
    (reduce (fn [acc _] (inc acc))
            0
            lst)))

(def p22-old (fn [l]
               (loop [mylist l mynum 0]
                 (if (empty? mylist)
                   mynum
                   (recur (rest mylist) (inc mynum))))))

(deftest p22-test
  (testing "p22"
    (is (= (p22 '(1 2 3 3 1)) 5))
    (is (= (p22 '(1 2 3 3 1)) 5))
    (is (= (p22 [[1 2] [3 4] [5 6]]) 3))
    (is (= (p22 '(13)) 1))
    (is (= (p22 '(:a :b :c)) 3))))

; #23 ****************************************

(def p23
  (fn [lst]
    (reduce (fn [acc item]
              (cons item acc))
            '() lst)))

(def p23-old (fn [sq]
               (loop [myseq sq newSq '()]
                 (if (empty? myseq)
                   newSq
                   (recur (rest myseq) (conj newSq (first myseq)))))))

(deftest p23-test
  (testing
   (is (= (p23 [1 2 3 4 5]) [5 4 3 2 1]))
    (is (= (p23 [1 2 3 4 5]) [5 4 3 2 1]))
    (is (= (p23 (sorted-set 5 7 2 7)) '(7 5 2)))
    (is (= (p23 [[1 2] [3 4] [5 6]]) [[5 6] [3 4] [1 2]]))))

; #27 ****************************************

;;Write a function which returns true if the given sequence is a palindrome.
;; Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)

(def p27
  (fn [coll]
    (let [rev (reverse coll)
          same-lst (map #(vector %1 %2) coll rev)
          result (filter #(= (first %) (second %)) same-lst)]
      (= (count coll) (count result)))))

(def p27-v1
  (fn [coll]
    (let [rev (reverse coll)
          same-lst (map #(vector %1 %2) coll rev)
          result (filter #(= (first %) (second %)) same-lst)]
      (= (count coll) (count result)))))

(def p27-old
  (fn [cand]
    (loop [s1 cand s2 (reverse cand)]
      (if (empty? s1)
        true
        (if-not (= (first s1) (first s2))
          false
          (recur (rest s1) (rest s2)))))))


(deftest p27-test
  (testing
   (is (false? (p27 '(1 2 3 4 5))))
    (is (true? (p27 "racecar")))
    (is (true? (p27 [:foo :bar :foo])))
    (is (true? (p27 '(1 1 3 3 1 1))))
    (is (false? (p27 '(:a :b :c))))))

; #26 ****************************************

;; Write a function which returns the first X fibonacci numbers.

(def v [1 1 2 3 5])
(def n (count v))
(+ (get v (- n 1)) (get v (- n 2)))


(def myfib (fn [v]
             (let [n (count v)]
               (conj v (+ (get v (- n 1)) (get v (- n 2)))))))
(myfib v)


(def p26-new
  (fn [n]
    (let [fib (fn [v]
                (let [n (count v)]
                  (conj v (+ (get v (- n 1)) (get v (- n 2))))))]
      (take n (last (take n (iterate fib [1 1])))))))

(def p26 (fn [n]
           (take n ((fn fib [a b]
                      (lazy-seq (cons a (fib b (+ a b)))))
                    1N 1N))))

(deftest p26-test
  (testing
   (is (= (p26 3) '(1 1 2)))
    (is (= (p26 6) '(1 1 2 3 5 8)))
    (is (= (p26 8) '(1 1 2 3 5 8 13 21)))))


;; P26
;(type (take 5 ((constantly 1))))
(def jee (constantly 1))

(def jee (iterate inc 0))
(take 5 jee)
"asdf"
(take 10  (iterate inc 5))
(take 10 (map first (iterate (fn [[x1 x2]] [x2 (+ x1 x2)]) [1 1])))
((fn [n] (take n (map first (iterate (fn [[x1 x2]] [x2 (+ x1 x2)]) [1 1])))) 5)

(if nil "nil on true" "nil on false")
(seq? '()) ; => true
(seq? []) ; => false
(seq? {}) ; => false
(seq? {:a 1}) ; => false
(seq? nil) ; => false
(seq? 1) ; => false
(coll? '()) ; => true
(coll? []) ; => true
(coll? {}) ; => true
(coll? nil) ; => false
(coll? 1) ; => false
(rest '()) ; => '()
(next '()) ; => nil
(next '(1)) ; => nil
(next '(1 2)) ; => '(2)
(next '(1 2 3)) ; => '(2 3)
;(next 1) ; => error
;(empty? 1) ; => error
;(not-empty 1) ; => error


*ns*
(def mystr "ASDXXXX")
(def jee (fn [x] (str "jee1a: " x)))
(def jee (fn [x] (str "XXXX: " x)))
(def jee (fn [x] (str "jee2a: " x)))
(jee "kakka")

;mystr
;(find-var 'solutions1/jee)
;(sort (ns-publics 'solutions1))
;(sort (ns-publics 'user))
*ns*

(def P100 (fn [& ns] (let [rats #p (filter ratio? ns)
                           ints1 #p (filter (complement ratio?) ns)
                           ints2 #p (mapv numerator rats)
                           dens #p (mapv denominator rats)]
                       (letfn [(min-idx [xs] (->> xs (map-indexed vector) (apply min-key second) first))
                               (lcm [nums cands] (if (apply = cands)
                                                   cands
                                                   (let [i (min-idx cands)]
                                                     (lcm nums (assoc cands i (+ (nth cands i) (nth nums i)))))))]
                         (let [mul #p (if (empty? dens) 1 (first (lcm dens dens)))
                               nums #p (mapv #(* mul %) (concat ints1 ints2))]
                           (-> (lcm nums nums) first (/ mul)))))))
(def P100 (fn [& ns] (let [rats #p (filter ratio? ns)
                           ints1 #p (filter (complement ratio?) ns)
                           ints2 #p (mapv numerator rats)
                           dens #p (mapv denominator rats)]
                       (letfn [(min-idx [xs] (->> xs (map-indexed vector) (apply min-key second) first))
                               (lcm [nums cands] (if (apply = cands)
                                                   cands
                                                   (let [i (min-idx cands)]
                                                     (lcm nums (assoc cands i (+ (nth cands i) (nth nums i)))))))]
                         (let [mul #p (if (empty? dens) 1 (first (lcm dens dens)))
                               nums #p (mapv #(* mul %) (concat ints1 ints2))]
                           (-> (lcm nums nums) first (/ mul)))))))

(def vec10 ((take 10) conj))
(reduce vec10 [] (range 20))
(transduce vec10 conj (range 20))
(def jee (take 10))
(jee [1 2 3 4 5 6])

(def xf (comp (filter odd?) (take 10)))
xf
(transduce xf conj (range))
(transduce xf + (range))

(reduce conj #{} [1 2 2 3 1])
(into #{} [1 2 2 3 1])

(defn inc-transducer
  "Given a reducing function rf, return a new reducing function that increments
  every input it receives, then calls rf with the result and the incremented
  input."
  ;; rf stands for "reducing function"
  [rf]
  ;; this here's a new reducing function
  (fn [result input]
    ;; here we call the original reducing function
    (rf result (inc input))))

(def inc-then-conj (inc-transducer conj))
;;=> #'user/inc-then-conj
(inc-then-conj [1 2] 3)

(reduce inc-then-conj [] [1 2 3 4 5])
(reduce (inc-transducer +) 0 [1 2 3 4 5])


; P135
(def P135 (fn [a & xs] (:acc (reduce (fn [acc x]
                                       (if (#{+ - / *} x)
                                         (assoc acc :oper x)
                                         (let [operator (:oper acc)
                                               operand (:acc acc)
                                               new-acc (operator operand x)]
                                           (assoc acc :acc new-acc))))
                                     {:acc a :oper nil} xs))))
(def P135 (fn [a & xs] (:acc (reduce (fn [acc x]
                                       (if (#{+ - / *} x)
                                         (assoc acc :oper x)
                                         (assoc acc :acc ((:oper acc) (:acc acc) x))))
                                     {:acc a :oper nil} xs))))
; Muiden
; Tosi nerokas. Eli lasketaan (f x y) ja sitten tämä arvo laitetaan funktiolle,
; joka on if-lauseen jälkeen joko (fn [z] (apply c z r)) tai +.
; Eli kun (f x y) on laskettu, niin sen arvo laitetaan sisään z:n arvoksi.
(def P135 (fn c [x f y & r]
            ((if r
               (fn [z] (apply c z r))
               +)
             (f x y))))
(def P135 (fn c [x f y & r]
            (let [_ #p x
                  _ #p f
                  _ #p y
                  _ #p r])
            ((if r
               (fn [z] (let [_ #p z] (apply c z r)))
               +)
             (f x y))))
; Tämä myös tosi hyvä.
(def P135 (fn calc
            ([x] x)
            ([x op y & r] (apply calc (conj r (op x y))))))
; Vesa
(def P135c (fn [& exprs]
             (reduce (fn [acc x]
                       (if (fn? x)
                         (partial x acc)
                         (acc x)))
                     exprs)))
; Valtteri
(def P135d
  (fn calc ([x op y & r]
            (if op
              (recur (op x y) (first r) (second r) (drop 2 r))
              x))))
(P135 10 / 2 - 1 * 2)
(apply P135 '(10 / 2 - 1 * 2))
(apply P135 '(1 + 1))
(apply P135 '(10 / 2 - 1 * 2))
(P135 2 + 5)
(= 7  (P135 2 + 5))
(= 42 (P135 38 + 48 - 2 / 2))
(= 8  (P135 10 / 2 - 1 * 2))
(= 72 (P135 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))
; Scratch
(assoc {:acc 0 :oper nil} :oper +)
(defn infix-list
  ([] (cons 1 (infix-list + 1)))
  ([o n] (lazy-seq (cons o (cons n (infix-list o n))))))
(def jee (take 5 (infix-list)))
(apply P135 jee)
; Oma reducella tehty ratkaisu toimii.
; Rekursiolla tehty ratkaisu räjäyttää pinon.
(def P135b (fn calc
             ([x] x)
             ([x op y & r] (apply calc (conj r (op x y))))))
;(time (apply P135b (take 1000000 (infix-list))))
;(time (apply P135c (take 1000000 (infix-list))))
;(time (apply P135d (take 1000000 (infix-list))))
;(time (apply P135 (take 1000000 (infix-list))))
(time (+ 12 2))
(apply P135 '(10 / 2 - 1 * 2))
;; (apply P135 (take 10 (infix-list))) ; Wrong number of args (2) passed to: myscratch/calc

(def P135 (fn [a & xs] (:acc (reduce (fn [acc x]
                                       (let [_ #p acc
                                             _ #p x
                                             _ #p (type x)
                                             _ #p (#{+ - / *} x)]
                                         (if (#{+ - / *} x)
                                           (assoc acc :oper x)
                                           (let [operator (:oper acc)
                                                 operand (:acc acc)
                                                 new-acc (operator operand x)]
                                             (assoc acc :acc new-acc)))))
                                     {:acc a :oper nil} xs))))
(P135 10 / 2 - 1 * 2)
;; (apply P135 '(10 / 2 - 1 * 2)) ; Nullpointer
(def P135c (fn [& exprs]
             (reduce (fn [acc x]
                       (let [_ #p acc
                             _ #p x])
                       (if (fn? x)
                         (partial x acc)
                         (acc x)))
                     exprs)))
(defn infix-list
  ([] (cons 1 (infix-list + 1)))
  ([o n] (lazy-seq (cons o (cons n (infix-list o n))))))
(P135c 10 / 2 - 1 * 2)
(apply P135c (take 100 (infix-list)))
(def P135-kari (fn [a & xs] (:acc (reduce (fn [acc x]
                                            (if (#{+ - / *} x)
                                              (assoc acc :oper x)
                                              (assoc acc :acc ((:oper acc) (:acc acc) x))))
                                          {:acc a :oper nil} xs))))
(def P135-ve (fn [& exprs]
               (reduce (fn [acc x]
                         (if (fn? x)
                           (partial x acc)
                           (acc x)))
                       exprs)))
(def P135-va
  (fn calc ([x op y & r]
            (if op
              (recur (op x y) (first r) (second r) (drop 2 r))
              x))))
(defn infix-list
  ([] (cons 1 (infix-list + 1)))
  ([o n] (lazy-seq (cons o (cons n (infix-list o n))))))
(time (apply P135-kari (take 1000000 (infix-list))))
(time (apply P135-ve (take 1000000 (infix-list))))
; (time (apply P135-va (take 1000000 (infix-list)))) ;; Nullpointer

(def mytype (fn [& r]
              (reduce (fn [acc x]
                        (conj acc (type x)))
                      [] r)))

; Huomaa ero!
(mytype 10 / 2 - 1)
(apply mytype '(10 / 2 - 1))
(apply mytype [10 / 2 - 1])
(apply mytype [10 / 2 - 1])

#:foo{:foo 1 :bar 2 :_/quux 3}
;==>
{:foo/foo 1, :foo/bar 2, :quux 3}
