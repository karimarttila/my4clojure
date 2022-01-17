(ns myscratch
  (:require [clojure.test :refer [deftest use-fixtures is testing]]
            [hashp.core]))

(require '[hashp.core])

; #156 ****************************************
(def p156 (fn [d v]
            (reduce (fn [m i] (assoc m i d)) {} v)))

(deftest p156-test
  (testing "p156"
    (is (= (p156 0 [:a :b :c]) {:a 0 :b 0 :c 0}))
    (is (= (p156 "x" [1 2 3]) {1 "x" 2 "x" 3 "x"}))
    (is (= (p156 [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]}))
    ))

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
                   (recur (rest myseq) (conj newSq (first myseq))))
                 )
               ))

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

