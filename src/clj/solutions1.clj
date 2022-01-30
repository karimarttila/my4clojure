(ns solutions1
  (:require [hashp.core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HUOM! Tässä solutions1 namespacessa elementary ja easy tehtävät.
;;
;; NOTE!
;; If some problem page does not work, try to hard refresh (ctrl + refresh-button), then supply
;; the answer again!

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

; P19
(def P19 (fn [lst] (let [c (count lst)] (first (drop (- c 1) lst)))))
; Muut:
(def P19 #(first (reverse %)))
; HYVÄ!
(def P19 (comp first reverse))
;
(= (P19 [1 2 3 4 5]) 5)
(= (P19 '(5 4 3)) 3)
(= (P19 ["b" "c" "d"]) "d")

; P20
(def P20 (fn [lst] (first (drop 1 (reverse lst)))))
(def P20 (fn [lst] (->> lst reverse (drop 1) first)))
(def P20 #(->> % reverse (drop 1) first))
; Muiden
(def P20 #(second (reverse %)))
(def P20 (comp last butlast))
(= (P20 (list 1 2 3 4 5)) 4)
(= (P20 ["a" "b" "c"]) "b")
(= (P20 [[1 2] [3 4]]) [1 2])

; P21
(def P21 (fn [lst n] (first (drop n lst))))
(def P21 #(->> %1 (drop %2) first))
(= (P21 '(4 5 6 7) 2) 6)
(= (P21 [:a :b :c] 0) :a)
(= (P21 [1 2 3 4] 1) 2)
(= (P21 '([1 2] [3 4] [5 6]) 2) [5 6])

; P22
(def P22 #(count %)) ; Ei saa käyttää count.
(def P22 (fn [lst] (reduce (fn [acc x] (let [;_ #p acc
                                             ;_ #p x
                                             ]
                                         (+ 1 acc))) 0 (seq lst))))
(def P22 (fn [lst] (reduce (fn [acc x] (+ 1 acc)) 0 (seq lst))))
(def P22 (fn [lst] (reduce (fn [acc x] (+ 1 acc)) 0 lst)))
; Muiden
(def P22 #(reduce + (map (constantly 1) %)))
;
(= (P22 '(1 2 3 3 1)) 5)
(= (P22 "Hello World") 11)
(= (P22 [[1 2] [3 4] [5 6]]) 3)
(= (P22 '(13)) 1)
(= (P22 '(:a :b :c)) 3)
; scratch
(seq "abc")
(P22 "abc")

; P23
(def P23 (fn [lst] (reduce (fn [acc x] (conj acc x)) '() lst)))
(def P23 #(reduce (fn [acc x] (conj acc x)) '() %))
; Muiden
; HYVÄ!
(def P23 (fn [xs] (into () xs)))
(def P23 #(into () %))
(def P23 #(reduce conj () %))
;
(= (P23 [1 2 3 4 5]) [5 4 3 2 1])
(= (P23 (sorted-set 5 7 2 7)) '(7 5 2))
(= (P23 [[1 2] [3 4] [5 6]]) [[5 6] [3 4] [1 2]])

; P24
(def P24 #(apply + %))
; Muiden
(def P24 #(reduce + %))
;
(= (P24 [1 2 3]) 6)
(= (P24 (list 0 -2 5 5)) 8)
(= (P24 #{4 2 1}) 7)
(= (P24 '(0 0 -1)) -1)
(= (P24 '(1 10 3)) 14)

; P25
(def P25 #(filter odd? %))
(= (P25 #{1 2 3 4 5}) '(1 3 5))
(= (P25 [4 2 1 6]) '(1))
(= (P25 [2 2 4 6]) '())
(= (P25 [1 1 1 3]) '(1 1 1 3))

; P26
; Muistin tämän ratkaisun jostain Clojure-kirjasta.
(def P26 (fn [n] (take n (map first (iterate (fn [[x1 x2]] [x2 (+ x1 x2)]) [1 1])))))
;(P26 4)
(= (P26 3) '(1 1 2))
(= (P26 6) '(1 1 2 3 5 8))
(= (P26 8) '(1 1 2 3 5 8 13 21))
; Scratch
(take 10 (map first (iterate (fn [[x1 x2]] [x2 (+ x1 x2)]) [1 1])))
((fn [n] (take n (map first (iterate (fn [[x1 x2]] [x2 (+ x1 x2)]) [1 1])))) 5)

; P27
(def P27 #(= (seq %) (reverse %)))
(false? (P27 '(1 2 3 4 5)))
(true? (P27 "racecar"))
(true? (P27 [:foo :bar :foo]))
(true? (P27 '(1 1 3 3 1 1)))
(false? (P27 '(:a :b :c)))
; Scratch
(reverse "racecar")

; TODO XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
; P28


;(P28 '(1 (2 3)))
; Muiden:
(def P28 (fn [x] (flatten x))) ; Ei saanut käyttää flatten.
(def P28 (fn [x] (filter (complement sequential?)
                         (rest (tree-seq sequential? seq x)))))
;
(= (P28 '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
(= (P28 ["a" ["b"] "c"]) '("a" "b" "c"))
(= (P28 '((((:a))))) '(:a))
; Scratch


; P29
; Works only with Java
;(def P29 (fn [s] (apply str (filter #(Character/isUpperCase %) s))))
; Javascript
;(def P29 (fn [s] (apply str (filter (fn [c] (= c (.toUpperCase c))) s))))
; Varsinainen ratkaisu:
(def P29 (fn [s] (apply str (filter (set (map char (range 65 91))) s))))
(P29 "HeLlO, WoRlD!")
; Muiden:
(def P29 (fn [st] (apply str (filter #(<= (int \A) (int %) (int \Z)) st))))
(def P29 #(apply str (re-seq #"[A-Z]+" %)))
;
(= (P29 "HeLlO, WoRlD!") "HLOWRD")
(empty? (P29 "nothing"))
(= (P29 "$#A(*&987Zf") "AZ")
; Scratch
(char 65)
(char 90)
(set (map char (range 65 91)))



; P30
(def P30 (fn [xs] (let [lst (->> xs
                                 (partition 2 1)
                                 (remove (fn [[a b]] (= a b))))
                        ; We need to add the last item in the right position.
                        [a b] (last lst)
                        ; Convert to vector so that we add to the end.
                        lst (conj (vec lst) [b a])]
                    (map (fn [[a b]] a) lst))))
(def P30 (fn [xs] (reduce (fn [acc x] (let [z (last acc)] (if (= z x) acc (conj acc x)))) [] xs)))
(P30 [1 1 2 3 3 2 2 3])
;
; Muiden:
(def P30 (fn [x] (reduce #(if (= (last %) %2) % (conj % %2)) [] x)))
(def P30 #(map first (partition-by identity %)))
;
(= (apply str (P30 "Leeeeeerrroyyy")) "Leroy")
(= (P30 [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
(= (P30 [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))
; Scratch
(->> "Leeeeeerrroyyy"
     (partition 2 1)
     (remove (fn [[a b]] (= a b))))
(partition-by identity "Leeeeeerrroyyy")


; P31
(def P31 (fn [xs] (partition-by identity xs)))
(P31 [1 1 2 1 1 1 3 3])
;

(= (P31 [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
(= (P31 [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
(= (P31 [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))
; Scratch
(partition-by identity [1 1 2 1 1 1 3 3])

; P32
(def P32 (fn [xs] (mapcat (fn [x] [x x]) xs)))
; Muiden
(def P32 #(interleave % %))
(= (P32 [1 2 3]) '(1 1 2 2 3 3))
(= (P32 [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
(= (P32 [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
(= (P32 [44 33]) [44 44 33 33])
; Scratch
(->> [1 2 3]
     (mapcat (fn [x] [x x])))


; P33
(def P33 (fn [xs n] (apply interleave (for [n (range n)] xs))))
(P33 [1 2 3] 2)
; Muiden
(def P33 #(mapcat (partial repeat %2) %))
;
(= (P33 [1 2 3] 2) '(1 1 2 2 3 3))
(= (P33 [:a :b] 4) '(:a :a :a :a :b :b :b :b))
(= (P33 [4 5 6] 1) '(4 5 6))
(= (P33 [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
(= (P33 [44 33] 2) [44 44 33 33])
; Scratch
(interleave [1 2 3] [1 2 3])
(for [n (range 2)]
  [1 2])
(interleave [1 2] [1 2])
((fn [xs n] (apply interleave (for [n (range n)] xs))) [1 2] 2)


; P34
(def P34 (fn [b e] (take (- e b) (iterate inc b))))
(P34 1 4)
(= (P34 1 4) '(1 2 3))
(= (P34 -2 2) '(-2 -1 0 1))
(= (P34 5 8) '(5 6 7))


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

; P38
(def P38 (fn [& xs] (reduce (fn [acc x] (if (> x acc) x acc)) (first xs) xs)))
(def P38 (fn [& xs] (-> xs sort reverse first)))
(def P38 (comp first reverse sort list))
; Muiden
(def P38 #(reduce (fn [x y] (if (> x y) x y)) %&))
(def P38 (fn [& s] (last (sort s))))
; Tämä toimii, koska partial tekee sort-funktion, jossa ensimmäinen parametri on >
; clojure.core/sort [coll] [^java.util.Comparator comp coll], eli > on Comparator.
; List tarvitaan, koska se on ensimmäinen, joka ottaa variadic määrän argumentteja sisään
; => eli tekee niistä ensin listan. Yllä [& xs] tekee listan jo valmiiksi.
(def P38 (comp first (partial sort >) list))
;
(= (P38 1 8 3 4) 8)
(= (P38 30 20) 30)
(= (P38 45 67 11) 67)

; P39
(def P39 (fn [xs1 xs2] (interleave xs1 xs2)))
(def P39 (fn [xs1 xs2] (mapcat (fn [a b] [a b]) xs1 xs2)))
; Muiden
(def P39 (fn [c1 c2] (mapcat list c1 c2)))
(P39 [1 2 3] [:a :b :c])
(= (P39 [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
(= (P39 [1 2] [3 4 5 6]) '(1 3 2 4))
(= (P39 [1 2 3 4] [5]) [1 5])
(= (P39 [30 20] [25 15]) [30 25 20 15])
; Scratch


; P40
(def P40 (fn [sep xs] (interpose sep xs)))
(def P40 (fn [sep xs] (drop-last (interleave xs (iterate identity sep)))))
; Muiden
(def P40 #(drop-last (interleave %2 (repeat %1))))
;
(P40 0 [1 2 3])
(= (P40 0 [1 2 3]) [1 0 2 0 3])
(= (apply str (P40 ", " ["one" "two" "three"])) "one, two, three")
(= (P40 :z [:a :b :c :d]) [:a :z :b :z :c :z :d])
; Scratch
(take 10 (iterate identity 1))
(drop 5 (iterate identity 1))

; P41
; Ei ihan toimi.
(def P41 (fn [xs n] (apply concat (partition (- n 1) n xs))))
(def P41 (fn [xs n] (loop [acc [] lst xs n n]
                      (if (empty? lst)
                        (vec (apply concat acc))
                        (let [add (take (- n 1) lst)
                              xs2 (drop n lst)]
                          (recur (conj acc add) xs2 n))))))
; Muiden
(def P41 #(apply concat (partition-all (dec %2) %2 %)))
;
(P41 [1 2 3 4 5 6] 4)
(P41 [1 2 3 4 5 6 7 8] 3)
(= (P41 [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
(= (P41 [:a :b :c :d :e :f] 2) [:a :c :e])
(= (P41 [1 2 3 4 5 6] 4) [1 2 3 5 6])
; Scratch
"asdf"

; P42
(def P42 (fn [n] (->> n inc (range 1) (apply *))))
; Muiden
(def P42 #(reduce * (range 2 (inc %))))
;
(P42 5)
(= (P42 1) 1)
(= (P42 3) 6)
(= (P42 5) 120)
(= (P42 8) 40320)

; P43
(def P43)

; P44
(def P44)

; P45
(def P45 '(1 4 7 10 13))
(= P45 (take 5 (iterate #(+ 3 %) 1)))


; P46
(def P46)

; P47
(def P47 4)
(contains? #{4 5 6} P47)
(contains? [1 1 1 1 1] P47)
(contains? {4 :a 2 :b} P47)
(not (contains? [1 2 4] P47))

; P48
(def P48 6)
(= P48 (some #{2 7 6} [5 6 7 8]))
(= P48 (some #(when (even? %) %) [5 6 7 8]))

; P49
(def P49 (fn [n xs] [(take n xs) (drop n xs)]))
; Muiden
(def P49 (juxt take drop))
(P49 3 [1 2 3 4 5 6])

(= (P49 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
(= (P49 1 [:a :b :c :d]) [[:a] [:b :c :d]])
(= (P49 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])

; P50
(def P50)

; P51
(def P51 [1 2 3 4 5])
(= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] P51] [a b c d]))

; P52
(= [2 4] (let [[a b c d e f g] (range)] [c e]))
; scratch
(take 10 (range))

; P53
(def P53)

; P54
(def P54)

; P55
(def P55)

; P56
(def P56)

; P57
(def P57 '(5 4 3 2 1))
(= P57 ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))

; P58
(def P58)

; P59
(def P59)

; P60
(def P60)

; P61
(def P61 (fn [xs1 xs2] (zipmap xs1 xs2)))
(def P61 (fn [xs1 xs2] (reduce (fn [acc [k v]] (assoc acc k v)) {} (map (fn [a b] [a b]) xs1 xs2))))
(def P61 (fn [xs1 xs2] (reduce (fn [acc [k v]] (assoc acc k v)) {} (partition 2 (interleave xs1 xs2)))))
(P61 [:a :b :c] [1 2 3])
; Muiden
(def P61 #(apply hash-map (interleave %1 %2)))
(def P61 #(into {} (map vector %1 %2)))
;
(= (P61 [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
(= (P61 [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
(= (P61 [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})
; Scratch
(reduce (fn [acc [k v]] (assoc acc k v)) {} (map (fn [a b] [a b]) [:a :b :c] [1 2 3]))
(interleave [:a :b :c] [1 2 3])
(map vector [:a :b :c] [1 2 3])


; P62
(def P62 (fn [f x] (lazy-seq (cons x (P62 f (f x))))))
;
(take 5 (P62 #(* 2 %) 1))
(= (take 5 (P62 #(* 2 %) 1)) [1 2 4 8 16])
(= (take 100 (P62 inc 0)) (take 100 (range)))
(= (take 9 (P62 #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))

; P63
(def P63 (fn [f xs] (group-by f xs)))
; First make a sequence of [k v] pairs. Then reduce the pairs to a map.
(def P63 (fn [f xs] (reduce (fn [acc [k v]] (assoc acc k (conj (or (get-in acc [k]) []) v)))
                            {}
                            (map (fn [x] [(f x) x]) xs))))
; Parannus: get:lle voi antaa arvon, mikä palautetaan, jos key ei löydy.
(def P63 (fn [f xs] (reduce (fn [acc [k v]] (assoc acc k (conj (get acc k []) v)))
                            {}
                            (map (fn [x] [(f x) x]) xs))))
; Muiden
; Nerokas. Katso doc:sta merge-with.
(def P63 (fn [f coll]
           (reduce #(merge-with concat %1 {(f %2) [%2]}) {} coll)))
(def P63 (fn [f coll]
           (reduce (fn [acc x] (merge-with concat acc {(f x) [x]})) {} coll)))
(P63 #(> % 5) #{1 3 6 8})
;
(= (P63 #(> % 5) #{1 3 6 8}) {false [1 3], true [6 8]})
(= (P63 #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
   {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
(= (P63 count [[1] [1 2] [3] [1 2 3] [2 3]])
   {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})
; Scratch.
(assoc {:b 2} :b 5)
(merge-with concat {:a [1 2]} {:a [3 4]})
(merge-with (fn [xs1 xs2] (concat (drop 1 xs1) (drop 1 xs2))) {:a [1 2]} {:a [3 4]})


; P64
(def P64 +)
(= 15 (reduce P64 [1 2 3 4 5]))

; P65
(def P65)

; P66
; https://brilliant.org/wiki/greatest-common-divisor/
(def P66 (fn [n1 n2] (apply max (map first (filter (fn [[a b]] (= a b))
                                                   (for [x1 (range 1 (inc n1))
                                                         x2 (range 1 (inc n2))
                                                         :when (and (= (mod n1 x1) 0)
                                                                    (= (mod n2 x2) 0))]
                                                     [x1 x2]))))))
; Muiden
(def P66 (fn [a b] (if (= b 0) a (recur b (mod a b)))))
;
(P66 4488 12240)
(= (P66 2 4) 2)
(= (P66 10 5) 5)
(= (P66 5 7) 1)
(= (P66 1023 858) 33)
; Scratch
(range 1 3)


; P67
(def P67)

; P68
(def P68 '[7 6 5 4 3])
(= P68
   (loop [x 5
          result []]
     (if (> x 0)
       (recur (dec x) (conj result (+ 2 x)))
       result)))

; P69
(def P69)

; P70
(def P70)



; P71
(def P71 last)
(= (P71 (sort (rest (reverse [2 5 4 1 3 6]))))
   (-> [2 5 4 1 3 6] reverse rest sort P71)
   5)


; P72
(= (reduce + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
   (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (reduce +))
   11)


; P73
(def P73)

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

; P79
(def P79)

; P80
(def P80)

; P81
(def P81 (fn [set1 set2] (loop [acc #{} xs set1]
                           (if (empty? xs)
                             acc
                             (let [x (first xs)]
                               (recur (if (set2 x) (conj acc x) acc)
                                      (drop 1 xs)))))))
; Muiden
(def P81 (comp set filter))
(def P81 (comp set keep))
(def P81 #(set (for [x % y %2 :when (= x y)] x)))
;
(P81 #{0 1 2 3} #{2 3 4 5})
(= (P81 #{0 1 2 3} #{2 3 4 5}) #{2 3})
(= (P81 #{0 1 2} #{3 4 5}) #{})
(= (P81 #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})
; Scratch
(filter #{0 1 2 3} #{2 3 4 5}) ; => (3 2)

; P82
(def P82)

; P83
(def P83 (fn [& lst] (boolean (and (some true? lst) (not-every? true? lst)))))
; Muiden
; Eli pitää olla kumpaakin, sekä true että false.
; Ja koska pitää olla kumpaakin, niin ei voi olla niin, että on vain true tai on vain false.
(def P83 #(not (apply = %&)))
(def P83 not=)
;
(P83 false true false)
(= false (P83 false false))
(= true (P83 true false))
(= false (P83 true))
(= true (P83 false true false))
(= false (P83 true true true))
(= true (P83 true true true false))
; Scratch
(some true? (list true false true))
(not-every? true? (list true false true))
(not-every? true? (list true true true))
(some true? (list false false))
;
(apply = (list true false true))
(apply = (list true true true))
(apply = (list false false false))

; P84
(def P84)

; P85
(def P85)

; P86
(def P86)

; P87
(def P87)

; P88
(def P88 (fn [set1 set2] (let [both (clojure.set/intersection set1 set2)
                               all (clojure.set/union set1 set2)]
                           (set (remove both all)))))
(def P88 (fn [set1 set2] (set (remove (clojure.set/intersection set1 set2)
                                      (clojure.set/union set1 set2)))))
; Muiden
(def P88 (fn [set1 set2] (reduce #((if (% %2) disj conj) % %2) set1 set2)))
(def P88 (fn [set1 set2] (reduce (fn [acc x] ((if (acc x) disj conj) acc x)) set1 set2)))
(def P88 (fn [set1 set2] (reduce (fn [acc x] (let [;_ #p acc
                                                   ;_ #p x
                                                   f (if (acc x) disj conj)
                                                   ;_ #p f
                                                   ] (f acc x))) set1 set2)))
;
(P88 #{1 2 3 4 5 6} #{1 3 5 7})
(= (P88 #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
(= (P88 #{:a :b :c} #{}) #{:a :b :c})
(= (P88 #{} #{4 5 6}) #{4 5 6})
(= (P88 #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})
; Scratch
(clojure.set/intersection #{1 2 3 4 5 6} #{1 3 5 7})
(clojure.set/union #{1 2 3 4 5 6} #{1 3 5 7})



; P89
(def P89)

; P90
(def P90 (fn [set1 set2] (set (for [x1 set1 x2 set2] [x1 x2]))))
(P90 #{1 2 3} #{4 5})
(= (P90 #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
   #{["ace" "♠"] ["ace" "♥"] ["ace" "♦"] ["ace" "♣"]
     ["king" "♠"] ["king" "♥"] ["king" "♦"] ["king" "♣"]
     ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]})
(= (P90 #{1 2 3} #{4 5})
   #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})
(= 300 (count (P90 (into #{} (range 10))
                   (into #{} (range 30)))))

; P91
(def P91)

; P92
(def P92)

; P93
(def P93)

; P94
(def P94)

; P95
; Mieti ratkaisua: Pitää tarkistaa, että binary-treen jokainen lehti on nil tai binary-tree.
(def P95 (fn [x] (or (nil? x) ; Ollaan lehdessä, jonka on oltava nil, tai... pitää olla
                     (and (sequential? x) ; ... sequence, jonka
                          (= (count x) 3) ; ... pituus on kolme
                          (P95 (second x)) ; ... ja jonka eka
                          (P95 (nth x 2)))))) ; ... ja toka lehti on myös binary-tree.
(P95 '(:a (:b nil nil) nil))
;
; Muiden
(def P95 (fn t [x]
           (or (nil? x) (and (sequential? x) (= 3 (count x)) (t (second x)) (t (nth x 2))) false)))
(def P95 (fn bin-tree? [s]
           (or (nil? s)
               (and (coll? s)
                    (= 3 (count s))
                    (every? bin-tree? (rest s))))))
(P95 '(:a (:b nil nil) nil))
;
(= (P95 '(:a (:b nil nil) nil))
   true)
(= (P95 '(:a (:b nil nil)))
   false)
(= (P95 [1 nil [2 [3 nil nil] [4 nil nil]]])
   true)
(= (P95 [1 [2 nil nil] [3 nil nil] [4 nil nil]])
   false)
(= (P95 [1 [2 [3 [4 nil nil] nil] nil] nil])
   true)
(= (P95 [1 [2 [3 [4 false nil] nil] nil] nil])
   false)
(= (P95 '(:a nil ()))
   false)
; Scratch
(count '(:a (:b nil nil) nil))
(sequential? '(:a (:b nil nil) nil))
(tree-seq seq? identity '((1 2 (3)) (4)))


; P96
; Eli tehdään kaksi traversal-funktiota: tr1 kulkee vasenta puolta, tr2 oikeaa puolta.
; Kummankin pitää palauttaa samat alkiot samassa järjestyksessä, jos puut ovat symmetrisiä.
; HUOM: On tärkeää, että jos nil, niin palautetaan [x] eikä x, koska näin nähdään,
; että '(:a (:b nil nil) nil) ei ole symmetrinen.
(def P96 (fn [x] (letfn [(tr1 [x] (if (nil? x) [x] (concat [(first x)] (tr1 (second x)) (tr1 (nth x 2)))))
                         (tr2 [x] (if (nil? x) [x] (concat [(first x)] (tr2 (nth x 2)) (tr2 (second x)))))]
                   (= (tr1 x) (tr2 x)))))
; Muiden:
; TODO: kannattaa tutkia lisää näitä ratkaisuja.
;
(P96 '(:a (:b nil nil) nil))
(P96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
      [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
(P96 '(:a (:b nil nil) (:b nil nil)))
;
(= (P96 '(:a (:b nil nil) (:b nil nil))) true)
(= (P96 '(:a (:b nil nil) nil)) false)
(= (P96 '(:a (:b nil nil) (:c nil nil))) false)
(= (P96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
         [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
   true)
(= (P96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
         [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
   false)
(= (P96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
         [2 [3 nil [4 [6 nil nil] nil]] nil]])
   false)
; Scratch
(= '(1 2 3 4 5 (1 1) 2 3 4 6 5) '(1 2 3 4 5 (1 2) 2 3 4 6 5))
(concat [1] [2])
(concat [2] nil)


; P97
(def P97 (fn [n] (-> (iterate (fn [xs] (concat [1] (map (fn [[a b]] (+ a b)) (partition 2 1 xs)) [1]))
                              [1]) ; Iteraattorin alkuarvo on [1]
                     (nth (dec n)))))

(P97 5)
;

(= (P97 1) [1])
(= (map P97 (range 1 6))
   [[1]
    [1 1]
    [1 2 1]
    [1 3 3 1]
    [1 4 6 4 1]])
(= (P97 11)
   [1 10 45 120 210 252 210 120 45 10 1])
; Scratch
(partition 2 1 [1])
(partition 2 1 [1 1])
(partition 2 1 [1 2 1])
(vec (concat [1] (map (fn [[a b]] (+ a b)) (partition 2 1 [1 3 3 1])) [1]))


; P98
(def P98)



; P99
; HUOM! Clojurescript-ratkaisu hieman erilainen, ks. nodesolutions1!
(def P99 (fn [x1 x2] (mapv (fn [x] (-> x int (- 48))) (seq (str (* x1 x2))))))
;
(= (P99 1 1) [1])
(= (P99 99 9) [8 9 1])
(= (P99 999 99) [9 8 9 0 1])
; Scratch
(char \8)
(int \0)
(int \9)
(mapv (fn [x] (-> x int (- 48))) (seq (str 891)))


; P100
; https://en.wikipedia.org/wiki/Least_common_multiple
; https://www.calculatorsoup.com/calculators/math/lcm.php
; https://www.youtube.com/watch?v=5oMdTsfBCYc
; HUOM! Ei toimi sivustolla, koska ratio? ei ole Javascriptissa. Mutta läpäisee testit.
; Turha käsitellä fraktioita kuten videossa, koska integerien ja fraktioiden käsittely
; yhdessä toimii kuten alla olevassa oman ratkaisun geneerisemmässä ratkaisussa.
(def P100 (fn [& ns] (letfn [(gcd [a b] (if (= b 0) a (recur b (mod a b))))
                             (hcf [xs] (reduce (fn [acc x] (gcd acc x)) 0 xs))
                             (min-idx [xs] (->> xs (map-indexed vector) (apply min-key second) first))
                             (lcm-h [nums cands] (if (apply = cands)
                                                   cands
                                                   (let [i (min-idx cands)]
                                                     (lcm-h nums (assoc cands i (+ (nth cands i) (nth nums i)))))))
                             (lcm [nums] (first (lcm-h nums nums)))]
                       (let [rats (filter ratio? ns)
                             ints1 (filter (complement ratio?) ns)
                             ints2 (mapv numerator rats)
                             nums (vec (concat ints1 ints2))
                             dens (mapv denominator rats)
                             lcm_n (lcm nums)
                             hcf_n (let [tmp (hcf dens)]
                                     (if (= tmp 0) 1 tmp))]
                         (/ lcm_n hcf_n)))))
; Muiden
; HUOM: Paljon parempi!
(def P100 (fn [& x]
            (let [y (apply min x)]
              (loop [z y]
                (if (every? #(zero? (mod z %)) x)
                  z
                  (recur (+ z y)))))))
; Oman ratkaisun geneerisempi ratkaisu.
(def P100 (fn [& numbers]
            (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
              (/
                (apply * numbers)
                (reduce gcd numbers)))))
; Tämä ratkaisu ei jää jumiin sivulle.
(def P100 (fn [e & r]
            ((fn f [p]
               (if (every? #(= (mod p %) 0) r)
                 p
                 (f (+ p e)))) e)))
;
(P100 2 3)
(P100 3/4 1/6)
(P100 7 5/7 2 3/5)
;
(== (P100 2 3) 6)
(== (P100 5 3 7) 105)
(== (P100 1/3 2/5) 2)
(== (P100 3/4 1/6) 3/2)
(== (P100 7 5/7 2 3/5) 210)
; Scratch
(* 7 5/7 2 3/5)
(/ (* 245 25 70 21) 35)
(/ 7350 35)
(map numerator '(5/7 3/5))
(map denominator '(5/6 3/2))
(rationalize 3)
(* 5 5/7)
(/ 90 12)
(/ 45 6)
(filter (complement ratio?) '(7 5/7 2 3/5))
(map denominator (filter ratio? '(7 5/7 2 3/5)))
(->> [1 2 4 0 5]
     (map-indexed vector) ; [[0 1] [1 2] [2 4] [3 0] [4 5]]
     (apply min-key second) ; [3 0]
     first)
(/ 32 24)
(let [gcd (fn [a b] (if (= b 0) a (recur b (mod a b))))]
  (reduce gcd [342 12 240]))




; P101
(def P101)

; P102
(def P102)

; P103
(def P103)

; P104
(def P104)

; P105
(def P105)

; P106
(def P106)

; P107
(def P107 (fn [n] (fn [x] (if (= n 0) 1 (nth (iterate (partial * x) x) (dec n))))))
; Käytä oletusarvoa 1, jos nth index ei löydy.
(def P107 (fn [n] (fn [x] (nth (iterate (partial * x) x) (dec n) 1))))
; Muiden
(def P107 (fn [n] #(int (Math/pow % n))))
(def P107 (fn [n] (fn [x] (apply * (repeat n x)))))
;
(map (P107 3) [1 2 3 4])
;
(= 256 ((P107 2) 16), ((P107 8) 2))
(= [1 8 27 64] (map (P107 3) [1 2 3 4]))
(= [1 2 4 8 16] (map #((P107 %) 2) [0 1 2 3 4]))
; Scratch
(take 5 (iterate (partial * 5) 5))
(repeat 5 6)
(repeatedly 5 #(rand-int 10))
(def jee (partial * 5))
(jee 6)
(def pow2 (P107 2))
(pow2 3)



; P108
(def P108)

; P109
(def P109)

; P110
(def P110)

; P111
(def P111)

; P112
(def P112)

; P113
(def P113)

; P114
(def P114)

; P115
(def P115)

; P116
(def P116)

; P117
(def P117)

; P118
(def P118 map)
; HUOM: Ilmeisesti ei ole mahdollsta tehdä loop:lla. Kysy myöhemmin Metosin slackissa, miksi?
; Ei toimi: Stack overflow.
(def P118 (fn [f xs] (reverse (loop [acc '() xs xs]
                                (if (empty? xs)
                                  acc
                                  (recur (cons (f (first xs)) acc) (rest xs)))))))
; Eikä tämä.
(def P118 (fn [f xs] (reverse (lazy-seq (loop [acc '() xs xs]
                                          (if (empty? xs)
                                            acc
                                            (recur (cons (f (first xs)) acc) (next xs))))))))
; Tämäkään ei toimi.
(def P118 (fn [f xs] (reverse (loop [acc '() xs xs]
                                (if (empty? xs)
                                  acc
                                  (recur (lazy-seq (cons (f (first xs)) acc)) (rest xs)))))))
; Eikä tämä.
(def P118 (fn [f xs] (reverse (loop [acc '() xs xs]
                                (if (empty? xs)
                                  acc
                                  (recur (lazy-seq (cons (f (first xs)) acc)) (next xs)))))))
; Tämäkään ei toimi.
(def P118 (fn [f xs] (reduce (fn [acc x] (conj acc (f x))) [] xs)))
; Eikä tämä.
(def P118 (fn [f xs] (lazy-seq (reduce (fn [acc x] (conj acc (f x))) [] xs))))
; Eikä tämä.
(def P118 (fn [f xs] (reduce (fn [acc x] (lazy-seq (conj acc (f x)))) [] xs)))
; Toimii: https://clojure.org/reference/lazy
(def P118 (fn [f coll]
            (lazy-seq
              (when-let [s (seq coll)]
                (cons (f (first s)) (P118 f (rest s)))))))
(def P118 map)
"asdf"
(P118 inc [2 3 4 5 6])
(= [3 4 5 6 7]
   (P118 inc [2 3 4 5 6]))
(= (repeat 10 nil)
   (P118 (fn [_] nil) (range 10)))
(= [1000000 1000001]
   (->> (P118 inc (range))
        (drop (dec 1000000))
        (take 2)))
(= [1000000 1000001]
   (->> (P118 inc (range))
        (drop (dec 1000000))
        (take 2)))
; Scratch
"asdf"
(->> (P118 inc (range))
     (drop (dec 100))
     (take 2))
(->> (map inc (range))
     (drop (dec 100))
     (take 2))
(take 5 (range))
(seq [2 3 4 5 6])


; P119
(def P119)

; P120
(require '[hashp.core])
; Works in JVM
(def P120 (fn [xs] (count (filter (fn [x]
                                    (let [nums (map (comp #(* % %) #(- % 48) int) (seq (str x)))]
                                      (< x (apply + nums))))
                                  xs))))
; Works in Clojurescript.
#_ (def P120 (fn [xs] (count (filter (fn [x]
                                    (let [nums (map (comp #(* % %) int) (seq (str x)))]
                                      (< x (apply + nums))))
                                  xs))))
(P120 (range 10))
(= 8 (P120 (range 10)))
(= 19 (P120 (range 30)))
(= 50 (P120 (range 100)))
(= 50 (P120 (range 1000)))
; Scratch
(map (comp #(- % 48) int) (seq "40"))
(str 9)


; P121
(def P121)

; P122
(require '[hashp.core])
(def P122 (fn [s] (let [muls (reverse (map (comp #(- % 48) int) (seq s)))
                        exps (iterate inc 0)]
                    (apply + (map (fn [mu ex] (int (* mu (Math/pow 2 ex)))) muls exps)))))
; No Math/pow in Clojurescript. Implement yourself.
(def P122 (fn [s] (let [mypow (fn [x] (fn [n] (apply * (repeat n x))))
                        mypow2 (mypow 2)
                        muls (reverse (map (comp #(- % 48) int) (seq s)))
                        exps (iterate inc 0)]
                    (apply + (map (fn [mu ex] (int (* mu (mypow2 ex)))) muls exps)))))
; Clojurescript versio.
#_(def P122 (fn [s] (let [mypow (fn [x] (fn [n] (apply * (repeat n x))))
                        mypow2 (mypow 2)
                        muls (reverse (map int (seq s)))
                        exps (iterate inc 0)]
                    (apply + (map (fn [mu ex] (int (* mu (mypow2 ex)))) muls exps)))))
; Muiden
(def P122 #(Integer/parseInt % 2))
;
(P122 "101")
"asdf"
;
(= 0     (P122 "0"))
(= 7     (P122 "111"))
(= 8     (P122 "1000"))
(= 9     (P122 "1001"))
(= 255   (P122 "11111111"))
(= 1365  (P122 "10101010101"))
(= 65535 (P122 "1111111111111111"))
; Scratch
(map (comp #(- % 48) int) (seq "101"))
(take 5 (iterate inc 0))
(map (fn [x y] [x y]) [1 2 3] [5 6 7])
(apply + '(1.0 0.0 4.0))
(def mypow (fn [x] (fn [n] (apply * (repeat n x)))))
(def mypow2 (mypow 2))
(mypow2 4)

; P123
(def P123)

; P124
(def P124)

; P125
(def P125)

; P126
(def P126)

; P127
(def P127)

; P128
(require '[hashp.core])
(def P128 (fn [card] (let [[s r] card
                           ranks {\2 0 \3 1 \4 2 \5 3 \6 4 \7 5 \8 6 \9 7 \T 8 \J 9 \Q 10 \K 11 \A 12 }
                           suits {\D :diamond \H :heart \C :club \S :spade}]
                       {:suit (suits s)
                        :rank (ranks r)})))

(P128 "D2")
(= {:suit :diamond :rank 10} (P128 "DQ"))
(= {:suit :heart :rank 3} (P128 "H5"))
(= {:suit :club :rank 12} (P128 "CA"))
(= (range 13) (map (comp :rank P128 str)
                                      '[S2 S3 S4 S5 S6 S7
                                        S8 S9 ST SJ SQ SK SA]))
; Scratch


; P129
(def P129)

; P130
(def P130)

; P131
(def P131)

; P132
(def P132)

; P133
(def P133)

; P134
(def P134 (fn [k m] (and (contains? m k) (nil? (k m)))))
(true? (P134 :a {:a nil :b 2}))
(false? (P134 :b {:a nil :b 2}))
(false? (P134 :c {:a nil :b 2}))

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
(P135c 10 / 2 - 1 * 2)
(P135d 10 / 2 - 1 * 2)
(apply P135 '(10 / 2 - 1 * 2))
(apply P135 (list 10 / 2 - 1 * 2))
(apply P135 [10 / 2 - 1 * 2])
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
(time (apply P135b (take 1000000 (infix-list))))
(time (apply P135c (take 1000000 (infix-list))))
(time (apply P135d (take 1000000 (infix-list))))
(time (apply P135 (take 1000000 (infix-list))))

(time (+ 12 2))
(P135 10 / 2 - 1 * 2)
(apply P135 '(10 / 2 - 1 * 2))
(apply P135 (take 10 (infix-list)))

; P136
(def P136)

; P137
(def P137)

; P138
(def P138)

; P139
(def P139)

; P140
(def P140)

; P141
(def P141)

; P142
(def P142)

; P143
(def P143 (fn [xs1 xs2] (reduce + (map (fn [a b] (* a b)) xs1 xs2))))
; Muiden
(def P143 #(apply + (map * % %2)))
;
(P143 [1 2 3] [4 5 6])
(= 0 (P143 [0 1 0] [1 0 0]))
(= 3 (P143 [1 1 1] [1 1 1]))
(= 32 (P143 [1 2 3] [4 5 6]))
(= 256 (P143 [2 5 6] [100 10 1]))

; P144
(def P144)

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

; P146
(def P146 (fn [m] (into {}
                        (mapcat (fn [[k v]]
                                  (let [vs (map (fn [[k1 v1]] [[k k1] v1]) v)]
                                    vs)) m))))
; Ai niin, piti tehdä for:
(def P146 (fn [m]
            (into {}
                  (for [[k v] m
                        [k1 v1] v]
                    [[k k1] v1]))))
;
(P146 '{a {p 1, q 2}
           b {m 3, n 4}})
(= (P146 '{a {p 1, q 2}
           b {m 3, n 4}})
   '{[a p] 1, [a q] 2
     [b m] 3, [b n] 4})
(= (P146 '{[1] {a b c d}
           [2] {q r s t u v w x}})
   '{[[1] a] b, [[1] c] d,
     [[2] q] r, [[2] s] t,
     [[2] u] v, [[2] w] x})
(= (P146 '{m {1 [a b c] 3 nil}})
   '{[m 1] [a b c], [m 3] nil})

; P147
(def P147)

; P148
(def P148)

; P149
(def P149)

; P150
(def P150)

; P151
(def P151)

; P152
(def P152)

; P153
(def P153)

; P154
(def P154)

; P155
(def P155)

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


; P157
(def P157)

; P158
(def P158)

; P159
(def P159)

; P160
(def P160)

; P161f
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

; P163
(def P163)

; P164
(def P164)

; P165
(def P165)

; P166
(def P166)

; P167
(def P167)

; P168
(def P168)

; P169
(def P169)

; P170
(def P170)

; P171
(def P171)

; P172
(def P172)

; P173
(def P173)

; P174
(def P174)

; P175
(def P175)

; P176
(def P176)

; P177
(def P177)

; P178
(def P178)

; P179
(def P179)

; P180
(def P180)

; P181
(def P181)

; P182
(def P182)

; P183
(def P183)

; P184
(def P184)

; P185
(def P185)

; P186
(def P186)

; P187
(def P187)

; P188
(def P188)

; P189
(def P189)

; P190
(def P190)

; P191
(def P191)

; P192
(def P192)

; P193
(def P193)

; P194
(def P194)

; P195
(def P195)

; P196
(def P196)

; P197
(def P197)

; P198
(def P198)

; P199
(def P199)



















