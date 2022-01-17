(ns solutions1
  (:require [hashp.core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(def P22 (fn [lst] (reduce (fn [acc x] (let [_ #p acc
                                           _ #p x] (+ 1 acc))) 0 (seq lst))))
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
(= (P23 [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])

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
(= (P29 "HeLlO, WoRlD!") "HLOWRD")
(empty? (P29 "nothing"))
(= (P29 "$#A(*&987Zf") "AZ")
; Scratch
(char 65)
(char 90)
(set (map char (range 65 91)))



; P30
(def P30 )

; P31
(def P31 )

; P32
(def P32 )

; P33
(def P33 )

; P34
(def P34 )

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
(def P39 )

; P40
(def P40 )

; P41
(def P41 )

; P42
(def P42 )

; P43
(def P43 )

; P44
(def P44 )

; P45
(def P45 )

; P46
(def P46 )

; P47
(def P47 )

; P48
(def P48 )

; P49
(def P49 )

; P50
(def P50 )

; P51
(def P51 )

; P52
(= [2 4] (let [[a b c d e f g] (range)] [c e]))
; scratch
(take 10 (range))

; P53
(def P53 )

; P54
(def P54 )

; P55
(def P55 )

; P56
(def P56 )

; P57
(def P57 '(5 4 3 2 1))
(= P57 ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))

; P58
(def P58 )

; P59
(def P59 )

; P60
(def P60 )

; P61
(def P61 )

; P62
(def P62 )

; P63
(def P63 )

; P64
(def P64 +)
(= 15 (reduce P64 [1 2 3 4 5]))

; P65
(def P65 )

; P66
(def P66 )

; P67
(def P67 )

; P68
(def P68 '[7 6 5 4 3])
(= P68
  (loop [x 5
         result []]
    (if (> x 0)
      (recur (dec x) (conj result (+ 2 x)))
      result)))

; P69
(def P69 )

; P70
(def P70 )



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
(def P73 )

; P74
(def P74 )

; P75
(def P75 )

; P76
(def P76 )

; P77
(def P77 )

; P78
(def P78 )

; P79
(def P79 )

; P80
(def P80 )

; P81
(def P81 )

; P82
(def P82 )

; P83
(def P83 )

; P84
(def P84 )

; P85
(def P85 )

; P86
(def P86 )

; P87
(def P87 )

; P88
(def P88 )

; P89
(def P89 )

; P90
(def P90 )

; P91
(def P91 )

; P92
(def P92 )

; P93
(def P93 )

; P94
(def P94 )

; P95
(def P95 )

; P96
(def P96 )

; P97
(def P97 )

; P98
(def P98 )

; P99
(def P99 )

; P100
(def P100 )

; P101
(def P101 )

; P102
(def P102 )

; P103
(def P103 )

; P104
(def P104 )

; P105
(def P105 )

; P106
(def P106 )

; P107
(def P107 )

; P108
(def P108 )

; P109
(def P109 )

; P110
(def P110 )

; P111
(def P111 )

; P112
(def P112 )

; P113
(def P113 )

; P114
(def P114 )

; P115
(def P115 )

; P116
(def P116 )

; P117
(def P117 )

; P118
(def P118 )

; P119
(def P119 )

; P120
(def P120 )

; P121
(def P121 )

; P122
(def P122 )

; P123
(def P123 )

; P124
(def P124 )

; P125
(def P125 )

; P126
(def P126 )

; P127
(def P127 )

; P128
(def P128 )

; P129
(def P129 )

; P130
(def P130 )

; P131
(def P131 )

; P132
(def P132 )

; P133
(def P133 )

; P134
(def P134 (fn [k m] (and (contains? m k) (nil? (k m)))))
(true?  (P134 :a {:a nil :b 2}))
(false? (P134 :b {:a nil :b 2}))
(false? (P134 :c {:a nil :b 2}))

; P135
(def P135 )

; P136
(def P136 )

; P137
(def P137 )

; P138
(def P138 )

; P139
(def P139 )

; P140
(def P140 )

; P141
(def P141 )

; P142
(def P142 )

; P143
(def P143 )

; P144
(def P144 )

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
(def P146 )

; P147
(def P147 )

; P148
(def P148 )

; P149
(def P149 )

; P150
(def P150 )

; P151
(def P151 )

; P152
(def P152 )

; P153
(def P153 )

; P154
(def P154 )

; P155
(def P155 )

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
(def P157 )

; P158
(def P158 )

; P159
(def P159 )

; P160
(def P160 )

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
(def P163 )

; P164
(def P164 )

; P165
(def P165 )

; P166
(def P166 )

; P167
(def P167 )

; P168
(def P168 )

; P169
(def P169 )

; P170
(def P170 )

; P171
(def P171 )

; P172
(def P172 )

; P173
(def P173 )

; P174
(def P174 )

; P175
(def P175 )

; P176
(def P176 )

; P177
(def P177 )

; P178
(def P178 )

; P179
(def P179 )

; P180
(def P180 )

; P181
(def P181 )

; P182
(def P182 )

; P183
(def P183 )

; P184
(def P184 )

; P185
(def P185 )

; P186
(def P186 )

; P187
(def P187 )

; P188
(def P188 )

; P189
(def P189 )

; P190
(def P190 )

; P191
(def P191 )

; P192
(def P192 )

; P193
(def P193 )

; P194
(def P194 )

; P195
(def P195 )

; P196
(def P196 )

; P197
(def P197 )

; P198
(def P198 )

; P199
(def P199 )



















