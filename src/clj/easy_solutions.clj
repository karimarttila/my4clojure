(ns easy-solutions
    ; Turn off, since I provide many different solutions using the same def.
  {:clj-kondo/config '{:linters {:redefined-var {:level :off}}}}
  (:require [hashp.core] 
            [clojure.set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://4clojure.oxal.org/
;;
;; NOTE: In this namespace are the easy solutions.

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
;(require '[hashp.core])


; P19
; New 2025-01-25
(def P19 (fn [l] (first (reverse l))))
; Old.
(def P19 (fn [lst] (let [c (count lst)] (first (drop (- c 1) lst)))))
; Other developers' solutions:
(def P19 #(first (reverse %)))
; HYVÄ!
(def P19 (comp first reverse))
;
(= (P19 [1 2 3 4 5]) 5)
(= (P19 '(5 4 3)) 3)
(= (P19 ["b" "c" "d"]) "d")

; P20
; New 2025-01-27
(def P20 #((comp second reverse) %))
(def P20 #(-> % reverse second))
; Old.
(def P20 (fn [lst] (first (drop 1 (reverse lst)))))
(def P20 (fn [lst] (->> lst reverse (drop 1) first)))
(def P20 #(->> % reverse (drop 1) first))
; Other developers' solutions
(def P20 #(second (reverse %)))
(def P20 (comp last butlast))
(= (P20 (list 1 2 3 4 5)) 4)
(= (P20 ["a" "b" "c"]) "b")
(= (P20 [[1 2] [3 4]]) [1 2])

; P21
; New 2025-01-27
(def P21 #(first (drop %2 %1)))
; Old.
(def P21 (fn [lst n] (first (drop n lst))))
(def P21 #(->> %1 (drop %2) first))
(= (P21 '(4 5 6 7) 2) 6)
(= (P21 [:a :b :c] 0) :a)
(= (P21 [1 2 3 4] 1) 2)
(= (P21 '([1 2] [3 4] [5 6]) 2) [5 6])

; P22
; New 2025-01-27
(def P22 (fn [l] (reduce (fn [acc _] (inc acc)) 0 l)))
; Old.
(def P22 #(count %)) ; Ei saa käyttää count.
(def P22 (fn [lst] (reduce (fn [acc x] (+ 1 acc)) 0 (seq lst))))
(def P22 (fn [lst] (reduce (fn [acc x] (+ 1 acc)) 0 (seq lst))))
(def P22 (fn [lst] (reduce (fn [acc x] (+ 1 acc)) 0 lst)))
; Other developers' solutions
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
; New 2025-01-27
(def P23 (fn [c] (reduce (fn [acc x] (cons x acc)) '() c)))
; Old.
(def P23 (fn [lst] (reduce (fn [acc x] (conj acc x)) '() lst)))
(def P23 #(reduce (fn [acc x] (conj acc x)) '() %))
; Other developers' solutions
; HYVÄ!
(def P23 (fn [xs] (into () xs)))
(def P23 #(into () %))
(def P23 #(reduce conj () %))
;
(= (P23 [1 2 3 4 5]) [5 4 3 2 1])
(= (P23 (sorted-set 5 7 2 7)) '(7 5 2))
(= (P23 [[1 2] [3 4] [5 6]]) [[5 6] [3 4] [1 2]])

; P24
; New 2025-01-27
(def P24 (fn [s] (reduce + s)))
(def P24 #(reduce + %))
; Old.
(def P24 #(apply + %))
; Other developers' solutions
(def P24 #(reduce + %))
;
(= (P24 [1 2 3]) 6)
(= (P24 (list 0 -2 5 5)) 8)
(= (P24 #{4 2 1}) 7)
(= (P24 '(0 0 -1)) -1)
(= (P24 '(1 10 3)) 14)

; P25
; New 2025-01-27
(def P25 (fn [s] (filter odd? s)))
(def P25 #(filter odd? %))
; Old.
(def P25 #(filter odd? %))
(= (P25 #{1 2 3 4 5}) '(1 3 5))
(= (P25 [4 2 1 6]) '(1))
(= (P25 [2 2 4 6]) '())
(= (P25 [1 1 1 3]) '(1 1 1 3))

; P26
; New 2025-01-27
; Hieman kökkö, mutta toimii.
(def P26 (fn [n]
           (let [fib (fn [v num]
                       (if (> num 2)
                         (let [x (last v)
                               y (last (butlast v))
                               z (+ x y)]
                           (recur (conj v z) (dec num)))
                         v))]
             (cond
               (= n 1) [1]
               (= n 2) [1 1]
               (> n 2) (fib [1 1] n)))))
; Old.
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
; New 2025-01-28
(def P27 #(= (seq %) (reverse %)))
; Old.
(def P27 #(= (seq %) (reverse %)))
(false? (P27 '(1 2 3 4 5)))
(true? (P27 "racecar"))
(true? (P27 [:foo :bar :foo]))
(true? (P27 '(1 1 3 3 1 1)))
(false? (P27 '(:a :b :c)))
; Scratch
(reverse "racecar")

; P28
; New 2025-01-28
(def P28 (fn [input]
           (letfn [(my-fun [xs]
                     (reduce (fn [acc x]
                               (if (sequential? x)
                                 (concat acc (my-fun x))
                                 (concat acc [x]))) [] xs))]
             (my-fun input))))
; Am I getting old? My solution a couple of years ago is a lot shorter.
; Old.
(def P28 (fn [x]
           (cond
             (sequential? x) (mapcat P28 x)
             :else [x])))

(def P28 (fn [x]
           (cond
             (sequential? x) (remove nil? (concat (P28 (first x)) (P28 (next x))))
             :else [x])))

(P28 '(1 (2 3)))
; Other developers' solutions:
; Ei saanut käyttää flatten.
(def P28 (fn [x] (flatten x)))
(def P28 (fn flat [c]
           (if (sequential? c)
             (mapcat flat c)
             (list c))))
(= (P28 '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
(= (P28 ["a" ["b"] "c"]) '("a" "b" "c"))
(= (P28 '((((:a))))) '(:a))
; Scratch
(cons 1 nil)
(tree-seq seq? identity '((1 2 (3)) (4)))
(P28 '((1 2) 3 [4 [5 6]]))
(mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]])
(map reverse [[3 2 1 0] [6 5 4] [9 8 7]])


; P29
; New 2025-01-29.
(def P29 (fn [s]
           (let [caps (set (map char (range (int \A) (inc (int \Z)))))
                 filtered (filter (fn [x]
                                    (caps x))
                                  s)]
             (apply str filtered))))
; Old.
; Works only with Java
;(def P29 (fn [s] (apply str (filter #(Character/isUpperCase %) s))))
; Javascript
;(def P29 (fn [s] (apply str (filter (fn [c] (= c (.toUpperCase c))) s))))
; Varsinainen ratkaisu:
(def P29 (fn [s] (apply str (filter (set (map char (range 65 91))) s))))
(P29 "HeLlO, WoRlD!")
; Other developers' solutions:
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
; New 2025-01-30.
(def P30 (fn [s]
           (let [check-type (fn [x]
                              (cond
                                (string? x) :string
                                (vector? x) :vector
                                (list? x) :list
                                :else :unknown))
                 my-type (check-type s)
                 result (:acc (reduce (fn [acc mys]
                                        (if (= (:prev acc) mys)
                                          {:acc (:acc acc) :prev mys}
                                          {:acc (conj (:acc acc) mys) :prev mys})) {:acc [] :prev ""} (seq s)))]
             (cond
               (= my-type :string) (reduce str result)
               (= my-type :vector) (vec result)
               (= my-type :list) (list result)
               :else :error))))
; WTF? Am I getting old? My previous solutions were a lot shorter than the new one.
;Old
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
; Other developers' solutions:
; Why didn't I figure this one. This is a real beauty. Clear and concise. Maybe I didn't realize to use `last`?
(def P30 (fn [x] (reduce #(if (= (last %) %2) % (conj % %2)) [] x)))
; This is also genious.
(def P30 #(map first (partition-by identity %)))
(partition-by identity "Leeeeeerrroyyy") ; => Then just map with first...
;;=> ((\L) (\e \e \e \e \e \e) (\r \r \r) (\o) (\y \y \y))
; ... it really is a good to do these exercises: you get to know the standard library  
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
; New 2025-01-31
; I learned from the previous exercise P31.
(def P31 (fn [xs] (partition-by identity xs)))
; Old.
(def P31 (fn [xs] (partition-by identity xs)))
(P31 [1 1 2 1 1 1 3 3])
;
(= (P31 [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
(= (P31 [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
(= (P31 [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))
; Scratch
(partition-by identity [1 1 2 1 1 1 3 3])

; P32
; New 2025-01-31.
; Damn, I'm getting better at this.
(def P32 (fn [xs] (mapcat (fn [x] [x x]) xs)))
; Old. Or not - same function a few years ago.
(def P32 (fn [xs] (mapcat (fn [x] [x x]) xs)))
; Other developers' solutions
; This is good out-of-the-box thinking.
(def P32 #(interleave % %))
(= (P32 [1 2 3]) '(1 1 2 2 3 3))
(= (P32 [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
(= (P32 [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
(= (P32 [44 33]) [44 44 33 33])
; Scratch
(->> [1 2 3]
     (mapcat (fn [x] [x x])))


; P33
; New 2025-01-31
(def P33 (fn [xs n] (mapcat (fn [x] (take n (iterate identity x))) xs)))
; I examined other developers' solutions. I should have used repeat.
; Old.
; Hm. That was also good.
(def P33 (fn [xs n] (apply interleave (for [n (range n)] xs))))
(P33 [1 2 3] 2)
; Other developers' solutions
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
; New 2025-01-31
(def P34 (fn [b e]
           (take (- e b) (iterate inc b))))
; Exactly the same, even the symbols (b = begin, e = end). Did I remember this?
; Old.
(def P34 (fn [b e] (take (- e b) (iterate inc b))))
(P34 1 4)
(= (P34 1 4) '(1 2 3))
(= (P34 -2 2) '(-2 -1 0 1))
(= (P34 5 8) '(5 6 7))


; P38
; New 2025-01-31
(def P38 (fn [& r] (-> r sort last)))
; Old
(def P38 (fn [& xs] (reduce (fn [acc x] (if (> x acc) x acc)) (first xs) xs)))
(def P38 (fn [& xs] (-> xs sort reverse first)))
(def P38 (comp first reverse sort list))
; Other developers' solutions
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
; New 2025-01-31
(def P39 (fn [xs xy] (flatten (map (fn [x y] [x y]) xs xy))))
; Old
(def P39 (fn [xs1 xs2] (interleave xs1 xs2)))
(def P39 (fn [xs1 xs2] (mapcat (fn [a b] [a b]) xs1 xs2)))
; Other developers' solutions
(def P39 (fn [c1 c2] (mapcat list c1 c2)))
(P39 [1 2 3] [:a :b :c])
(= (P39 [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
(= (P39 [1 2] [3 4 5 6]) '(1 3 2 4))
(= (P39 [1 2 3 4] [5]) [1 5])
(= (P39 [30 20] [25 15]) [30 25 20 15])
; Scratch


; P40
; New 2025-02-01
(def P40 (fn [n xs] (drop 1 (mapcat (fn [x y] [x y]) (iterate identity n) xs))))
; Old
(def P40 (fn [sep xs] (interpose sep xs)))
(def P40 (fn [sep xs] (drop-last (interleave xs (iterate identity sep)))))
; Other developers' solutions
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
; New 2025-02-02
; Just realized: Should the [:pad :pad :pad] be using iterate? Try later.
(def P41 (fn [xs n]
           (->> xs
                (partition n n [:pad :pad :pad])
                (mapcat (fn [x] (take (- n 1) x)))
                (remove #(= :pad %)))))
; Ok, didn't know about partition-all, let's refine the solution:
(def P41 (fn [xs n]
           (->> xs
                (partition-all n)
                (mapcat (fn [x] (take (dec n) x))))))
; Old.
; Ei ihan toimi.
(def P41 (fn [xs n] (apply concat (partition (- n 1) n xs))))
(def P41 (fn [xs n] (loop [acc [] lst xs n n]
                      (if (empty? lst)
                        (vec (apply concat acc))
                        (let [add (take (- n 1) lst)
                              xs2 (drop n lst)]
                          (recur (conj acc add) xs2 n))))))
; Other developers' solutions
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
; New 2025-02-03
(def P42 (fn [num]
           (reduce (fn [acc n]
                     (if (= n 1)
                       acc
                       (* acc n)))
                   1 (range 1 (inc num)))))
; Damn, my old solution is shorter. And better. I'm getting dumber.
; Old
(def P42 (fn [n] (->> n inc (range 1) (apply *))))
; Other developers' solutions. 
; A beauty.
(def P42 #(reduce * (range 2 (inc %))))

(P42 5)
(= (P42 1) 1)
(= (P42 3) 6)
(= (P42 5) 120)
(= (P42 8) 40320)

; P45
(def P45 '(1 4 7 10 13))
(= P45 (take 5 (iterate #(+ 3 %) 1)))


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
; New 2025-02-03
; Should not have looked the split-at documentation. :-)
(def P49 (fn [n xs] [(take n xs) (drop n xs)]))
; Old.
(def P49 (fn [n xs] [(take n xs) (drop n xs)]))
; Other developers' solutions
; What?
(def P49 (juxt take drop))
(P49 3 [1 2 3 4 5 6])

(= (P49 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
(= (P49 1 [:a :b :c :d]) [[:a] [:b :c :d]])
(= (P49 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])


; P51
(def P51 [1 2 3 4 5])
(= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] P51] [a b c d]))


; P61
; New 2025-02-03
(def P61 (fn [ks vs]
           (let [pairs (map (fn [x y] [x y]) ks vs)]
             (reduce (fn [acc [k v]]
                       (assoc acc k v))
                     {} pairs))))
; Seems to be that I got the same solution as previously.
; Old
(def P61 (fn [xs1 xs2] (zipmap xs1 xs2)))
(def P61 (fn [xs1 xs2] (reduce (fn [acc [k v]] (assoc acc k v)) {} (map (fn [a b] [a b]) xs1 xs2))))
(def P61 (fn [xs1 xs2] (reduce (fn [acc [k v]] (assoc acc k v)) {} (partition 2 (interleave xs1 xs2)))))
(P61 [:a :b :c] [1 2 3])
; Other developers' solutions
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
; New 2025-02-04
(def P62 (fn [f n]
           (letfn [(iter [g m]
                     (lazy-seq (cons m (iter g (g m)))))]
             (iter f n))))
; I kind of like better my new solution.
; Elaborated a bit after looking other developers' solutions.
(def P62 (fn [f n]
           (letfn [(iter [g m]
                     (cons m (lazy-seq (iter g (g m)))))]
             (iter f n))))
; Old
(def P62 (fn [f x] (lazy-seq (cons x (P62 f (f x))))))
;
(take 5 (P62 #(* 2 %) 1))
(= (take 5 (P62 #(* 2 %) 1)) [1 2 4 8 16])
(= (take 100 (P62 inc 0)) (take 100 (range)))
(= (take 9 (P62 #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))

; P63
; New 2025-02-05
(def P63 (fn [f xs]
           (let [items (map (fn [x] [(f x) x]) xs)]
             (reduce (fn [acc [k v]]
                       (let [myv (acc k)]
                         (if (nil? myv)
                           (assoc acc k [v])
                           (assoc acc k (conj myv v))))) {} items))))
; My old solution is almost the same, but using or is a bit more elegant.
; Old
(def P63 (fn [f xs] (group-by f xs)))
; First make a sequence of [k v] pairs. Then reduce the pairs to a map.
(def P63 (fn [f xs] (reduce (fn [acc [k v]] (assoc acc k (conj (or (get-in acc [k]) []) v)))
                            {}
                            (map (fn [x] [(f x) x]) xs))))
; Parannus: get:lle voi antaa arvon, mikä palautetaan, jos key ei löydy.
(def P63 (fn [f xs] (reduce (fn [acc [k v]] (assoc acc k (conj (get acc k []) v)))
                            {}
                            (map (fn [x] [(f x) x]) xs))))
; Other developers' solutions
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


; P66
; New 2025-02-05
(def P66 (fn [x y]
           (let [m (max x y)
                 n (min x y)
                 cands (reverse (range 1 (inc m)))]
             (->> cands
                  (map (fn [x]
                         (if (= (mod n x) (mod m x) 0)
                           x
                           nil)))
                  (remove nil?)
                  first))))
; Old.
; https://brilliant.org/wiki/greatest-common-divisor/
(def P66 (fn [n1 n2] (apply max (map first (filter (fn [[a b]] (= a b))
                                                   (for [x1 (range 1 (inc n1))
                                                         x2 (range 1 (inc n2))
                                                         :when (and (= (mod n1 x1) 0)
                                                                    (= (mod n2 x2) 0))]
                                                     [x1 x2]))))))
; Other developers' solutions
(def P66 (fn [a b] (if (= b 0) a (recur b (mod a b)))))
;
(P66 4488 12240)
(= (P66 2 4) 2)
(= (P66 10 5) 5)
(= (P66 5 7) 1)
(= (P66 1023 858) 33)
; Scratch
(range 1 3)


; P81
; New 2025-02-05
(def P81 (fn [xs ys]
           (->> ys
                (map (fn [x]
                       (xs x)))
                (remove nil?)
                set)))
; Wow, wow. My old solution uses infamous loop/recur.
; Old.
(def P81 (fn [set1 set2] (loop [acc #{} xs set1]
                           (if (empty? xs)
                             acc
                             (let [x (first xs)]
                               (recur (if (set2 x) (conj acc x) acc)
                                      (drop 1 xs)))))))
; Other developers' solutions
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

; P83
; New 2025-02-06
(def P83 (fn [& args]
           (true? (and (some true? args) (some false? args)))))
; Old.
(def P83 (fn [& lst] (boolean (and (some true? lst) (not-every? true? lst)))))
; Other developers' solutions
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

; P88
; New 2025-02-06
(def P88 (fn [s1 s2]
           (let [both (clojure.set/intersection s1 s2)]
             (set (concat (remove both s1) (remove both s2))))))
; Old.
(def P88 (fn [set1 set2] (let [both (clojure.set/intersection set1 set2)
                               all (clojure.set/union set1 set2)]
                           (set (remove both all)))))
(def P88 (fn [set1 set2] (set (remove (clojure.set/intersection set1 set2)
                                      (clojure.set/union set1 set2)))))
; Other developers' solutions
(def P88 (fn [set1 set2] (reduce #((if (% %2) disj conj) % %2) set1 set2)))
(def P88 (fn [set1 set2] (reduce (fn [acc x] ((if (acc x) disj conj) acc x)) set1 set2)))
(def P88 (fn [set1 set2] (reduce (fn [acc x] (let [f (if (acc x) disj conj)] (f acc x))) set1 set2)))
;
(P88 #{1 2 3 4 5 6} #{1 3 5 7})
(= (P88 #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
(= (P88 #{:a :b :c} #{}) #{:a :b :c})
(= (P88 #{} #{4 5 6}) #{4 5 6})
(= (P88 #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})
; Scratch
(clojure.set/intersection #{1 2 3 4 5 6} #{1 3 5 7})
(clojure.set/union #{1 2 3 4 5 6} #{1 3 5 7})


; P90
; New 2025-02-06
(def P90 (fn [s1 s2]
           (set (for [a s1
                      b s2]
                  [a b]))))
; Exactly the same as old one.
; Old.
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


; P95
; New 2025-02-06
(def P95 (fn [t]
           (letfn [(leaf? [item] (not (coll? item)))
                   (tree? [cand]
                     (if (leaf? cand)
                       (nil? cand)
                       (if (= (count cand) 3)
                         (let [[_ l r] cand]
                           (and (tree? l)
                                (tree? r)))
                         false)))]
             (tree? t))))
; WTF? My old solution is an abomination. 
; I think that my new solution is rather beautifull, you can see the idea from the code.
; Old. 
; Mieti ratkaisua: Pitää tarkistaa, että binary-treen jokainen lehti on nil tai binary-tree.
(def P95 (fn [x] (or (nil? x) ; Ollaan lehdessä, jonka on oltava nil, tai... pitää olla
                     (and (sequential? x) ; ... sequence, jonka
                          (= (count x) 3) ; ... pituus on kolme
                          (P95 (second x)) ; ... ja jonka eka
                          (P95 (nth x 2)))))) ; ... ja toka lehti on myös binary-tree.
(P95 '(:a (:b nil nil) nil))
;
; Other developers' solutions
(def P95 (fn t [x]
           (or (nil? x) (and (sequential? x) (= 3 (count x)) (t (second x)) (t (nth x 2))) false)))
; This is pretty beautifull.
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
; New 2025-02-07
; We re-use most of the P95 solution regarding checking if it is a binary tree.
; It is a binary symmetric tree, if:
; 1. It is a valid tree.
; 2. Every branch is symmetric.
; We mirror the left side main branch, then it should be identical to the right side main branch.
(def P96 (fn [t]
           (letfn [(leaf? [item] (not (coll? item)))
                     ; Mirror all branches.
                   (mirror [t]
                     (if (leaf? t)
                       t
                       (let [[v l r] t]
                         [v (mirror r) (mirror l)])))
                   (tree? [cand]
                     (if (leaf? cand)
                       (nil? cand)
                       (if (= (count cand) 3)
                         (let [[_ l r] cand]
                           (and (tree? l)
                                (tree? r)))
                         false)))]
               ; 1. Valid tree, 2. Left branch mirrored is the same as right branch.
             (and (tree? t)
                  (let [[_ l r] t]
                    (= (mirror l) r))))))
; Strange. My new solution is nothing like the previous solution.
; Old.
; Eli tehdään kaksi traversal-funktiota: tr1 kulkee vasenta puolta, tr2 oikeaa puolta.
; Kummankin pitää palauttaa samat alkiot samassa järjestyksessä, jos puut ovat symmetrisiä.
; HUOM: On tärkeää, että jos nil, niin palautetaan [x] eikä x, koska näin nähdään,
; että '(:a (:b nil nil) nil) ei ole symmetrinen.
(def P96 (fn [x] (letfn [(tr1 [x] (if (nil? x) [x] (concat [(first x)] (tr1 (second x)) (tr1 (nth x 2)))))
                         (tr2 [x] (if (nil? x) [x] (concat [(first x)] (tr2 (nth x 2)) (tr2 (second x)))))]
                   (= (tr1 x) (tr2 x)))))
; Other developers' solutions:
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
; Other developers' solutions
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


; P107
(def P107 (fn [n] (fn [x] (if (= n 0) 1 (nth (iterate (partial * x) x) (dec n))))))
; Käytä oletusarvoa 1, jos nth index ei löydy.
(def P107 (fn [n] (fn [x] (nth (iterate (partial * x) x) (dec n) 1))))
; Other developers' solutions
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


; P120
(require '[hashp.core])
; Works in JVM
(def P120 (fn [xs] (count (filter (fn [x]
                                    (let [nums (map (comp #(* % %) #(- % 48) int) (seq (str x)))]
                                      (< x (apply + nums))))
                                  xs))))
; Works in Clojurescript.
#_(def P120 (fn [xs] (count (filter (fn [x]
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
; Other developers' solutions
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


; P126
; HUOM: Ei toimi Clojurescriptissa.
(def P126 Class)
(let [x P126] (and (= (class x) x) x))
; Scratch
(class java.lang.Class)


; P128
(require '[hashp.core])
(def P128 (fn [card] (let [[s r] card
                           ranks {\2 0 \3 1 \4 2 \5 3 \6 4 \7 5 \8 6 \9 7 \T 8 \J 9 \Q 10 \K 11 \A 12}
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
(P135 10 / 2 - 1 * 2)

; Other developers' solutions
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
; My Ve colleagues's solution.
(def P135c (fn [& exprs]
             (reduce (fn [acc x]
                       (if (fn? x)
                         (partial x acc)
                         (acc x)))
                     exprs)))
#_(def P135cc (fn [& exprs]
                (reduce (fn [acc x]
                          (let [_ #p acc
                                _ #p x])
                          (if (fn? x)
                            (partial x acc)
                            (acc x)))
                        exprs)))
; My Va colleagues's solution.
(def P135d
  (fn calc ([x op y & r]
            (if op
              (recur (op x y) (first r) (second r) (drop 2 r))
              x))))
(P135 10 / 2 - 1 * 2)
(P135c 10 / 2 - 1 * 2)
#_(P135cc 10 / 2 - 1 * 2)
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
;(time (apply P135b (take 1000001 (infix-list)))) ; => StackOverFlowError
(time (apply P135c (take 1000001 (infix-list))))
(time (apply P135d (take 1000001 (infix-list))))
;(time (apply P135  (take 1000001 (infix-list)))) ; => StackOverFlowError

(time (+ 12 2))
(P135 10 / 2 - 1 * 2)
(apply P135 '(10 / 2 - 1 * 2))
(apply P135 (take 11 (infix-list)))


; P143
(def P143 (fn [xs1 xs2] (reduce + (map (fn [a b] (* a b)) xs1 xs2))))
; Other developers' solutions
(def P143 #(apply + (map * % %2)))
;
(P143 [1 2 3] [4 5 6])
(= 0 (P143 [0 1 0] [1 0 0]))
(= 3 (P143 [1 1 1] [1 1 1]))
(= 32 (P143 [1 2 3] [4 5 6]))
(= 256 (P143 [2 5 6] [100 10 1]))



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
(require '[hashp.core])
; Käytä 4Clojure-sivulla vain + eikä +'
(def P147 (fn [xs]
            (lazy-seq (when-let [s (seq xs)]
                        (let [x1 (first s)
                              xn (last s)
                              nxs (map #(apply +' %) (partition 2 1 s))]
                          (cons xs (P147 (vec (concat [x1] nxs [xn])))))))))
; Muista: iterate:lla tekee siistin lazy-seq:n!
(def P147 (fn [s] (iterate (fn [v]
                             (let [x1 (first v)
                                   xn (last v)
                                   nxs (map #(apply +' %) (partition 2 1 v))]
                               (vec (concat [x1] nxs [xn])))) s)))
; Other developers' solutions
; Eli laitetaan alkuun 0 ja loppuun nolla => menee limittäin ja lasketaan yhteen.

#_(def P147 (fn [coll] (iterate
                        (fn [xs]
                          (let [eka #p (conj (vec xs) 0)
                                toka #p (cons 0 (vec xs))]
                            (map +' eka toka)))
                        coll)))
(def P147 (fn [coll] (iterate #(map +' (conj (vec %) 0) (cons 0 (vec %))) coll)))
;
(= (second (P147 [2 3 2])) [2 5 5 2])
(= (take 5 (P147 [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])
(= (take 2 (P147 [3 1 2])) [[3 1 2] [3 4 3 2]])
(= (take 100 (P147 [2 4 2])) (rest (take 101 (P147 [2 2]))))
; Scratch
(partition 2 1 [2 3 2])
(def lazy-seq-example (fn [f coll]
                        (lazy-seq
                         (when-let [s (seq coll)]
                           (cons (f (first s)) (lazy-seq-example f (rest s)))))))
(map #(apply + %) (partition 2 1 [2 3 2]))
(take 5 (P147 [2 3 2]))


; P153
(require '[hashp.core])
(def P153 (fn [s]
            ; Eli tarkistetaan, että cs-setin jokainen item ei ole sama kuin
            ; missään xss seqin sisällä olevien xs-settien item.
            (let [checkit (fn [cs xss]
                            (every? false? (for [xs xss
                                                 x xs
                                                 c cs]
                                             (= c x))))]
              (if (empty? s)
                true
                (and (checkit (first s) (rest s)) (recur (rest s)))))))
; Other developers' solutions
(def P153 #(apply distinct? (mapcat seq %)))
(def P153 (fn [xs] (apply distinct? (mapcat seq xs))))
; TODO: Mukana muitakin hyviä ratkaisuja, tutki myöhemmin!

(P153 #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})

(= (P153 #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
   true)
(= (P153 #{#{:a :b :c :d :e}
           #{:a :b :c :d}
           #{:a :b :c}
           #{:a :b}
           #{:a}})
   false)
(= (P153 #{#{[1 2 3] [4 5]}
           #{[1 2] [3 4 5]}
           #{[1] [2] 3 4 5}
           #{1 2 [3 4] [5]}})
   true)
(= (P153 #{#{'a 'b}
           #{'c 'd 'e}
           #{'f 'g 'h 'i}
           #{''a ''c ''f}})
   true)
(= (P153 #{#{'(:x :y :z) '(:x :y) '(:z) '()}
           #{#{:x :y :z} #{:x :y} #{:z} #{}}
           #{'[:x :y :z] [:x :y] [:z] [] {}}})
   false)
(= (P153 #{#{(= "true") false}
           #{:yes :no}
           #{(class 1) 0}
           #{(symbol "true") 'false}
           #{(keyword "yes") ::no}
           #{(class '1) (int \0)}})
   false)
(= (P153 (set [(set [distinct?])
               (set [#(-> %) #(-> %)])
               (set [#(-> %) #(-> %) #(-> %)])
               (set [#(-> %) #(-> %) #(-> %)])]))
   true)
(= (P153 #{#{(#(-> *)) + (quote mapcat) #_nil}
           #{'+ '* mapcat (comment mapcat)}
           #{(do) set contains? nil?}
           #{#_empty?}})
   false)
; Scratch
(def P153 (fn [xs] (apply distinct? (mapcat seq xs))))
(P153 #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
(mapcat seq #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})


; P157
(def P157 (fn [xs] (map (fn [a b] [a b]) xs (range))))
; Other developers' solutions
(def P157 #(map-indexed (fn [a b] [b a]) %))
;
(P157 [:a :b :c])
(= (P157 [:a :b :c]) [[:a 0] [:b 1] [:c 2]])
(= (P157 [0 1 3]) '((0 0) (1 1) (3 2)))
(= (P157 [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])


; P166
; https://stackoverflow.com/questions/18289671/implementing-other-comparison-operators-in-terms-of-operator-in-one-call
(def P166 (fn [o x y]
            (cond
              (o x y) :lt
              (and (not (o x y)) (not (o y x))) :eq
              (o y x) :gt
              :else :panic!)))
; Other developers' solutions
(def P166 (fn [< x y]
            (cond (< x y) :lt
                  (< y x) :gt
                  :else :eq)))
(= :gt (P166 < 5 1))
(= :eq (P166 (fn [x y] (< (count x) (count y))) "pear" "plum"))
(= :lt (P166 (fn [x y] (< (mod x 5) (mod y 5))) 21 3))
(= :gt (P166 > 0 2))


; P173
; __ => f a
(= 3
   (let [[f a] [+ (range 3)]] (apply f a))
   (let [[[f a] b] [[+ 1] 2]] (f a b))
   (let [[f a] [inc 2]] (f a)))


