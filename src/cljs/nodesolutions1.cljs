(ns nodesolutions1)

; First:
; just shadow-node
; ... wait till you see "Build completed", then:
; just run-node
; Then start node REPL in Cursive (connect to .shadowin this window
; (shadow.cljs.devtools.api/repl :app)
; HUOM! Älä ota pois tätä main! koska shadow-cljs vaatii sen.
(defn main! [])


;; Works only with Java
;;(def P29 (fn [s] (apply str (filter #(Character/isUpperCase %) s))))
;; Javascript
;(def P29 (fn [s] (apply str (filter (fn [c] (= c (.toUpperCase c))) s))))
;;(def P29 (fn [s] (apply str (filter (fn [c] (= c (.toUpperCase c))) s))))
;; Varsinainen ratkaisu:
;(def P29 (fn [s] (apply str (filter (set (map char (range 65 91))) s))))
;(P29 "HeLlO, WoRlD!")
;@@ -224,13 +224,54 @@
;
;
;; P30
;(def P30 )
;(def P30 (fn [xs] (let [lst (->> xs
;                        (partition 2 1)
;                        (remove (fn [[a b]] (= a b))))
;                        ; We need to add the last item in the right position.
;                        [a b] (last lst)
;                        ; Convert to vector so that we add to the end.
;                        lst (conj (vec lst) [b a])]
;                    (map (fn [[a b]] a) lst))))
;(def P30 (fn [xs] (reduce (fn [acc x] (let [z (last acc)] (if (= z x) acc (conj acc x)))) [] xs)))
;(P30 [1 1 2 3 3 2 2 3])
;;
;; Muiden:
;(def P30 (fn [x] (reduce #(if (= (last %) %2) % (conj % %2)) [] x)))
;(def P30 #(map first (partition-by identity %)))
;;
;(= (apply str (P30 "Leeeeeerrroyyy")) "Leroy")
;(= (P30 [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
;(= (P30 [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))
;; Scratch
;(->> "Leeeeeerrroyyy"
;     (partition 2 1)
;     (remove (fn [[a b]] (= a b))))
;(partition-by identity "Leeeeeerrroyyy")
;
;
;; P31
;(def P31 )
;(def P31 (fn [xs] (partition-by identity xs)))
;(P31 [1 1 2 1 1 1 3 3])
;;
;
;(= (P31 [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
;(= (P31 [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
;(= (P31 [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))
;; Scratch
;(partition-by identity [1 1 2 1 1 1 3 3])
;
;; P32
;(def P32 )
;(def P32 (fn [xs] (mapcat (fn [x] [x x]) xs)))
;; Muiden
;(P32 #(interleave % %))
;(= (P32 [1 2 3]) '(1 1 2 2 3 3))
;(= (P32 [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
;(= (P32 [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
;(= (P32 [44 33]) [44 44 33 33])
;; Scratch
;(->> [1 2 3]
;     (mapcat (fn [x] [x x])))
;
;; P99
;(def P99 (fn [x1 x2] (mapv (fn [x] (-> x int )) (seq (str (* x1 x2))))))
;; Muiden
;
;(= (P99 1 1) [1])
;(= (P99 99 9) [8 9 1])
;(= (P99 999 99) [9 8 9 0 1])
;; Scratch
;(char \8)
;(int \0)
;(int \9)
;(mapv (fn [x] (-> x int )) (seq (str 891)))
;(list 7 5/7 2 3/5)
;
