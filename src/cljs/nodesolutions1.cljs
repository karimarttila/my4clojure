(ns nodesolutions1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Node / Clojurescript experimentation.
;; https://4clojure.oxal.org/ runs solutions using Clojurescript - some Clojure / JVM specific and
;; Java interop does not work. Test in this namespace that the solution works with Clojurescript.

; First:
; just shadow-node
; ... wait till you see "Build completed", then:
; just run-node
; Then start node REPL in Cursive and give command in the REPL:
; (shadow.cljs.devtools.api/repl :app)
 (shadow.cljs.devtools.api/repl :app)
; Do not remove main since shadow-cljs requires it.

; Calva: 
; - Start the node nrepl as described above.
; - Evaluate


(defn main! [])

"asdf"
(+ 1 1)

(def my-obj #js {"a" 1 "b" 2})
(type my-obj)

(Math/abs -3)



(def P44 (fn [n xs]
          (let
           [c (count xs)
            n2 (Math/abs n)
            x (Math/abs (if (<= n2 c) n2 (mod n2 c)))
            p? (pos? n)
            xs1 (if p?
                  (take x xs)
                  (take-last x xs))
            xs2 (if p?
                  (drop x xs)
                  (drop-last x xs))]
            (if p?
              (concat xs2 xs1)
              (concat xs1 xs2)))))

(= (P44 2 [1 2 3 4 5]) '(3 4 5 1 2))
(= (P44 -2 [1 2 3 4 5]) '(4 5 1 2 3))
(= (P44 6 [1 2 3 4 5]) '(2 3 4 5 1))
(= (P44 1 '(:a :b :c)) '(:b :c :a))
(= (P44 -4 '(:a :b :c)) '(:c :a :b))


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

