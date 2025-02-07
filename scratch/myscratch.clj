(ns myscratch
  {:clj-kondo/config '{:linters {:redefined-var {:level :off}}}}
  (:require [clojure.test :refer [deftest use-fixtures is testing]]
            [hashp.core]
            [clojure.string :as s]
            [clojure.repl :as repl]
            [clojure.set :as se]
            [clojure.walk :as walk]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A bigger scratch area for experimentation.

; Some useful keybindings
; ** Calva specific: **
; See also: https://www.karimarttila.fi/clojure/2025/02/02/clojure-keybindings.html
; See also: https://www.karimarttila.fi/keyboard/2020/09/28/dygma-raise-reflections-part-1.html
; https://www.karimarttila.fi/clojure/2022/10/08/clojure-calva.html
; https://www.karimarttila.fi/clojure/2022/10/16/clojure-calva-part2.html 
; https://www.karimarttila.fi/clojure/2022/10/18/clojure-calva-part3.html
; ctrl+shift+D => show doc for symbol in a tab.
; ctrl+x k => kill tab.
; ctrl+shift+i => navigate to symbol source.
; ctrl+shift+j => naviage back.
; alt+l => evaluate form
; alt+shift+l => evaluate S-expr and add result as comment below.
; ctrl+shift+k => kill S-expr.
; alt+shift+k => copy S-expr.
; ctrl+k => just ordinary Emacs kill till the end of row.
; ** Copilot specific: ** 
; https://www.karimarttila.fi/programming/2025/02/01/copilot-keybindings.html
; ctrl+shift+n => Ask Copilot for a suggestion.
; ctrl+shift+m => Accept Copilot suggestion.
; ctrl+shift+, => Accept Copilot suggestion word by word.
; ctrl+shift+. => Show several Copilot suggestions in a separate panel.

; TODO: Write a blog post regarding: Insight - Learn to Know the Clojure Standard Library
; Ks. personal/Blog-idea.txt


(comment
  ; General area.

  (repl/source seq))


; Easy solutions.

(comment

  ; For P96, see P95 solution!
  
  (= '(:b nil nil) '(:b nil nil))
  ;;=> true
  (= [:b nil nil] '(:b nil nil))
  ;;=> true
  

  (defn third [l] (first (drop 2 l)))
  (first [1 2 3])
  (first '(1 2 3))
  (second [1 2 3])
  (second '(1 2 3))
  (third [1 2 3])
  (third '(1 2 3))



  ; Let's use our P95 solution as a base solution, and elaborate it.
  (def P96 (fn [t]
             (letfn [(third [li] (first (drop 2 li)))
                     (same? [l r]
                       (let [_ #p l
                             _ #p r
                             av (first l)
                             al (second l)
                             ar (third l)
                             bv (first r)
                             bl (second r)
                             br (second r)
                             _ #p av
                             _ #p al
                             _ #p ar
                             _ #p bv
                             _ #p bl
                             _ #p br]
                         (and (= av bv)
                              (or (and (= al bl) (= ar br))
                                  (and (= al br) (= ar bl))))))
                     (leaf? [item] (not (coll? item)))
                     (tree? [cand]
                       (if (leaf? cand)
                         (nil? cand)
                         (if (= (count cand) 3)
                           (let [[_ l r] cand]
                             (and (tree? l)
                                  (tree? r)
                                  (same? l r)))
                           false)))]
               (tree? t))))

  ; Symmetric.
  (P96 '(:a (:b nil nil) (:b nil nil)))
  ; Not symmetric.
  (P96 '(:a (:b nil nil) nil))

  (= (P96 '(:a (:b nil nil) (:b nil nil))) true)
  (= (P96 '(:a (:b nil nil) nil)) false)

  ; All tests passed, except this one:
  (= (P96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
           [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
     true)

  (P96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
        [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
  ;;=> false
  ; Let's examine it a bit further.
  (P96 [1
        [2
         nil
         [3
          [4
           [5 nil nil]
           [6 nil nil]]
          nil]]
        [2
         [3
          nil
          [4
           [6 nil nil]
           [5 nil nil]]]
         nil]])
  ; So just the ordering is different.
  
  (= [3 [4 [5 nil nil] [6 nil nil]] nil]
     [3 nil [4 [6 nil nil] [5 nil nil]]])
  ;;=> false
  
  ; Let's examine also: https://clojuredocs.org/clojure.walk
  (require '[clojure.walk :as walk])
  (def tree [1 [2] [3 [4]]])
  (walk/prewalk-demo tree)
  (walk/prewalk-demo '(:a (:b nil nil) (:b nil nil)))

  ((into #{} (keys (ns-publics *ns*))) 'tree)
  (ns-resolve *ns* 'tree)
  (ns-unmap *ns* 'tree)

  ; I draw this binary tree. And realized: If you swap every branch of the other main level branch,
  ; then the main level branches should be identical, if it is a symmetrical tree.
  ; So, how to mirror this subtree? 
  [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
  (defn simple-mirror [[v l r]]
    [v r l])
  ; Ok. Now we can mirror the main level:
  (simple-mirror [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]])
  ;;=> [2 [3 [4 [5 nil nil] [6 nil nil]] nil] nil]
  
  (defn mirror-it [tree]
    (letfn [(leaf? [item] (not (coll? item)))
            (mirror [t]
              (if (leaf? t)
                t
                (let [[v l r] t]
                  [v (mirror r) (mirror l)])))]
      (mirror tree)))


  (mirror-it [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]])
  ;;=> [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]
  ; And back:
  (mirror-it [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil])
  ;;=> [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
  (def test-tree [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]])
  (= test-tree test-tree)
  ;;=> true
  (= test-tree (mirror-it test-tree))
  ;;=> false
  (= (mirror-it test-tree) (mirror-it test-tree))
  ;;=> true
  
  ; Ok. Now we have to check:
  ; 1. It is a valid tree.
  ; 2. It is symmetric.
  
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
  
  (= (P96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
           [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
     true)
  ;;=> true
  
  ; My previous solution.
  (def P96 (fn [x] (letfn [(tr1 [x] (if (nil? x) [x] (concat [(first x)] (tr1 (second x)) (tr1 (nth x 2)))))
                           (tr2 [x] (if (nil? x) [x] (concat [(first x)] (tr2 (nth x 2)) (tr2 (second x)))))]
                     (= (tr1 x) (tr2 x)))))

  (defn tr1 [x] (if (nil? x) [x] (concat [(first x)] (tr1 (second x)) (tr1 (nth x 2)))))
  (defn tr2 [x] (if (nil? x) [x] (concat [(first x)] (tr2 (nth x 2)) (tr2 (second x)))))
  (tr1 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
        [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
  ;;=> (1 2 nil 3 4 5 nil nil 6 nil nil nil 2 3 nil 4 6 nil nil 5 nil nil nil)
  (tr2 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
        [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
  ;;=> (1 2 nil 3 4 5 nil nil 6 nil nil nil 2 3 nil 4 6 nil nil 5 nil nil nil)
  
  )




(comment

  (def P95 (fn [t]
             (letfn [(node? [item] (not (coll? item)))
                     (tree? [cand]
                       (if (= (count cand) 3)
                         (let [[l m r] cand
                               _ #p l
                               _ #p m
                               _ #p r]
                           (and (or (node? l) (tree? l))
                                (or (node? m) (tree? m))
                                (or (node? r) (tree? r))))
                         false))]
               (tree? t))))

  ; Power of hashp.
  (P95 '(:a (:b nil nil) nil))
  ; #p[myscratch/P95:54] l => :a
  ; #p[myscratch/P95:54] m => (:b nil nil)
  ; #p[myscratch/P95:54] r => nil
  ; #p[myscratch/P95:54] l => :b
  ; #p[myscratch/P95:54] m => nil
  ; #p[myscratch/P95:54] r => nil
  true

  ; Without hashp.
  (def P95 (fn [t]
             (letfn [(node? [item] (not (coll? item)))
                     (tree? [cand]
                       (if (= (count cand) 3)
                         (let [[l m r] cand]
                           (and (or (node? l) (tree? l))
                                (or (node? m) (tree? m))
                                (or (node? r) (tree? r))))
                         false))]
               (tree? t))))

  (= (P95 '(:a (:b nil nil) nil)) true)

  ; Hm. All tests passed, except this one:

  (= (P95 [1 [2 [3 [4 false nil] nil] nil] nil]) false)

  ; Let's debug.
  ; Hm. I get it now. The spec is: [value, left, right]. 
  ; left and right is either a tree or a value.
  ; Let's try again.

  ; Ok. New trial as spec: [value, left, right]
  ; If leaf, then it has to be nil, or it is false.
  (def P95 (fn [t]
             (letfn [(leaf? [item] (not (coll? item)))
                     (tree? [cand]
                       (let [_ #p cand]
                         (if (leaf? cand)
                           (or (nil? cand) false)
                           (if (= (count cand) 3)
                             (let [[v l r] cand
                                   _ #p v
                                   _ #p l
                                   _ #p r]
                               (and (tree? l)
                                    (tree? r)))
                             false))))]
               (tree? t))))

  (P95 [1 [2 [3 [4 false nil] nil] nil] nil])
  ;;=> false
  (= (P95 '(:a (:b nil nil) nil)) true)
  ;;=> true

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

  ; Beautifull. All tests passed this time.
  )


(comment

  (for [a #{1 2 3}
        b #{4 5}]
    [a b])

  (def P90 (fn [s1 s2]
             (set (for [a s1
                        b s2]
                    [a b]))))

  (P90 #{1 2 3} #{4 5})
  (= (P90 #{1 2 3} #{4 5}) #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))



(comment

  (require '[clojure.set :as se])

  (def both (se/intersection #{1 2 3 4 5 6} #{1 3 5 7}))
  both
  ;;=> #{1 3 5}
  (remove both #{1 2 3 4 5 6})
  ;;=> (4 6 2)
  (remove both #{1 3 5 7})
  ;;=> (7)
  (set (concat '(4 6 2) '(7)))
  ;;=> #{7 4 6 2}

  (def P88 (fn [s1 s2]
             (let [both (clojure.set/intersection s1 s2)]
               (set (concat (remove both s1) (remove both s2))))))

  (P88 #{1 2 3 4 5 6} #{1 3 5 7})

  (= (P88 #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7}))


(comment

  (identity true)
  ;;=> true
  (identity false)
  ;;=> false
  (every? identity [true false])
  ;;=> false
  (every? identity [true true])
  ;;=> true
  (every? identity [false false])
  ;;=> false
  (some identity [true false])
  ;;=> true
  (some identity [true true])
  ;;=> true
  (some identity [false false])
  ;;=> nil
  (some false? [true false])
  ;;=> true
  (some false? [false false])
  ;;=> true
  (some true? [true false])
  ;;=> true
  (some true? [false false])
  ;;=> nil

  (= false nil)
  ;;=> false
  (false? false)
  ;;=> true
  (false? nil)
  ;;=> false
  (and nil true)
  (true? nil)
  ;;=> nil

  (def P83 (fn [& args]
             (true? (and (some true? args) (some false? args)))))

  (P83 false false)
  ;;=> false
  (P83 false true)
  ;;=> true
  (P83 true true)
  ;;=> false

  (= false (P83 false false))

  ; NOTE: This exercise was a good example on how to solve problems using REPL in Clojure.
  )

(comment

  (require '[clojure.set :as se])

  (se/intersection #{0 1 2 3} #{2 3 4 5})
  ;;=> #{3 2}

  (def P81 (fn [xs ys]
             (->> ys
                  (map (fn [x]
                         (xs x)))
                  (remove nil?)
                  set)))

  (P81 #{0 1 2 3} #{2 3 4 5})
  ;;=> #{3 2}

  (= (P81 #{0 1 2 3} #{2 3 4 5}) #{2 3})

  ; Other developers' solutions. What the heck? Let's try to understand this.
  (def P81 (comp set filter))

  ((comp set filter) #{0 1 2 3} #{2 3 4 5})
  ((comp str +) 8 8 8)
  ;;=> "24"
  (str (apply + [8 8 8]))
  ;;=> "24"
  (set (apply filter [#{0 1 2 3} #{2 3 4 5}]))
  ;;=> #{3 2}
  ; So: filter-function uses the first set as function, and the second set as argument.
  ; This other develop's solution clarifies the idea.
  (def P81 #(set (filter % %2)))
  (P81 #{0 1 2 3} #{2 3 4 5}))

(comment

  ; Brute force. Just try each candidate in turn.
  ; In real life we should stop when finding the first match.
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

  (= (P66 2 4) 2))


(comment

  ({:a 1} :a)
  ;;=> 1
  ({:a 1} :b)
  ;;=> nil

  (group-by #(> % 5) #{1 3 6 8})
  ;;=> {false [1 3], true [6 8]}

  ; Let's show the iterative workflow using Clojure.
  ; First, lets map them and get the keys and values.  
  (def P63 (fn [f xs]
             (map (fn [x]
                    [(f x) x]) xs)))
  (P63 #(> % 5) #{1 3 6 8})
  ;;=> ([false 1] [true 6] [false 3] [true 8])
  ; Second iteration.
  (def P63 (fn [f xs]
             (let [items (map (fn [x] [(f x) x]) xs)]
               (reduce (fn [acc [k v]]
                         (let [myv (acc k)]
                           (if (nil? myv)
                             (assoc acc k [v])
                             (assoc acc k (conj myv v))))) {} items))))
  (P63 #(> % 5) #{1 3 6 8})
  ;;=> {false [1 3], true [6 8]}

  (= (P63 #(> % 5) #{1 3 6 8}) {false [1 3], true [6 8]})

  ; Other developers' solutions.
  ; This is something to tell in my blog: Know the standard library! 
  ; merge-with does exactly what we need here.
  ; If you know the standard library, you don't have to invent the wheel again.
  (def P63b (fn [f coll]
              (reduce (fn [acc x] (merge-with concat acc {(f x) [x]})) {} coll)))
  (P63b #(> % 5) #{1 3 6 8})

  ; I forgot update-in
  (def P63c (fn [f s]
              (reduce
               (fn [m n]
                 (update-in m [(f n)] concat [n]))
               {} s)))

  (P63c #(> % 5) #{1 3 6 8}))



(comment

  (take 5 (iterate #(* 2 %) 1))
  ;;=> (1 2 4 8 16)

  (defn positive-numbers
    ([] (positive-numbers 1))
    ([n] (lazy-seq (cons n (positive-numbers (inc n))))))

  (take 5 (positive-numbers))
  ;;=> (1 2 3 4 5)

  (defn iter [f n]
    (lazy-seq (cons n (iter f (f n)))))

  (take 5 (iter #(* 2 %) 1))
  ;;=> (1 2 4 8 16)
  ; Damn it. It was so easy using an example.

  (def P62 (fn [f n]
             (letfn [(iter [g m]
                       (lazy-seq (cons m (iter g (g m)))))]
               (iter f n))))

  (take 5 (P62 #(* 2 %) 1))
  ;;=> (1 2 4 8 16)

  ; Other developers' solutions
  ; Did not remember that you can give name for a function using the fn macro.
  ; And also, it would be better to have the lazy-seq for the part of adding x.
  (def P62b (fn func [f x]
              (cons
               x
               (lazy-seq
                (func f (f x))))))

  (take 5 (P62b #(* 2 %) 1))

  ; Let's elaborate my solution a bit.
  (def P62 (fn [f n]
             (letfn [(iter [g m]
                       (cons m (lazy-seq (iter g (g m)))))]
               (iter f n))))

  (take 5 (P62 #(* 2 %) 1))
  ;;=> (1 2 4 8 16)

  ; Let's iterate a bit more. We can get rid of g and use f closure.
  ; Someone told me this in Koodiklinikka slack.
  (def P62 (fn [f n]
             (letfn [(iter [m]
                       (cons m (lazy-seq (iter (f m)))))]
               (iter n))))

  (take 5 (P62 #(* 2 %) 1))
  ;;=> (1 2 4 8 16)


  ; Other developers' solutions, what the heck is this?
  (def P62c (fn [f x]
              (reductions #(%2 %1) x (repeat f))))

  (take 5 (P62c #(* 2 %) 1))
  ;;=> (1 2 4 8 16)

  ; Let's try to make it more understandable by eliminating the #() macro.
  (def P62d (fn [f x]
              (reductions (fn [n g] (g n)) x (repeat f))))

  (take 5 (P62d #(* 2 %) 1))
  ;;=> (1 2 4 8 16)

  ; Ok. I got it now. Reductions takes a function, and creates a lazy seq by applying it like reduce.
  ; See: https://clojuredocs.org/clojure.core/reductions
  (reductions + [1 2 3])
  ;;=> (1 3 6)
  )

(comment

  (zipmap [:a :b :c] [1 2 3])

  (map (fn [x y] [x y]) [:a :b :c] [1 2 3])
  ;;=> ([:a 1] [:b 2] [:c 3])
  (reduce (fn [acc [k v]]
            (assoc acc k v))
          {} '([:a 1] [:b 2] [:c 3]))
  ; And then combine them.
  (def P61 (fn [ks vs]
             (let [pairs (map (fn [x y] [x y]) ks vs)]
               (reduce (fn [acc [k v]]
                         (assoc acc k v))
                       {} pairs))))

  (P61 [:a :b :c] [1 2 3])

  (= (P61 [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))

(comment
  ; Trivial.
  (let [[a b & c :as d] [1 2 3 4 5]] [a b c d])
  (def P51 [1 2 3 4 5])
  (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] P51] [a b c d])))

(comment

  (split-at 3 [1 2 3 4 5 6])
  ; Should not have looked the split-at documentation. :-)
  (take 3 [1 2 3 4 5 6])
  (drop 3 [1 2 3 4 5 6])
  (def P49 (fn [n xs] [(take n xs) (drop n xs)]))
  ; Other developers solutions:
  ; Damn, I forgot juxt. Got to remember it now.
  (def P49 (juxt take drop))

  (P49 3 [1 2 3 4 5 6])

  (= (P49 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))

(comment
  ; Trivial
  (def P48 6)
  (= P48 (some #{2 7 6} [5 6 7 8])))

(comment
  ; Trivial
  (def P47 4)
  (contains? #{4 5 6} P47)
  (contains? [1 1 1 1 1] P47))

(comment
  ; Trivial.
  (def P45 '(1 4 7 10 13))
  (= P45 (take 5 (iterate #(+ 3 %) 1))))



(comment

  ; Not quite there. :-) 
  (take 5 (iterate (partial #(* (inc %) %)) 1))
  ;;=> (1 2 6 42 1806)

  (reduce (fn [acc n]
            (let [_ #p acc
                  _ #p n]
              (if (= n 1)
                acc
                (* acc n))))
          1 (range 1 (inc 5)))
  ;;=> 120

  ; Let's just implement it using a reduce, 
  ; even though I'm pretty sure someone used some elaborate one liner without reduce.
  (def P42 (fn [num]
             (reduce (fn [acc n]
                       (if (= n 1)
                         acc
                         (* acc n)))
                     1 (range 1 (inc num)))))
; My old solution, a lot shorter:
  (def P42 (fn [n] (->> n inc (range 1) (apply *))))

  (->> 5 inc (range 1) (apply *))

  (P42 5)
  (= (P42 5) 120))

(comment

  [:a :b :c]

  (partition 3 3 [:pad :pad :pad] [1 2 3 4 5 6 7 8 9 10])
  ;;=> ((1 2 3) (4 5 6) (7 8 9) (10 :pad :pad))
  (mapcat (fn [x]
            (take (- 3 1) x))
          '((1 2 3) (4 5 6) (7 8 9) (10 :pad :pad)))
  ;;=> (1 2 4 5 7 8 10 :pad)

  [1 2 4 5 7 8 10 :pad]
  (remove #(= :pad %) [1 2 4 5 7 8 10 :pad])


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

  (P41 [1 2 3 4 5 6 7 8 9 10] 3)

  (= (P41 [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])

  ; Other developers:
  (def P41 (fn [s c] (mapcat #(take (dec c) %) (partition-all c s)))))


(comment

  (interpose 0 [1 2 3])
  ;;=> (1 0 2 0 3) 

  (take 5 (iterate identity 0))

  (def P40 (fn [n xs] (drop 1 (mapcat (fn [x y] [x y]) (iterate identity n) xs))))

  (P40 0 [1 2 3])
  (= (P40 0 [1 2 3]) [1 0 2 0 3])

  ; Old
  ; Damn, I didn't remember interleave.
  ; Anyway, pretty similar as my new solution.
  (def P40 (fn [sep xs] (drop-last (interleave xs (iterate identity sep)))))
  (interleave [1 2 3] (iterate identity 0))
  ;;=> (1 0 2 0 3 0)
  )

(comment

  (flatten (map (fn [x y]
                  [x y])
                [1 2 3] [:a :b]))

  (def P39 (fn [xs xy]
             (flatten (map (fn [x y] [x y])
                           xs xy))))

  (P39 [1 2 3] [:a :b :c])

  (= (P39 [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))

  (apply assoc {}
         (interleave [:fruit :color :temp]
                     ["grape" "red" "hot"])))


(comment.

 (def P38 (fn [n & r]
            (last (sort (flatten (list n r))))))

 (def P38 (fn [n & r] (-> (list n r) flatten sort last)))

 (def P38 (fn [& r] (-> r flatten sort last)))

 (def P38 (fn [& r] (-> r sort last)))

 (P38 1 8 3 4)

 (= (P38 1 8 3 4) 8))

(comment

  ; Really easy.
  (def P34 (fn [b e]
             (take (- e b) (iterate inc b))))

  (P34 1 4)

  (= (P34 1 4) '(1 2 3)))

(comment

  (def mys [1 2 3])

  (take 2 (iterate identity 1))
  ;;=> (1 1)

  ; That was easy, now that know mapcat, iterate and identity.
  (def P33 (fn [xs n]
             (mapcat (fn [x]
                       (take n (iterate identity x)))
                     xs)))

  ; Let's see how to use repeat.
  (take 2 (repeat 1))
  ;;=> (1 1)

  (def P33 (fn [xs n]
             (mapcat (fn [x]
                       (take n (repeat x)))
                     xs)))
  ; => It is better.

  (P33 mys 2)

  (= (P33 [1 2 3] 2) '(1 1 2 2 3 3)))


(comment

  (def mys [1 2 3])
  (map (fn [x] [x x]) mys)
  ;;=> ([1 1] [2 2] [3 3])
  (mapcat (fn [x] [x x]) mys)
  ;;=> (1 1 2 2 3 3)

  (def P32 (fn [xs] (mapcat (fn [x] [x x]) xs)))



  (P32 mys)


  (= (P32 [1 2 3]) '(1 1 2 2 3 3)))


(comment

  (def P31 (fn [xs] (partition-by identity xs)))

  (def mys [1 1 2 1 1 1 3 3])
  (partition-by identity mys)

  (P31 [1 1 2 1 1 1 3 3])
  (= (P31 [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))

(comment

  (seq "Leee")
  ;;=> (\L \e \e \e)
  (str "" \L)
  ;;=> "L"


  (def my-acc {:acc "Le", :prev \e})
  (def my-mys \e)
  (= (str (:prev my-acc)) my-mys)

  (def P30 (fn [s]
             (:acc (reduce (fn [acc mys]
                             (let [_ #p acc
                                   _ #p mys]
                               (if (= (:prev acc) mys)
                                 {:acc (:acc acc) :prev mys}
                                 {:acc (str (:acc acc) mys) :prev mys}))) {:acc "" :prev ""} (seq s)))))
  ; Does not work with generic list items...
  (def P30 (fn [s]
             (:acc (reduce (fn [acc mys]
                             (if (= (:prev acc) mys)
                               {:acc (:acc acc) :prev mys}
                               {:acc (str (:acc acc) mys) :prev mys})) {:acc "" :prev ""} (seq s)))))

  (type '(1 1))
  ;;=> clojure.lang.PersistentList
  (type [1 1])
  ;;=> clojure.lang.PersistentVector
  (type "11")
  ;;=> java.lang.String
  (string? "11")
  ;;=> true
  (string? [1 1])
  ;;=> false


  (def P30 (fn [s]
             (let [check-type (fn [x]
                                (cond
                                  (string? x) :string
                                  (vector? x) :vector
                                  (list? x) :list
                                  :else :unknown))
                   my-type (check-type s)
                   _ #p my-type
                   result (:acc (reduce (fn [acc mys]
                                          (let [_ #p acc
                                                _ #p mys]
                                            (if (= (:prev acc) mys)
                                              {:acc (:acc acc) :prev mys}
                                              {:acc (conj (:acc acc) mys) :prev mys}))) {:acc [] :prev ""} (seq s)))
                   _ #p result]
               (cond
                 (= my-type :string) (reduce str result)
                 (= my-type :vector) (vec result)
                 (= my-type :list) (list result)
                 :else :error))))

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

  (reduce str [\L \e \r \o \y])
  (P30 "Leeeeeerrroyyy")
  (P30 [1 1 2 3 3 2 2 3])
  (P30 [[1 2] [1 2] [3 4] [1 2]])
  (= (P30 [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))

  (= (apply str (P30 "Leeeeeerrroyyy")) "Leroy")

  (= (P30 [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))

(comment

  (def my-caps #{\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z})
  (def my-caps (set (map char (range (int \A) (inc (int \Z))))))
  my-caps
  (my-caps \A)
  (my-caps \a)

  (def P29 (fn [s]
             (let [caps (set (map char (range (int \A) (inc (int \Z)))))
                   filtered (filter (fn [x]
                                      (caps x))
                                    s)]
               (apply str filtered))))
  (P29 "HeLlO, WoRlD!")

  (= (P29 "HeLlO, WoRlD!") "HLOWRD"))

(comment

  (flatten '((1 2) 3 [4 [5 6]]))

  (seq? '(1 2))
  (seq? 1)
  (seq? [4 [5 6]])
  (sequential? [4 [5 6]])
  (sequential? '(1 2))

  (def P28 (fn [xs]
             (reduce (fn [acc x]
                       (let [_ #p x
                             _ #p acc]
                         (if (sequential? x)
                           (conj acc (P28 x))
                           (conj acc x)))) [] xs)))

  (P28 '((1 2) 3 [4 [5 6]]))

  (def P28 (fn [xs]
             (reduce (fn [acc x]
                       (let [_ #p x
                             _ #p acc]
                         (if (sequential? x)
                           (concat acc (P28 x))
                           (conj acc x)))) [] xs)))
  ; Almost there, but not quite: the order is wrong.
  (P28 '((1 2) 3 [4 [5 6]]))

  (def P28 (fn [xs]
             (reduce (fn [acc x]
                       (let [_ #p x
                             _ #p acc]
                         (if (sequential? x)
                           (concat acc (P28 x))
                           (concat acc [x])))) [] xs)))

  (P28 '((1 2) 3 [4 [5 6]]))



  (= (P28 '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))

;;   (fn [input]
;;     (let [my-fun (fn [xs]
;;                    (reduce (fn [acc x]
;;                              (let [_ #p x
;;                                    _ #p acc]
;;                                (if (sequential? x)
;;                                  (concat acc (my-fun x))
;;                                  (concat acc [x])))) [] xs))]
;;       (my-fun input)))

  ; You have to use letfn !!!
  (def P28 (fn [input]
             (letfn [(my-fun [xs]
                       (reduce (fn [acc x]
                                 (if (sequential? x)
                                   (concat acc (my-fun x))
                                   (concat acc [x]))) [] xs))]
               (my-fun input))))

  (= (P28 '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))

(comment

  (s/join (reverse "racecar"))
  (def P27 (fn [sr] (= sr (s/join (reverse sr)))))
  (def P27 #(= % (s/join (reverse %))))
  (true? (P27 "racecar"))
  (true? (P27 [:foo :bar :foo]))

  (seq "racecar")
  (seq [:foo :bar :foo])
  (def P27 #(= (seq %) (reverse %))))

(comment

  ((fn [v num]
     (if (> num 2)
       (let [x (last v)
             _ #p x
             y (last (butlast v))
             z (+ x y)]
         (recur (conj v z) (dec num)))
       v)) [1 1] 5)

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

  (P26 1)
  (P26 2)
  (P26 3)
  (P26 10)
  (= (P26 3) '(1 1 2)))



(comment
  (def P25 (fn [s] (filter odd? s)))
  (def P25 #(filter odd? %))
  (= (P25 #{1 2 3 4 5}) '(1 3 5)))


(comment

  (def P24 (fn [s] (reduce + s)))
  (def P24 #(reduce + %))
  (= (P24 [1 2 3]) 6))

(comment

  (def P23 (fn [c] (reduce (fn [acc x] (cons x acc)) '() c)))
  (P23 [1 2 3 4 5])
  (= (P23 [1 2 3 4 5]) [5 4 3 2 1]))


(comment
  (def P22 (fn [l] (count l)))
  (def P22 (fn [l] (reduce (fn [acc _] (inc acc)) 0 l)))
  (P22 '(1 2 3 3 1))
  (= (P22 '(1 2 3 3 1)) 5))

(comment

  (def P21 (fn [lst n] (nth lst n)))
  (def P21 (fn [l n] (first (drop n l))))
  (def P21 #(first (drop %2 %1)))
  (P21 '(4 5 6 7) 2)
  (= (P21 '(4 5 6 7) 2) 6))

(comment

  (reverse '(1 2 3))
  (second '(1 2 3))
  (second (reverse '(1 2 3)))
  ((comp second reverse) '(1 2 3))
  (def P20 #((comp second reverse) %))
  (P20 (list 1 2 3 4 5 6))
  (def P20 #(-> % reverse second))
  (P20 (list 1 2 3 4 5 6))
  (= (P20 (list 1 2 3 4 5 6)) 5))





; 2025-01-26: All elementary solutions done (again).


(comment
  (if-not false 1 0))

(comment
  (clojure.set/superset? #{1 2} #{2}))


(comment
  (def P156 (fn [d s]
              (reduce (fn [acc i] (assoc acc i d)) {} s)))
  (= (P156 0 [:a :b :c]) {:a 0 :b 0 :c 0}))

(comment
  (get {:a 1} :b :not-found)
  (contains? {:a nil} :a)
  (:a {:a nil})

  (for [x (range 40)
        :when (= 1 (rem x 4))]
    x)
  (take 5 (iterate #(+ 4 %) 0))
  (take 5 (for [x (range 40)
                :when (= 1 (rem x 4))]
            x))

  (def P134 (fn [x m]
              (let [x  (get m x :not-found)]
                (if (= x :not-found)
                  false
                  (nil? x)))))


  (true?  (jee :a {:a nil :b 2}))


  (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (__))

  (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (reduce +))


  (->> 5 (+ 3) (/ 2) (- 1))
   ; 3/4
  (-> 5 (+ 3) (/ 2) (- 1))
   ; 3


  '(1 2 3 4 5)


  (-> [2 5 4 1 3 6] reverse rest sort last)

  (->> [2 5 4 1 3 6] reverse rest sort last)


  (take 10 (range))

  (def P37 "ABC")

  (= P37 (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))


  (= __ (map #(+ % 5) '(1 2 3)))

  (#(+ % 5) 1)



  (= (__ '(5 4 3)) 3)


  (drop 5)


  ((fn [l] (first (reverse l))) '(5 4 3)))




(comment
  (use 'clojure.pprint)

  (pprint (for [x (range 10)] (range x)))

  (+ 1 2)

  String

  (defn date? [d] (instance? java.util.Date d))

  (date? (java.util.Date.))

  (date? "asdf")

  (require '[hashp.core]))



(comment
  ((fn [xs n]
     (let [tuples (map (fn [z] [z n]) (range n))]
       (map (fn [[y x]]
              (let [_  y
                    _  x]
                (->> xs (drop y) (take-nth x)))) tuples)))
   [1 2 3 4 5 6] 2)

  ((fn [x1 y1 xs1]
     (->> xs1 (drop x1) (take-nth y1)))
   0 2 [1 2 3 4 5 6])

  (map (fn [z] [z 2]) (range 2))

  ((fn [z] [z 0]) 2))



(comment

  (def p23
    (fn [lst]
      (reduce (fn [acc item]
                (cons item acc))
              '() lst)))


  (def p23-old (fn [sq]
                 (loop [myseq sq newSq '()]
                   (if (empty? myseq)
                     newSq (recur (rest myseq) (conj newSq (first myseq))))))))



(comment
; #156 ****************************************
  (def p156 (fn [d v]
              (reduce (fn [m i] (assoc m i d)) {} v)))



  (deftest p156-test
    (testing "p156"
      (is (= (p156 0 [:a :b :c]) {:a 0 :b 0 :c 0}))
      (is (= (p156 "x" [1 2 3]) {1 "x" 2 "x" 3 "x"}))
      (is (= (p156 [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})))))

(comment
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
      (is (= (p22 '(:a :b :c)) 3)))))

(comment
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
      (is (= (p23 [[1 2] [3 4] [5 6]]) [[5 6] [3 4] [1 2]])))))

(comment
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
      (is (false? (p27 '(:a :b :c)))))))

(comment

; #26 ****************************************

;; Write a function which returns the first X fibonacci numbers.

  (def v [1 1 2 3 5])

  (def n (count v))

  (+ (get v (- n 1)) (get v (- n 2)))



  (def myfib (fn [v]
               (let [n (count v)]
                 (conj v (+ (get v (- n 1)) (get v (- n 2)))))))

  (myfib v))

(comment
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
      (is (= (p26 8) '(1 1 2 3 5 8 13 21))))))

(comment

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

  (seq? '())
   ; => true
  (seq? [])
   ; => false
  (seq? {})
   ; => false
  (seq? {:a 1})
   ; => false
  (seq? nil)
   ; => false
  (seq? 1)
   ; => false
  (coll? '())
   ; => true
  (coll? [])
   ; => true
  (coll? {})
   ; => true
  (coll? nil)
   ; => false
  (coll? 1)
   ; => false
  (rest '())
   ; => '()
  (next '())
   ; => nil
  (next '(1))
   ; => nil
  (next '(1 2))
   ; => '(2)
  (next '(1 2 3))
   ; => '(2 3)
;(next 1) ; => error
;(empty? 1) ; => error
;(not-empty 1) ; => error
  )

(comment
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
  *ns*)

(comment
  (def P100 (fn [& ns] (let [rats  (filter ratio? ns)
                             ints1  (filter (complement ratio?) ns)
                             ints2  (mapv numerator rats)
                             dens  (mapv denominator rats)]
                         (letfn [(min-idx [xs] (->> xs (map-indexed vector) (apply min-key second) first))
                                 (lcm [nums cands] (if (apply = cands)
                                                     cands
                                                     (let [i (min-idx cands)]
                                                       (lcm nums (assoc cands i (+ (nth cands i) (nth nums i)))))))]
                           (let [mul  (if (empty? dens) 1 (first (lcm dens dens)))
                                 nums  (mapv #(* mul %) (concat ints1 ints2))]
                             (-> (lcm nums nums) first (/ mul)))))))

  (def P100 (fn [& ns] (let [rats  (filter ratio? ns)
                             ints1  (filter (complement ratio?) ns)
                             ints2  (mapv numerator rats)
                             dens  (mapv denominator rats)]
                         (letfn [(min-idx [xs] (->> xs (map-indexed vector) (apply min-key second) first))
                                 (lcm [nums cands] (if (apply = cands)
                                                     cands
                                                     (let [i (min-idx cands)]
                                                       (lcm nums (assoc cands i (+ (nth cands i) (nth nums i)))))))]
                           (let [mul  (if (empty? dens) 1 (first (lcm dens dens)))
                                 nums  (mapv #(* mul %) (concat ints1 ints2))]
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

  (into #{} [1 2 2 3 1]))

(comment

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

  (reduce (inc-transducer +) 0 [1 2 3 4 5]))

(comment

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
              (let [_  x
                    _  f
                    _  y
                    _  r])
              ((if r
                 (fn [z] (let [_  z] (apply c z r)))
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
                                         (let [_  acc
                                               _  x
                                               _  (type x)
                                               _  (#{+ - / *} x)]
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
                         (let [_  acc
                               _  x])
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
                x)))))


(comment
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
  {:foo/foo 1, :foo/bar 2, :quux 3})
