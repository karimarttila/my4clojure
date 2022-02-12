# My 4Clojure Exercises  <!-- omit in toc -->

My solutions to [4ever-clojure](https://4clojure.oxal.org/) exercises. Also scratch for experimentations and copy-pasted best other developers' solutions and examined them using REPL. 

I have been doing these exercises for learning purposes. I'm no Clojure guru but I'm a productive Clojure full stack developer. While doing these exercises I realized that after my own solution it is really important to study other developers' solutions. While studying other developers' solution I often noticed that there is a one-liner solution which just applies one or two Clojure standard library functions. So, studying other developers' solutions provides two great learning purposes:

1. You get to understand how other developers think while constructing their own solutions.
2. You get to have good examples how to apply Clojure standard library functions - applying one or more functions with map, filter and reduce often provides a very concise and beautiful solution.

I have added best other developers' solutions after my own solution after comment "; Muiden" ("muiden" is Finnish and means "others", i.e. other developers' solutions). 

There are quite a lot of Finnish comments here and there - I didn't originally to make this a public repo - the solutions are originally meant to just for myself for learning purposes.

I have often studied in more detail other developers' solutions using the [hashp](https://github.com/weavejester/hashp) library. I strongly recommend it.

Example:

```clojure
(def P135cc (fn [& exprs]
              (reduce (fn [acc x]
                        (let [_ #p acc
                              _ #p x])
                        (if (fn? x)
                          (partial x acc)
                          (acc x)))
                      exprs)))
=> #'easy-solutions/P135cc
(P135cc 10 / 2 - 1 * 2)
#p[easy-solutions/P135cc:3] acc => 10
#p[easy-solutions/P135cc:3] x => #<Fn@7f6441b clojure.core/_SLASH_>
#p[easy-solutions/P135cc:3] acc => #<Fn@41d4c2c2 clojure.core/partial[fn]>
#p[easy-solutions/P135cc:3] x => 2
#p[easy-solutions/P135cc:3] acc => 5
#p[easy-solutions/P135cc:3] x => #<Fn@270025c clojure.core/_>
#p[easy-solutions/P135cc:3] acc => #<Fn@7d9367c3 clojure.core/partial[fn]>
#p[easy-solutions/P135cc:3] x => 1
#p[easy-solutions/P135cc:3] acc => 4
#p[easy-solutions/P135cc:3] x => #<Fn@4cb86345 clojure.core/_STAR_>
#p[easy-solutions/P135cc:3] acc => #<Fn@43fcb03e clojure.core/partial[fn]>
#p[easy-solutions/P135cc:3] x => 2
=> 8
```
