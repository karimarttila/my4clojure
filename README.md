# My 4Clojure Exercises  <!-- omit in toc -->

- [Babashka as a Task Runner](#babashka-as-a-task-runner)
- [Clojure REPL](#clojure-repl)
- [Clojurescript REPL](#clojurescript-repl)
- [Check the REPL in VSCode](#check-the-repl-in-vscode)
- [REPL Connect Sequences](#repl-connect-sequences)


My solutions to [4ever-clojure](https://4clojure.oxal.org/) exercises. Also scratch for experimentations and copy-pasted best other developers' solutions and examined them using REPL. 

I have been doing these exercises for learning purposes. I'm no Clojure guru but I'm a productive Clojure full stack developer. While doing these exercises I realized that after my own solution it is really important to study other developers' solutions. While studying other developers' solutions I often noticed that there is a one-liner solution which just applies one or two Clojure standard library functions. So, studying other developers' solutions provides two great learning purposes:

1. You get to understand how other developers think while constructing their own solutions.
2. You get to have good examples how to apply Clojure standard library functions - applying one or more functions with map, filter and reduce often provides a very concise and beautiful solution.

I have added best other developers' solutions after my own solution after comment "; Muiden" ("muiden" is Finnish and means "others", i.e. other developers' solutions). 

There are quite a lot of Finnish comments here and there - I didn't originally mean to make this a public repo - the solutions are originally written just for myself for learning purposes.

I have often studied in more detail other developers' solutions using the [hashp](https://github.com/weavejester/hashp) library. I strongly recommend hashp tool while learning Clojure.

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

Though the [4ever-clojure](https://4clojure.oxal.org/) site uses Clojurescript, I used Clojure (JVM) repl while developing the solutions for the exercises. The tasks are rather general in nature and in most cases the Clojure solution works in the Clojurescript side as well. For those exercises that I needed Clojurescript there is a [nodesolutions1.cljs](src/cljs/nodesolutions1.cljs) namespace.

The [4ever-clojure](https://4clojure.oxal.org/) site uses four categories and I created four namespaces, one for each category solutions:

- [elementary-solutions.clj](src/clj/elementary_solutions.clj) 
- [easy-solutions.clj](src/clj/easy_solutions.clj)
- [medium-solutions.clj](src/clj/medium_solutions.clj)
- [hard-solutions.clj](src/clj/hard_solutions.clj)

There is blog post regarding this exercise: [4Clojure Exercises Part 1](https://www.karimarttila.fi/clojure/2022/03/29/4clojure-exercises-part-1.html)

## Babashka as a Task Runner

I have recently switched from [Just](https://github.com/casey/just) to [babashka](https://github.com/babashka/babashka) as a task runner. 

See available babashka tasks:

```bash
bb tasks
```


## Clojure REPL

See also chapter [REPL Connect Sequences](#repl-connect-sequences) below => there you can find the actual instructions how to do the REPL connection with Calva.

Start backend repl with Calva dependencies: `bb backend-calva-repl`.
In VSCode: command: `Connect to a running REPL server in the project`. Choose `deps.edn` and then accept the port (you can check that the port is the same as in file `.nrepl-port`). You are good to go. In Clojure files remember to `alt-n` - change namespace to use that of the file.

## Clojurescript REPL

See also chapter [REPL Connect Sequences](#repl-connect-sequences) below => there you can find the actual instructions how to do the REPL connection with Calva.

First run: `bb shadow-node` ... and wait till you see `Build completed`.
Then in another terminal run: `bb run-node`.
Then in VSCode: command: `Connect to a running REPL server in the project`, then choose: `Clojurescript nRepl server`, then check the port number in file `.shadow-cljs/nrepl.port` and give that port number in VSCode.
Then in the `cljs` file: evaluate: `(shadow.cljs.devtools.api/repl :app)`.
You are good to go.

## Check the REPL in VSCode

Calva automatically uses the right REPL based on whether the file is `clj` (Clojure) or `cljs` (Clojurescript). You can see at the bottom of the VSCode window the status bar, which says `REPL ⚡ clj` (if your focus is in a `clj` file) or `REPL ⚡ cljs` (if your focus is in a `cljs` file).

## REPL Connect Sequences

NOTE: You can have only one REPL connection in one VSCode window (read more in [Configuring VSCode/Calva for Clojure programming - Part 3](https://www.karimarttila.fi/clojure/2022/10/18/clojure-calva-part3.html).)

I created the following [Calva REPL Connect Sequences](https://calva.io/connect-sequences/), in VSCode `settings.json` file:

```json
    "calva.replConnectSequences": [
      {
        "name": "backend + frontend",
        "projectType": "shadow-cljs",
        "cljsType": "shadow-cljs",
        "menuSelections": {
          "cljsLaunchBuilds": [
            ":app"
          ],
          "cljsDefaultBuild": ":app"
        }
      },
      {
        "name": "clojure-backend",
        "nReplPortFile": [".nrepl-port"],
        "projectType": "deps.edn",
        "cljsType": "none"
      },   
      {
        "name": "clojurescript-frontend",
        "projectType": "shadow-cljs",
        "cljsType": {
            "dependsOn": "shadow-cljs",
            "connectCode": "(shadow.cljs.devtools.api/repl :app)",
        }
      }
    ],    
```

So, first, start `shadow-cljs` process and `node process` (in two different terminals):

- `bb shadow-node` (wait until you see: `[watch:cljs] [:app] Build completed.`)
- `bb run-node` (you should see something like: `shadow-cljs - #3 ready!` )

Then use VSCode command `Calva: Connect to a Running REPL Server in the Project` => Choose `backend + frontend`, next for the suggested `host:port` press `Enter`.

You should see something like:
```text
; Connecting ...
; Hooking up nREPL sessions...
; Connected session: clj
...
clj꞉shadow.user꞉> 
; Creating cljs repl session...
; Connecting cljs repl: backend + frontend...
;   The Calva Connection Log might have more connection progress information.
; Connected session: cljs, repl: :app
```

You now should have both Clojure (JVM) and Clojurescript (Node) REPLs available (through shadow-cljs).

Test:

- Open file [misc1.clj](scratch/misc1.clj): evaluate and you should see `REPL clj` at the bottom of the VSCode info bar.
- Open file [nodesolutions1.cljs](src/cljs/nodesolutions1.cljs) evaluate and you should see `REPL cljs` at the bottom of the VSCode info bar.
