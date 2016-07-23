; > *This is a recipe that I wrote for the [*Clojure Data
; Analysis
; Cookbook*](/pages/announcements/clj-data-analysis/index.html).
; However, it didn't make it into the final book, so I'm
; sharing it with you today.*
;
; When working with data, it’s often useful to have a
; computer language or DSL that allows us to express how
; data flows through our program. The computer can then
; decide how best to execute that flow, whether it should
; be spread across multiple cores or even multiple
; machines.
;
; <!--more-->
;
; This style of programming is called [**dataflow
; programming**](http://en.wikipedia.org/wiki/Dataflow_programming).
; There are a couple of different ways of looking at
; dataflow programming. One way describes it as being like
; a spreadsheet. We declare relationships between cells,
; and a change in one cell percolates through the graph.
;
; Another way of looking at it is as a graph that captures
; the computation. Each computation is a node, and the
; data flows between them. After we build the computation
; graph, we tell the computer to run it how it seems best.
; It could distribute the computations across cores or
; even across computers in a network.
;
; And guess what? Clojure itself allows that kind of
; expression, especially with the threading macros (`->`
; and `->>`). Reducers handle parallelization under the
; covers. Let’s see how far we can take that.
;
; ### Getting ready…
;
; To use reducers, we first need to depend on Clojure 1.5.
; We’ll need this in our
; [Leiningen](http://leiningen.org/)
; [`project.clj`](project.clj) file:
;

; ```clojure
; :dependencies [[org.clojure/clojure "1.5.1"]]
; ```

;
; We also need to import the library, and since it defines
; several functions with the same name as core functions,
; we’ll alias it.

(require '[clojure.core.reducers :as r])

; For this example, we’ll work with a list of the Doctor’s
; companions from the entire run of Doctor Who. I won’t
; reproduce the whole list, but here is a sample of six:

(def input-data
  [{:given-name "Susan", :surname "Forman",
    :doctors [1]}
   {:given-name "Harry", :surname "Sullivan",
    :doctors [4]}
   {:given-name "Nyssa", :surname nil,
    :doctors [4 5]}
   {:given-name "Melanie", :surname "Bush",
    :doctors [6 7]}
   {:given-name "Jackson", :surname "Lake",
    :doctors [10]}
   {:given-name "Craig", :surname "Owens",
    :doctors [11]}])

; (If you want to use the entire dataset, you can download
; it from the book's [data page](/clj-data-analysis/) or
; directly from [this link](/clj-data-analysis/data/companions.clj).)
;
; ### How to do it…
;
; For our slightly contrived example, we’ll compute the
; average length of the companions’ surnames, for those
; who have surnames. First, we’ll need a couple of
; functions to accumulate the item count and sum for
; computing the mean. We’ll also need a function to add
; two accumulators’ data together. And we’ll need a
; function to calculate the mean from an accumulator's
; data. Here are those functions.

(defn accum-mean
  "Accumulate the sum and length of a sequence for
  calculating the mean."
  ([] {:sum 0, :n 0})
  ([{:keys [sum n]} x]
   {:sum (+ sum x)
    :n (inc n)}))

(defn join-accum
  "Take the output of two calls to accum-mean and join
  them."
  ([] {:sum 0, :n 0})
  ([accum1 accum2]
   {:sum (+ (:sum accum1) (:sum accum2))
    :n (+ (:n accum1) (:n accum2))}))

(defn calc-mean
  "Take the output of accum-mean or join-accum and
  calculate the mean."
  [{:keys [sum n]}]
  (double (/ sum n)))

; With these in place, we can define a function that
; creates a reducer that returns the length of the
; surnames, filtering out those with no surname. Combined
; with a threading macro, this makes a very clear
; dataflow.

(defn process-seq
  [coll]
  (->>
    coll
    (r/map :surname)
    (r/filter #(not (nil? %)))
    (r/map count)))

; First we can run that with the core reduce function to
; execute it sequentially.
;

; ```clojure
; user=> (calc-mean (reduce accum-mean (accum-mean) 
;                           (process-seq input-data)))
; 5.4
; ```

;
; But by changing from reduce to
; [`clojure.core.reducers/fold`](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core.reducers/fold),
; it will automatically partition our data and spread the
; processing across multiple cores, even given the same
; input process.
;

; ```clojure
; user=> (calc-mean (r/fold join-accum accum-mean 
;                           (process-seq input-data)))
; 5.4
; ```

;
; ### How it works…
;
; Because it has a more complicated execution model, the
; `r/fold` function takes a little more information.
;
; 1. The `accum-mean` function takes the results of the
; process and turns them into an accumulator map.
;
; 2. But if `r/fold` decides to use more than one
; partition, those maps will need to be combined. That’s
; where `join-accum` comes into play.
;
; Both the reducer and combiner functions can be called
; with no parameters. In that case, both return a zero
; accumulator map. We use that with the `reduce` call, and
; `r/fold` will use it in its processing to get the
; starting state for the reduce and combine steps.
;
; In either case, reducers allow us to create a data
; structure for processing the data and decide later—or
; let the computer decide—how best to execute the process.
; Meanwhile, our code remains clear and readable, and what
; is happening with the data is obvious.

; <!-- vim: set textwidth=58: -->
