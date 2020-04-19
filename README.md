# Arrudeia

> "Arrudeia que a troça vai passar"
> - from everyone in Recife, Northeast region of Brazil

_It means to turn arround._

A small and simple library to control scheduling in Clojure for testing,
only testing purposes.

Did I say that this should be used only for testing?

## Installation

Add the following dependency to your `project.clj` file

[![Clojars Project](http://clojars.org/pfeodrippe/arrudeia/latest-version.svg)](http://clojars.org/pfeodrippe/arrudeia)

## Status

Please, this library should not be used in production, but as its use
should be for testing, it's probably okay to play with it, hope you find
some use for it.

## Getting Started

Imagine you have some contrived "server" code like below which can
(in theory) handle multiple financial transactions

``` clojure
;; implementation file
(ns arrudeia.example.transaction)

(def balances (atom {}))

(defn- adapt
  [m]
  (update m :money bigdec))

(defn- add-new-amounts
  [{:keys [:money :receiver :sender] :as args}]
  (assoc args
         :receiver-new-amount (+ (receiver @balances) money)
         :sender-new-amount (- (sender @balances) money)))

(defn- give-money!
  [{:keys [:sender :sender-new-amount] :as args}]
  (swap! balances assoc sender sender-new-amount)
  args)

(defn- receive-money!
  [{:keys [:receiver :receiver-new-amount] :as args}]
  (swap! balances assoc receiver receiver-new-amount)
  args)

(defn request
  [data]
  (-> (adapt data)
      add-new-amounts
      give-money!
      receive-money!)
  @balances)

;; test file
(ns arrudeia.transaction-test
  (:require
   [arrudeia.example.transaction :as transaction]
   [clojure.test :refer [deftest testing is]]))

(deftest test-transaction
  (reset! transaction/balances {:c1 500M :c2 300M})
  (testing "c1 transfers money to c2"
    (is (= {:c1 450M :c2 350M}
           (transaction/request {:money "50"
                                 :sender :c1
                                 :receiver :c2}))))
  (testing "c2 transfers money to c1"
    (is (= {:c1 675M :c2 125M}
           (transaction/request {:money "225"
                                 :sender :c2
                                 :receiver :c1})))))
```
You can check check the tests [here](./test/arrudeia/core_test.clj)

Hunnn, we have a feeling that this code is not thread safe. We can
test mentally and find a race condition and we want to test our guess,
we try to create some concurrent tests for it using Clojure, but how?

Let's assume we have 2 threads, we want to test each step in `request`
independently (run one step for one thread, then for another, then follow
with more 2 steps for one thread etc).

In *Arrudeia*, we have the macro `arrudeia.core/->*` which can replace
thread-first macros.

We require it with

``` clojure
[arrudeia.core :refer [->*]]
```

and then we replace the thread macro at request function

``` clojure
;; you can change the thread-first directly
(defn request
  [data]
  (->* (adapt data)    ;; <<<<< Changed here
       add-new-amounts
       give-money!
       receive-money!)
  @balances)

;; or use the tagged literal (they return the same form)
#ar/->*       ;; <<<<< Added here
(defn request
  [data]
  (-> (adapt data)
      add-new-amounts
      give-money!
      receive-money!)
  @balances)
```

The tests will look like

``` clojure
;; also require [arrudeia.core :as ar]
(defn disable-bypass-fixture
  [f]
  (ar/without-bypass
   (f)))

;; bypass is `true` by default so we can tell where we want
;; to use arrudeia
(use-fixtures :each disable-bypass-fixture)

(deftest test-concurrent-transaction
  (reset! transaction/balances {:c1 500M :c2 300M})
  (let [t1 (ar/register :t1 (transaction/request {:money "50"
                                                  :sender :c1
                                                  :receiver :c2}))
        t2 (ar/register :t2 (transaction/request {:money "225"
                                                  :sender :c2
                                                  :receiver :c1}))]
    (ar/run-processes! [[t1 ::transaction/add-new-amounts]
                        [t2 ::transaction/add-new-amounts]
                        [t1 ::transaction/give-money!]
                        [t2 ::transaction/give-money!]
                        [t1 ::transaction/receive-money!]
                        [t2 ::transaction/receive-money!]]))
  (is (= 1075M (reduce + 0.0M (vals @transaction/balances)))))
```

Look at this, money has been created out of thin air!
This is not good, but I leave the solution as an exercise to the reader =P

## Usage

Besides `->*`, also check `arrudeia.core/label`, it can be used in any
place where you want to create a step to be tested.

`arrudeia.core/valid-interleavings` returns all possible interleavings
for multiple processes so you can test some invariants (check its doc).
``` clojure
;; example
(valid-interleavings [[:t1 :step1]
                      [:t1 :step2]
                      [:t1 :step3]]
                     [[:t2 :other-step-1]
                      [:t2 :other-step-2]])
  =>
  [...           ;; other interleavings
   [[:t1 :step1]
    [:t2 :other-step-1]
    [:t1 :step2]
    [:t1 :step3]
    [:t2 :other-step-2]]
   ...]          ;; other interleavings

```

`arrudeia.core/parse-process-names` can be used together with
the output of `valid-interleavings`, maps keywords to concrete processes.

## Problems
### REPL can freeze

You may have to restart the REPL sometimes because there is some
bug in this library or you just put called some step which does not
exist.

## Copyright & License

The MIT License (MIT)

Copyright © 2020 Paulo Feodrippe.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
