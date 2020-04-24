(ns arrudeia.core-test
  (:require
   [arrudeia.core :as ar]
   [arrudeia.example.transaction :as transaction]
   [arrudeia.example.transaction-nested :as nested]
   [clojure.test :refer [deftest testing is use-fixtures]]
   [clojure.math.combinatorics :as combo]))

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

(deftest test-simple-concurrent-transaction
  (reset! transaction/balances {:c1 10M :c2 10M})
  (let [t1 (ar/register :t1 (transaction/request {:money "1"
                                                  :sender :c1
                                                  :receiver :c2}))]
    (ar/run-processes! [[t1 0]
                        [t1 1]
                        [t1 ::transaction/give-money!]]))
  (is (= {:c1 9M :c2 11M}
         @transaction/balances)))

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

(deftest test-args-modification
  ;; we modify t1's :give-money! result output
  (reset! transaction/balances {:c1 500M :c2 300M})
  (let [t1 (ar/register :t1 (transaction/request {:money "50"
                                                  :sender :c1
                                                  :receiver :c2})
                        {:result-modifiers
                         {::transaction/give-money!
                          (fn [args]
                            (update args :receiver-new-amount + 132M))}})
        t2 (ar/register :t2 (transaction/request {:money "225"
                                                  :sender :c2
                                                  :receiver :c1}))]
    (ar/run-processes! [[t1 ::transaction/add-new-amounts]
                        [t2 ::transaction/add-new-amounts]
                        [t2 ::transaction/receive-money!]
                        [t1 ::transaction/receive-money!]]))
  (is (= {:c1 450M :c2 482M} @transaction/balances)))

(deftest test-transaction-interleavings
  (testing "no valid balances invariant"
    (is (= false
           (->> (for [interleaving (ar/valid-interleavings
                                    [[:t1 ::transaction/add-new-amounts]
                                     [:t1 ::transaction/give-money!]
                                     [:t1 ::transaction/receive-money!]]
                                    [[:t2 ::transaction/add-new-amounts]
                                     [:t2 ::transaction/give-money!]
                                     [:t2 ::transaction/receive-money!]])]
                  (do (reset! transaction/balances {:c1 500M :c2 300M})
                      (let [t1 (ar/register :t1 (transaction/request {:money "50"
                                                                      :sender :c1
                                                                      :receiver :c2}))
                            t2 (ar/register :t2 (transaction/request {:money "225"
                                                                      :sender :c2
                                                                      :receiver :c1}))]
                        (ar/run-processes! (ar/parse-process-names
                                            {:t1 t1 :t2 t2} interleaving)))
                      (reduce + 0.0M (vals @transaction/balances))))
                (every? #(= % 800.M))))))

  (testing "bypass give-money! operation (it's considered, for our purposes
as a atomic operation together with receive-money); balances invariant is
respected but final values of clients are wrong"
    (doseq [interleaving (ar/valid-interleavings
                          [[:t1 ::transaction/add-new-amounts]
                           [:t1 ::transaction/receive-money!]]
                          [[:t2 ::transaction/add-new-amounts]
                           [:t2 ::transaction/receive-money!]])]
      (reset! transaction/balances {:c1 500M :c2 300M})
      (let [t1 (ar/register :t1 (transaction/request {:money "50"
                                                      :sender :c1
                                                      :receiver :c2}))
            t2 (ar/register :t2 (transaction/request {:money "225"
                                                      :sender :c2
                                                      :receiver :c1}))]
        (ar/run-processes! (ar/parse-process-names
                            {:t1 t1 :t2 t2} interleaving)))
      (is (reduce + 0.0M (vals @transaction/balances)))))
  (testing "each process now is considered atomic (add-new-amounts operation
was removed); balances invariant is respected and final values of clients are
correct"
    (doseq [interleaving (ar/valid-interleavings
                          [[:t1 ::transaction/receive-money!]]
                          [[:t2 ::transaction/receive-money!]])]
      (reset! transaction/balances {:c1 500M :c2 300M})
      (let [t1 (ar/register :t1 (transaction/request {:money "50"
                                                      :sender :c1
                                                      :receiver :c2}))
            t2 (ar/register :t2 (transaction/request {:money "225"
                                                      :sender :c2
                                                      :receiver :c1}))]
        (ar/run-processes! (ar/parse-process-names
                            {:t1 t1 :t2 t2} interleaving)))
      (is (= {:c1 675M, :c2 125M} @transaction/balances)))))

(deftest test-run-step
  (reset! transaction/balances {:c1 500M :c2 300M})
  (let [t1 (ar/register :t1 (transaction/request
                             {:money "50"
                              :sender :c1
                              :receiver :c2}))]
    (ar/run-step [t1 ::transaction/give-money!])
    (is (= {:c1 450M :c2 300M} @transaction/balances))
    (is (= {:c1 450M :c2 350M} @t1))))

(deftest test-nested
  (reset! nested/balances {:c1 500M :c2 300M})
  (let [t1 (ar/register :t1 (nested/request {:money "50"
                                             :sender :c1
                                             :receiver :c2}))]
    (ar/run-step [t1 :add])             ;; `:add` is a custom name
    (ar/run-step [t1 :custom/receive-money])
    (Thread/sleep 100)
    (is (realized? (:proc t1)))
    (ar/cancel-remaining-steps [t1]))
  (is (= {:c1 450M :c2 350M} @nested/balances)))
