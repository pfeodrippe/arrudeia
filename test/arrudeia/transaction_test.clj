(ns arrudeia.transaction-test
  (:require
   [arrudeia.core :as ar]
   [arrudeia.example.transaction :as transaction]
   [arrudeia.example.transaction-nested :as nested]
   [clojure.test :refer [deftest testing is]]))

(deftest test-transaction
  (ar/with-bypass
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
                                   :receiver :c1}))))))

(deftest test-simple-concurrent-transaction
  (reset! transaction/balances {:c1 10M :c2 10M})
  (let [t1 (ar/register :t1 (transaction/request {:money "1"
                                                  :sender :c1
                                                  :receiver :c2}))]
    (ar/run-processes! [[t1 0]
                        [t1 1]
                        [t1 ::transaction/give-money!]]))
  (is (= {:c1 9M :c2 10M}
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

(deftest test-run-step
  (reset! transaction/balances {:c1 500M :c2 300M})
  (let [t1 (ar/register :t1 (transaction/request {:money "50"
                                                  :sender :c1
                                                  :receiver :c2}))]
    (ar/run-step [t1 ::transaction/receive-money!]))
  (is (= {:c1 450M :c2 350M} @transaction/balances)))

(deftest test-nested
  (reset! nested/balances {:c1 500M :c2 300M})
  (let [t1 (ar/register :t1 (nested/request {:money "50"
                                             :sender :c1
                                             :receiver :c2}))]
    (ar/run-step [t1 :custom/receive-money])
    (Thread/sleep 100)
    (is (realized? (:proc t1)))
    (ar/cancel-remaining-steps [t1]))
  (is (= {:c1 450M :c2 350M} @nested/balances)))
