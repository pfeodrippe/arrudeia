(ns arrudeia.example.transaction-nested
  (:require
   [arrudeia.core :refer [->*] :as ar]))

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
  (ar/with-label :custom/receive-money
    (swap! balances assoc receiver receiver-new-amount))
  args)

(defn- transfer-money!
  [args]
  (receive-money! (->* (give-money! args))))

#ar/->*
(defn request
  [data]
  (-> (adapt data)
      ;; instead of ::add-new-amounts, step name will be :add
      ^{:arrudeia/name :add} add-new-amounts
      transfer-money!)
  @balances)
