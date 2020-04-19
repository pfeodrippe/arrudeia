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

;; The tagged literal `#ar/->*` replaces all `clojure.core/->` for
;; `arrudeia.core/->*` at the following form.
#ar/->*
(defn request
  [data]
  (-> (adapt data)
      add-new-amounts
      (-> give-money!
          receive-money!))
  @balances)
