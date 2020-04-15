(ns arrudeia.core)

(def ^:dynamic *proc-name*)
(def ^:dynamic *bypass* false)

(defmacro with-bypass
  [& body]
  `(binding [*bypass* true]
    ~@body))

(defonce semaphore (atom {:debug []}))

(defn waiting-step
  ([[proc-name keyword idx]]
   (waiting-step {} [proc-name keyword idx]))
  ([args [proc-name keyword idx]]
   (when (not *bypass*)
     (while (not (or (and (get @semaphore [proc-name :arrudeia/next])
                          (not (contains? #{keyword idx}
                                          (get @semaphore [proc-name :arrudeia/next]))))
                     (= (get-in @semaphore [proc-name idx]) :start)
                     (= (get-in @semaphore [proc-name keyword]) :start)))
       (when (Thread/interrupted)
         (.stop (Thread/currentThread))))
     (when (and (get @semaphore [proc-name :arrudeia/next])
                (contains? #{keyword idx}
                           (get @semaphore [proc-name :arrudeia/next])))
       (swap! semaphore assoc [proc-name :arrudeia/next] nil))
     (swap! semaphore update :debug conj {:start [proc-name keyword idx]}))
   args))

(defn done-step
  [args [proc-name keyword idx]]
  (when (not *bypass*)
    (swap! semaphore update :debug conj {:done [proc-name keyword idx]})
    (swap! semaphore assoc-in [proc-name [idx :args-after]] args)
    (swap! semaphore assoc-in [proc-name idx] :done)
    (swap! semaphore assoc-in [proc-name [keyword :args-after]] args)
    (swap! semaphore assoc-in [proc-name keyword] :done))
  args)

(defn var->keyword
  [v]
  (keyword (str (:ns (meta v)))
           (str (:name (meta v)))))

(defmacro label
  [{:keys [:identifier :idx]} & body]
  `(do
     (waiting-step {} [*proc-name* ~identifier ~idx])
     (done-step
      ~@body
      [*proc-name* ~identifier ~idx])))

(defmacro ->*
  [& forms]
  (let [keyword-steps
        (map-indexed (fn [idx form]
                       (cond
                         (and (zero? idx)
                              (symbol? form)) (keyword (str *ns*) (str form))
                         (symbol? form) (var->keyword (resolve form))
                         (list? form) (let [res-form (resolve (first form))]
                                        (when-not (:macro (meta res-form))
                                          (var->keyword res-form)))
                         :else idx))
                     forms)]
    `(-> ~@(->> forms
            (map (fn [idx k form]
                   (cond
                     (zero? idx) `(label {:identifier ~k :idx ~idx} ~form)
                     (nil? k) `((fn [args#] (-> args# ~form)))
                     :else
                     `((fn [args#]
                         (label {:identifier ~k :idx ~idx} (-> args# ~form))))))
                 (range)
                 keyword-steps)))))

(defmacro register
  [proc-name pipe]
  `(let [p# {:proc (future
                     (try (binding [*proc-name* ~proc-name]
                            ~pipe)
                          (catch Exception e#
                            (spit "a.txt"
                                  (with-out-str
                                    (clojure.pprint/pprint e#))))))
             :proc-name ~proc-name}]
     p#))

(defn run-step
  ([proc-with-step]
   (run-step proc-with-step {}))
  ([[{:keys [:proc-name]} step]
    {:keys [:run-intermediate-steps?]
     :or {run-intermediate-steps? true}}]
   (when run-intermediate-steps?
     (swap! semaphore assoc [proc-name :arrudeia/next] step))
   ;; trigger new step
   (swap! semaphore assoc-in [proc-name step] :start)
   ;; wait for triggered step
   (while (not= (get-in @semaphore [proc-name step]) :done))
   (get-in @semaphore [proc-name [step :args-after]])))

(defn cancel-remaining-steps
  [procs]
  (run! future-cancel (set (mapv :proc procs))))

(defn run-processes!
  [procs]
  (swap! semaphore (constantly {:debug []}))
  (run! run-step procs)
  ;; cancel all remaining futures processes
  (cancel-remaining-steps (mapv first procs))
  ;; return debug information
  (let [debug (:debug @semaphore)]
    (swap! semaphore (constantly {:debug []}))
    debug))





































(comment

  (do
    (reset! balances {:c1 500M :c2 300M})
    (reset! a {:debug []})
    (let [t1 (register :t1
                       (request {:money "50" :sender :c1 :receiver :c2}))
          t2 (register :t2
                       (request {:money "225" :sender :c2 :receiver :c1}))]
      (run-processes! #_[[t1 0]
                         [t1 1]
                         [t2 0]
                         [t2 1]
                         [t1 2]
                         [t2 2]
                         [t1 3]
                         [t2 3]]
                      [[t1 ::adapt]
                       [t1 ::add-new-amounts]
                       [t2 ::adapt]
                       [t2 ::add-new-amounts]
                       [t1 ::give-money!]
                       [t2 ::give-money!]
                       [t1 ::receive-money!]
                       [t2 ::receive-money!]])
      @(:proc t1) @(:proc t2))
    #_(= 800M (reduce + 0.0M (vals @balances)))
    ;; test error is stable
    (= 1075M (reduce + 0.0M (vals @balances))))

  (do
    (reset! balances {:c1 10M :c2 10M})
    (reset! a {:debug []})
    (let [t1 (register :t1
                       (request {:money "1" :sender :c1 :receiver :c2}))
          t2 (register :t2
                       (request {:money "1" :sender :c1 :receiver :c2}))]
      (run-processes! [[t1 ::adapt]
                       [t2 ::adapt]
                       [t1 ::add-new-amounts]
                       [t2 ::add-new-amounts]
                       [t1 ::give-money!]
                       [t1 ::receive-money!]
                       [t2 ::give-money!]
                       [t2 ::receive-money!]])
      @(:proc t1) @(:proc t2))
    #_(= 800M (reduce + 0.0M (vals @balances)))
    ;; test error is stable
    (reduce + 0.0M (vals @balances)))

  (do
    ;; 22
    (reset! balances {:c1 10M :c2 10M})
    (reset! a {:debug []})
    (let [t1 (register :t1
                       (request {:money "1" :sender :c1 :receiver :c2}))
          t2 (register :t2
                       (request {:money "3" :sender :c1 :receiver :c2}))]
      (run-processes! [[t1 ::adapt]
                       [t2 ::adapt]
                       [t1 ::add-new-amounts]
                       [t2 ::add-new-amounts]
                       [t2 ::give-money!]
                       [t1 ::give-money!]
                       [t2 ::receive-money!]
                       [t1 ::receive-money!]])
      @(:proc t1) @(:proc t2))
    #_(= 800M (reduce + 0.0M (vals @balances)))
    ;; test error is stable
    (reduce + 0.0M (vals @balances)))

  (:debug @a)
  (@b)

  (swap! a assoc-in [:p2 0] true)
  (swap! a assoc-in [:p2 1] true)
  (swap! a assoc-in [:p2 2] true)

  balances

  ())
