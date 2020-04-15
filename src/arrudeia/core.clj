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
                     (binding [*proc-name* ~proc-name]
                       ~pipe))
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
