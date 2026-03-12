(ns lifecycle.core
  (:require [clojure.set :as set]))

;; --- Helpers (private) ---

(defn- promise? [x]
  (instance? js/Promise x))

(defn- ->promise [x]
  (if (promise? x)
    x
    (js/Promise.resolve x)))

;; --- Protocol ---

(defprotocol Lifecycle
  (start [component] "Start this component. May return a Promise.")
  (stop [component] "Stop this component. May return a Promise."))

(extend-type default
  Lifecycle
  (start [this] this)
  (stop [this] this))

(defprotocol Versioned
  "Optional protocol. A component may implement it to report a version used by
   `reload` for change-detection, overriding the default value-based comparison.
   Useful when a component's spec closes over a function (never value-equal)."
  (version [component] "Return this component's version (any =-comparable value)."))

;; --- Dependency declaration ---

(defn component-deps
  "Returns the dependency spec for a component (from metadata)."
  [component]
  (::deps (meta component)))

(defn using
  "Declare dependencies for a component.
   deps: vector [:db :config] or map {:local-key :system-key}"
  [component deps]
  (vary-meta component assoc ::deps
             (if (map? deps)
               deps
               (zipmap deps deps))))

;; --- Internal helpers ---

(defn- validate-deps!
  "Throws if any component references a dep that doesn't exist."
  [component-map]
  (let [ks (set (keys component-map))]
    (doseq [[k component] component-map
            :let [deps (component-deps component)]
            :when deps
            [_ sys-key] deps]
      (when-not (contains? ks sys-key)
        (throw (ex-info (str "Component " k " depends on " sys-key " which does not exist")
                        {:component k
                         :failed-component k
                         :missing-dep sys-key
                         :lifecycle/error-type :missing-dep}))))))

(defn- dep-keys
  "Returns the set of system keys that component k depends on."
  [component-map k]
  (let [deps (component-deps (get component-map k))]
    (set (vals deps))))

(defn- component-changed?
  "True if new differs from old for reload purposes. Components implementing
   Versioned compare by version alone; otherwise by value and by declared deps
   (deps live in metadata, which = ignores)."
  [old new]
  (let [old-versioned? (satisfies? Versioned old)
        new-versioned? (satisfies? Versioned new)]
    (cond
      (not= old-versioned? new-versioned?) true
      (and old-versioned? new-versioned?) (not= (version old) (version new))
      :else (or (not= old new)
                (not= (component-deps old) (component-deps new))))))

(defn- adjacency
  "Builds the dependency adjacency map: dep -> set of dependents."
  [component-map]
  (reduce
   (fn [m k]
     (reduce (fn [m d] (update m d (fnil conj #{}) k))
             m
             (dep-keys component-map k)))
   {}
   (keys component-map)))

(defn- topo-sort
  "Kahn's algorithm. Returns ordered vector of component keys.
   Throws on cycles."
  [component-map]
  (let [ks (keys component-map)
        deps-by-key (into {} (map (fn [k] [k (dep-keys component-map k)])) ks)
        in-degree (reduce-kv (fn [m k ds] (assoc m k (count ds))) {} deps-by-key)
        adj (reduce-kv (fn [m k ds]
                         (reduce (fn [m d] (update m d (fnil conj #{}) k)) m ds))
                       {} deps-by-key)]
    (loop [queue (into [] (comp (filter (fn [[_ d]] (zero? d))) (map first)) in-degree)
           in-deg in-degree
           order []]
      (if (empty? queue)
        (if (= (count order) (count component-map))
          order
          (throw (ex-info "Cycle detected in component dependencies"
                          {:components (keys component-map)
                           :cycle (remove (set order) (keys component-map))
                           :lifecycle/error-type :cycle})))
        (let [k (first queue)
              queue (subvec queue 1)
              order (conj order k)
              dependents (get adj k #{})
              [in-deg new-queue]
              (reduce
               (fn [[in-deg q] dep]
                 (let [new-deg (dec (get in-deg dep))]
                   [(assoc in-deg dep new-deg)
                    (if (zero? new-deg) (conj q dep) q)]))
               [in-deg queue]
               dependents)]
          (recur new-queue in-deg order))))))

(defn- dependents-of
  "Returns the set of transitive dependents of the given keys."
  [component-map ks]
  (let [adj (adjacency component-map)]
    (loop [frontier (set ks)
           visited #{}]
      (if (empty? frontier)
        visited
        (let [next-level (reduce
                          (fn [s k] (into s (get adj k #{})))
                          #{}
                          frontier)
              next-level (set/difference next-level visited frontier)]
          (recur next-level (into visited frontier)))))))

(defn- assoc-deps
  "Assoc resolved dependencies onto a component based on its dep spec."
  [component instances]
  (let [deps (component-deps component)]
    (if (seq deps)
      (reduce-kv
       (fn [comp local-key sys-key]
         (assoc comp local-key (get instances sys-key)))
       component
       deps)
      component)))

;; --- SystemMap ---

(declare system-start system-stop)

(defrecord SystemMap [components order definitions]
  Lifecycle
  (start [this] (system-start this))
  (stop [this] (system-stop this)))

(defn- stop-all
  "Stop every started instance in `instances` in reverse `order`, resiliently:
   a failing stop is swallowed so the remaining components still get stopped.
   Returns a Promise of the map of stopped instances (nil where stop failed)."
  [instances order]
  (reduce
   (fn [p k]
     (.then p
            (fn [stopped]
              (if (contains? instances k)
                (-> (->promise (stop (get instances k)))
                    (.then (fn [stopped-comp] (assoc stopped k stopped-comp)))
                    (.catch (fn [_] (assoc stopped k nil))))
                (js/Promise.resolve stopped)))))
   (js/Promise.resolve {})
   (reverse order)))

(defn- start-in-order
  "Start `start-keys` in `order`, threading the instance map from `seed`.
   On failure, stop the instances named by `rollback-keys` (resiliently) then
   reject with an ex-info carrying :failed-component and a partial :system.
   Returns a Promise of the instance map."
  [comps order start-keys seed rollback-keys msg-fn]
  (reduce
   (fn [p k]
     (.then p
            (fn [instances]
              (let [with-deps (assoc-deps (get comps k) instances)]
                (-> (->promise (start with-deps))
                    (.then (fn [started-comp]
                             (assoc instances k started-comp)))
                    (.catch (fn [err]
                              (.then (stop-all (select-keys instances rollback-keys) order)
                                     (fn [_]
                                       (throw (ex-info (msg-fn k)
                                                       {:failed-component k
                                                        :lifecycle/error-type :start-failed
                                                        :system (->SystemMap instances order comps)}
                                                       err)))))))))))
   (js/Promise.resolve seed)
   start-keys))

(defn- system-start [sys]
  (let [{:keys [components order definitions]} sys]
    (.then
     (start-in-order components order order {} order
                     #(str "Failed to start component " %))
     (fn [started]
       (assoc (->SystemMap started order definitions) :phase :started)))))

(defn- system-stop [sys]
  (let [{:keys [components order]} sys]
    (.then
     (stop-all components order)
     (fn [stopped]
       (assoc (->SystemMap stopped order (:definitions sys)) :phase :stopped)))))

;; --- Public API ---

(defn system
  "Create a SystemMap from a map of key -> component.
   Components should declare deps via `using`.
   Validates deps and computes topological order."
  [component-map]
  (validate-deps! component-map)
  (let [order (topo-sort component-map)]
    (assoc (->SystemMap component-map order component-map) :phase :defined)))

(defn reload
  "Hot-reload: diff old vs new system, stop changed + dependents, restart.
   A component is changed when its value (=), declared deps, or `Versioned`
   version differ; components implementing Versioned compare by version alone.
   Rejects if `running-system` is not a started system.
   Returns a Promise resolving to the new running SystemMap."
  [running-system new-system]
  (if (not= :started (:phase running-system))
    (js/Promise.reject
     (ex-info "reload expects a started system"
              {:lifecycle/error-type :not-started
               :phase (:phase running-system)}))
    (let [old-comps (:definitions running-system)
          old-instances (:components running-system)
          new-comps (:definitions new-system)
          changed (into #{}
                        (filter (fn [k]
                                  (component-changed? (get old-comps k)
                                                      (get new-comps k))))
                        (keys new-comps))
          removed (into #{}
                        (filter (fn [k] (not (contains? new-comps k))))
                        (keys old-comps))
          affected-seeds (set/union changed removed)
          old-dependents (dependents-of old-comps affected-seeds)
          new-dependents (dependents-of new-comps changed)
          to-start (filterv (fn [k] (contains? new-dependents k))
                            (:order new-system))
          kept (reduce dissoc old-instances old-dependents)]
      (-> (stop-all (select-keys old-instances old-dependents)
                    (:order running-system))
          (.then
           (fn [_]
             (start-in-order new-comps (:order new-system) to-start kept to-start
                             #(str "Failed to start component " % " during reload"))))
          (.then
           (fn [instances]
             (assoc (->SystemMap instances (:order new-system) new-comps)
                    :phase :started)))))))
