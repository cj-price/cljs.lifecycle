(ns lifecycle.core-test
  (:require [cljs.test :refer [deftest testing is async]]
            [lifecycle.core :as lc]))

;; --- Test records ---

(defrecord TestComponent [key log deps-map started?]
  lc/Lifecycle
  (start [this]
    (swap! log conj [:start key])
    (assoc this :started? true))
  (stop [this]
    (swap! log conj [:stop key])
    (assoc this :started? false)))

(defrecord AsyncComponent [key log deps-map started?]
  lc/Lifecycle
  (start [this]
    (js/Promise.
     (fn [resolve _]
       (js/setTimeout
        (fn []
          (swap! log conj [:start key])
          (resolve (assoc this :started? true)))
        1))))
  (stop [this]
    (js/Promise.
     (fn [resolve _]
       (js/setTimeout
        (fn []
          (swap! log conj [:stop key])
          (resolve (assoc this :started? false)))
        1)))))

(defrecord StartOnlyComponent [key log started?]
  lc/Lifecycle
  (start [this]
    (swap! log conj [:start key])
    (assoc this :started? true)))
;; stop uses default impl (returns this)

(defrecord FailStartComponent [key log]
  lc/Lifecycle
  (start [_]
    (js/Promise.reject (ex-info "boom starting" {:key key})))
  (stop [this]
    (swap! log conj [:stop key])
    this))

(defrecord FailStopComponent [key log started?]
  lc/Lifecycle
  (start [this]
    (swap! log conj [:start key])
    (assoc this :started? true))
  (stop [_]
    (js/Promise.reject (ex-info "boom stopping" {:key key}))))

(defrecord VersionedComponent [key log ver started?]
  lc/Lifecycle
  (start [this]
    (swap! log conj [:start key])
    (assoc this :started? true))
  (stop [this]
    (swap! log conj [:stop key])
    (assoc this :started? false))
  lc/Versioned
  (version [_] ver))

(defrecord AsyncVersionedComponent [key log ver started?]
  lc/Lifecycle
  (start [this]
    (js/Promise.
     (fn [resolve _]
       (js/setTimeout
        (fn []
          (swap! log conj [:start key])
          (resolve (assoc this :started? true)))
        1))))
  (stop [this]
    (js/Promise.
     (fn [resolve _]
       (js/setTimeout
        (fn []
          (swap! log conj [:stop key])
          (resolve (assoc this :started? false)))
        1))))
  lc/Versioned
  (version [_] ver))

(defn make-component [key log & {:keys [deps async?] :or {deps [] async? false}}]
  (let [comp (if async?
               (->AsyncComponent key log nil false)
               (->TestComponent key log nil false))]
    (if (seq deps)
      (lc/using comp deps)
      comp)))

;; ============================================================
;; Protocol basics
;; ============================================================

;; 1. Record start/stop
(deftest record-start-stop
  (let [log (atom [])
        comp (->TestComponent :a log nil false)
        started (lc/start comp)]
    (is (true? (:started? started)))
    (is (= [[:start :a]] @log))
    (let [stopped (lc/stop started)]
      (is (false? (:started? stopped)))
      (is (= [[:start :a] [:stop :a]] @log)))))

;; 2. Default passthrough — plain values pass through start/stop
(deftest default-passthrough
  (is (= 42 (lc/start 42)))
  (is (= "hello" (lc/stop "hello")))
  (is (= {:x 1} (lc/start {:x 1}))))

;; 3. Async start/stop on a single component
(deftest async-single-component
  (async done
    (let [log (atom [])
          comp (->AsyncComponent :a log nil false)]
      (-> (lc/start comp)
          (.then (fn [started]
                   (is (true? (:started? started)))
                   (is (= [[:start :a]] @log))
                   (lc/stop started)))
          (.then (fn [stopped]
                   (is (false? (:started? stopped)))
                   (is (= [[:start :a] [:stop :a]] @log))
                   (done)))))))

;; ============================================================
;; `using`
;; ============================================================

;; 4. Vector form
(deftest using-vector-form
  (let [comp (lc/using (->TestComponent :a (atom []) nil false) [:db :config])]
    (is (= {:db :db :config :config} (lc/component-deps comp)))))

;; 5. Map form (renamed keys)
(deftest using-map-form
  (let [comp (lc/using (->TestComponent :a (atom []) nil false) {:database :db})]
    (is (= {:database :db} (lc/component-deps comp)))))

;; 6. No deps
(deftest using-no-deps
  (let [comp (->TestComponent :a (atom []) nil false)]
    (is (nil? (lc/component-deps comp)))))

;; ============================================================
;; System creation
;; ============================================================

;; 7. Topo order
(deftest system-topo-order
  (let [log (atom [])
        sys (lc/system {:a (lc/using (->TestComponent :a log nil false) [:b])
                        :b (->TestComponent :b log nil false)})]
    (is (= [:b :a] (:order sys)))))

;; 8. Cycle detection
(deftest cycle-detection
  (is (thrown-with-msg?
       js/Error #"Cycle detected"
       (lc/system {:a (lc/using (->TestComponent :a (atom []) nil false) [:b])
                   :b (lc/using (->TestComponent :b (atom []) nil false) [:a])}))))

;; 9. Missing dep
(deftest missing-dep-detection
  (is (thrown-with-msg?
       js/Error #"does not exist"
       (lc/system {:a (lc/using (->TestComponent :a (atom []) nil false) [:missing])}))))

;; 10. Empty system
(deftest empty-system
  (async done
    (let [sys (lc/system {})]
      (is (= {} (:components sys)))
      (is (= [] (:order sys)))
      (-> (lc/start sys)
          (.then (fn [running]
                   (is (= {} (:components running)))
                   (lc/stop running)))
          (.then (fn [_] (done)))))))

;; ============================================================
;; Start / stop
;; ============================================================

;; 11. Sync system start/stop — instances and order correct
(deftest sync-system-start-stop
  (async done
    (let [log (atom [])
          sys (lc/system {:db    (make-component :db log)
                          :cache (make-component :cache log :deps [:db])})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (is (= [[:start :db] [:start :cache]] @log))
                   ;; db should be assoc'd onto cache
                   (let [cache (get (:components running) :cache)]
                     (is (true? (:started? cache)))
                     (is (some? (:db cache))))
                   (reset! log [])
                   (lc/stop running)))
          (.then (fn [stopped]
                   (is (= [[:stop :cache] [:stop :db]] @log))
                   (done)))))))

;; 12. Async system start/stop
(deftest async-system-start-stop
  (async done
    (let [log (atom [])
          sys (lc/system {:db    (make-component :db log :async? true)
                          :cache (make-component :cache log :deps [:db] :async? true)})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (is (= [[:start :db] [:start :cache]] @log))
                   (reset! log [])
                   (lc/stop running)))
          (.then (fn [_]
                   (is (= [[:stop :cache] [:stop :db]] @log))
                   (done)))))))

;; 13. Stop order — dependents stop before dependencies (3-level chain)
(deftest stop-order
  (async done
    (let [log (atom [])
          sys (lc/system {:a (make-component :a log)
                          :b (make-component :b log :deps [:a])
                          :c (make-component :c log :deps [:b])})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (lc/stop running)))
          (.then (fn [_]
                   (is (= [[:stop :c] [:stop :b] [:stop :a]] @log))
                   (done)))))))

;; 14. Optional stop — default impl just returns this
(deftest optional-stop
  (async done
    (let [log (atom [])
          sys (lc/system {:a (->StartOnlyComponent :a log false)})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (is (true? (:started? (get (:components running) :a))))
                   (lc/stop running)))
          (.then (fn [stopped]
                   ;; should not error, component passes through stop
                   (is (true? (:started? (get (:components stopped) :a))))
                   (done)))))))

;; 15. Deps assoc'd correctly
(deftest deps-assoc-correctly
  (async done
    (let [log (atom [])
          sys (lc/system {:db    (make-component :db log)
                          :cache (make-component :cache log :deps [:db])})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (let [cache (get (:components running) :cache)
                         db-in-cache (:db cache)]
                     (is (some? db-in-cache))
                     (is (= :db (:key db-in-cache)))
                     (is (true? (:started? db-in-cache))))
                   (done)))))))

;; 16. Renamed deps via map
(deftest renamed-deps
  (async done
    (let [log (atom [])
          sys (lc/system {:db    (make-component :db log)
                          :cache (lc/using (->TestComponent :cache log nil false) {:database :db})})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (let [cache (get (:components running) :cache)]
                     ;; dep should be on :database key, not :db
                     (is (some? (:database cache)))
                     (is (nil? (:db cache)))
                     (is (= :db (:key (:database cache)))))
                   (done)))))))

;; ============================================================
;; Composition
;; ============================================================

;; 17. Merging component maps into system
(deftest system-merge
  (async done
    (let [log (atom [])
          sys (lc/system (merge {:db (make-component :db log)}
                                {:cache (make-component :cache log :deps [:db])}))]
      (-> (lc/start sys)
          (.then (fn [running]
                   (is (= [[:start :db] [:start :cache]] @log))
                   (is (some? (get (:components running) :db)))
                   (is (some? (get (:components running) :cache)))
                   (done)))))))

;; 18. Override — later key wins
(deftest system-override
  (async done
    (let [log (atom [])
          sys (lc/system (merge {:db (make-component :db log)}
                                {:db (make-component :db-v2 log)}))]
      (-> (lc/start sys)
          (.then (fn [running]
                   (is (= [[:start :db-v2]] @log))
                   (done)))))))

;; ============================================================
;; Reload
;; ============================================================

;; 19. No changes — nothing stops/starts
(deftest reload-no-changes
  (async done
    (let [log (atom [])
          db (make-component :db log)
          sys (lc/system {:db db})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (lc/reload running (lc/system {:db db}))))
          (.then (fn [new-running]
                   (is (= [] @log))
                   (is (true? (:started? (get (:components new-running) :db))))
                   (done)))))))

;; 20. Changed component — only it + dependents restart
(deftest reload-changed-component
  (async done
    (let [log (atom [])
          db (make-component :db log)
          cache (lc/using (->VersionedComponent :cache log 1 false) [:db])
          api (make-component :api log :deps [:cache])
          sys (lc/system {:db db :cache cache :api api})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (let [new-cache (lc/using (->VersionedComponent :cache log 2 false) [:db])
                         new-sys (lc/system {:db db :cache new-cache :api api})]
                     (lc/reload running new-sys))))
          (.then (fn [_]
                   (is (= [[:stop :api] [:stop :cache]
                           [:start :cache] [:start :api]]
                          @log))
                   (done)))))))

;; 21. Added component — only new one starts
(deftest reload-added-component
  (async done
    (let [log (atom [])
          db (make-component :db log)
          sys (lc/system {:db db})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (let [new-sys (lc/system {:db db
                                             :cache (make-component :cache log :deps [:db])})]
                     (lc/reload running new-sys))))
          (.then (fn [new-running]
                   (is (= [[:start :cache]] @log))
                   (is (some? (get (:components new-running) :cache)))
                   (done)))))))

;; 22. Removed component — it + dependents stop
(deftest reload-removed-component
  (async done
    (let [log (atom [])
          db (make-component :db log)
          cache (make-component :cache log :deps [:db])
          sys (lc/system {:db db :cache cache})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (lc/reload running (lc/system {:db db}))))
          (.then (fn [new-running]
                   (is (= [[:stop :cache]] @log))
                   (is (nil? (get (:components new-running) :cache)))
                   (is (some? (get (:components new-running) :db)))
                   (done)))))))

;; 23. Async reload
(deftest reload-async
  (async done
    (let [log (atom [])
          db (make-component :db log :async? true)
          cache (lc/using (->AsyncVersionedComponent :cache log 1 false) [:db])
          sys (lc/system {:db db :cache cache})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (let [new-cache (lc/using (->AsyncVersionedComponent :cache log 2 false) [:db])
                         new-sys (lc/system {:db db :cache new-cache})]
                     (lc/reload running new-sys))))
          (.then (fn [_]
                   (is (= [[:stop :cache] [:start :cache]] @log))
                   (done)))))))

;; ============================================================
;; Edge cases
;; ============================================================

;; 24. Plain value component (uses default Lifecycle)
(deftest plain-value-component
  (async done
    (let [sys (lc/system {:config {:port 3000 :host "localhost"}})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (is (= {:port 3000 :host "localhost"}
                          (get (:components running) :config)))
                   (lc/stop running)))
          (.then (fn [stopped]
                   (is (= {:port 3000 :host "localhost"}
                          (get (:components stopped) :config)))
                   (done)))))))

;; 25. Mixed sync/async in same system
(deftest mixed-sync-async
  (async done
    (let [log (atom [])
          sys (lc/system {:db    (make-component :db log :async? true)
                          :cache (make-component :cache log :deps [:db])})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (is (= [[:start :db] [:start :cache]] @log))
                   (reset! log [])
                   (lc/stop running)))
          (.then (fn [_]
                   (is (= [[:stop :cache] [:stop :db]] @log))
                   (done)))))))

;; 27. Failed start rolls back already-started components and surfaces ex-info
(deftest start-failure-tears-down
  (async done
    (let [log (atom [])
          sys (lc/system {:good (make-component :good log)
                          :bad  (lc/using (->FailStartComponent :bad log) [:good])})]
      (-> (lc/start sys)
          (.then (fn [_]
                   (is false "start should have rejected")
                   (done)))
          (.catch (fn [err]
                    (is (= :bad (:failed-component (ex-data err))))
                    (is (some? (:system (ex-data err))))
                    (is (= [[:start :good] [:stop :good]] @log))
                    (done)))))))

;; 28. A rejecting stop doesn't strand the rest of the system
(deftest stop-failure-is-resilient
  (async done
    (let [log (atom [])
          sys (lc/system {:db  (make-component :db log)
                          :bad (lc/using (->FailStopComponent :bad log false) [:db])})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (lc/stop running)))
          (.then (fn [_]
                   (is (= [[:stop :db]] @log))
                   (done)))
          (.catch (fn [_]
                    (is false "system stop should not reject")
                    (done)))))))

;; 26. nil from stop doesn't break system stop
(deftest nil-from-stop
  (async done
    (let [log (atom [])
          comp (reify lc/Lifecycle
                 (start [this]
                   (swap! log conj [:start :nil-stop])
                   this)
                 (stop [_]
                   (swap! log conj [:stop :nil-stop])
                   nil))
          sys (lc/system {:a comp})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (lc/stop running)))
          (.then (fn [stopped]
                   (is (= [[:start :nil-stop] [:stop :nil-stop]] @log))
                   (done)))))))

;; ============================================================
;; Reload change-detection (value = / Versioned / deps)
;; ============================================================

;; 29. Value-equal new definitions restart nothing (the core = contract)
(deftest reload-value-equal-no-restart
  (async done
    (let [log (atom [])
          sys (lc/system {:db    (make-component :db log)
                          :cache (make-component :cache log :deps [:db])})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (let [new-sys (lc/system {:db    (make-component :db log)
                                             :cache (make-component :cache log :deps [:db])})]
                     (lc/reload running new-sys))))
          (.then (fn [_]
                   (is (= [] @log))
                   (done)))))))

;; 30. Versioned: a bumped version forces a restart even when value-equal
(deftest reload-version-bump-restarts
  (async done
    (let [log (atom [])
          sys (lc/system {:db (->VersionedComponent :db log 1 false)})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (lc/reload running (lc/system {:db (->VersionedComponent :db log 2 false)}))))
          (.then (fn [_]
                   (is (= [[:stop :db] [:start :db]] @log))
                   (done)))))))

;; 31. Versioned: same version restarts nothing
(deftest reload-same-version-no-restart
  (async done
    (let [log (atom [])
          sys (lc/system {:db (->VersionedComponent :db log 1 false)})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (lc/reload running (lc/system {:db (->VersionedComponent :db log 1 false)}))))
          (.then (fn [_]
                   (is (= [] @log))
                   (done)))))))

;; 32. A deps-only change (value-equal record, different deps) restarts
(deftest reload-deps-change-restarts
  (async done
    (let [log (atom [])
          db    (make-component :db log)
          extra (make-component :extra log)
          sys   (lc/system {:db    db
                            :extra extra
                            :cache (make-component :cache log :deps [:db])})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (let [new-sys (lc/system {:db    db
                                             :extra extra
                                             :cache (make-component :cache log :deps [:db :extra])})]
                     (lc/reload running new-sys))))
          (.then (fn [_]
                   (is (= [[:stop :cache] [:start :cache]] @log))
                   (done)))))))

;; 33. Reload start failure tears down components started during this reload
(deftest reload-start-failure-tears-down
  (async done
    (let [log (atom [])
          db (make-component :db log)
          cache (lc/using (->VersionedComponent :cache log 1 false) [:db])
          api (lc/using (->VersionedComponent :api log 1 false) [:cache])
          sys (lc/system {:db db :cache cache :api api})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (let [new-cache (lc/using (->VersionedComponent :cache log 2 false) [:db])
                         new-api (lc/using (->FailStartComponent :api log) [:cache])
                         new-sys (lc/system {:db db :cache new-cache :api new-api})]
                     (lc/reload running new-sys))))
          (.then (fn [_]
                   (is false "reload should have rejected")
                   (done)))
          (.catch (fn [err]
                    (is (= :api (:failed-component (ex-data err))))
                    (is (= [[:stop :api] [:stop :cache]
                            [:start :cache] [:stop :cache]]
                           @log))
                    (done)))))))

;; 34. Reload completes even when an affected old instance's stop rejects
(deftest reload-stop-failure-is-resilient
  (async done
    (let [log (atom [])
          db (make-component :db log)
          bad (lc/using (->FailStopComponent :bad log false) [:db])
          sys (lc/system {:db db :bad bad})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (let [new-bad (lc/using (->VersionedComponent :bad log 2 false) [:db])
                         new-sys (lc/system {:db db :bad new-bad})]
                     (lc/reload running new-sys))))
          (.then (fn [new-running]
                   (is (= [[:start :bad]] @log))
                   (is (true? (:started? (get (:components new-running) :bad))))
                   (done)))
          (.catch (fn [_]
                    (is false "reload should not reject")
                    (done)))))))

;; ============================================================
;; Change-detection asymmetry & value change during reload
;; ============================================================

;; 35. Versioned -> non-Versioned swap forces a restart
(deftest reload-versioned-to-plain-restarts
  (async done
    (let [log (atom [])
          sys (lc/system {:svc (->VersionedComponent :svc log 1 false)})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (lc/reload running (lc/system {:svc (->TestComponent :svc log nil false)}))))
          (.then (fn [_]
                   (is (= [[:stop :svc] [:start :svc]] @log))
                   (done)))))))

;; 36. non-Versioned -> Versioned swap forces a restart
(deftest reload-plain-to-versioned-restarts
  (async done
    (let [log (atom [])
          sys (lc/system {:svc (->TestComponent :svc log nil false)})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (lc/reload running (lc/system {:svc (->VersionedComponent :svc log 1 false)}))))
          (.then (fn [_]
                   (is (= [[:stop :svc] [:start :svc]] @log))
                   (done)))))))

;; 37. Non-Versioned value change (differing field) restarts the component
(deftest reload-plain-value-change-restarts
  (async done
    (let [log (atom [])
          db  (make-component :db log)
          sys (lc/system {:db db :cache (make-component :cache log :deps [:db])})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (lc/reload running (lc/system {:db db
                                                  :cache (make-component :cache-v2 log :deps [:db])}))))
          (.then (fn [_]
                   (is (= [[:stop :cache] [:start :cache-v2]] @log))
                   (done)))))))

;; ============================================================
;; DAG shapes (diamond) — topo order & multi-path dependents
;; ============================================================

(defn- op-order [log op]
  (into [] (comp (filter #(= op (first %))) (map second)) log))

(defn- idx [xs v]
  (first (keep-indexed (fn [i x] (when (= x v) i)) xs)))

(defn- before? [xs a b]
  (< (idx xs a) (idx xs b)))

;; 38. Diamond topo order: root first, sink last, deps precede dependents
(deftest diamond-topo-order
  (async done
    (let [log (atom [])
          sys (lc/system {:a (make-component :a log)
                          :b (make-component :b log :deps [:a])
                          :c (make-component :c log :deps [:a])
                          :d (make-component :d log :deps [:b :c])})]
      (is (= :a (first (:order sys))))
      (is (= :d (last (:order sys))))
      (-> (lc/start sys)
          (.then (fn [_]
                   (let [starts (op-order @log :start)]
                     (is (= #{:a :b :c :d} (set starts)))
                     (is (before? starts :a :b))
                     (is (before? starts :a :c))
                     (is (before? starts :b :d))
                     (is (before? starts :c :d)))
                   (done)))))))

;; 39. Diamond reload: changing the root restarts every node via both paths
(deftest diamond-reload-root-change-restarts-all
  (async done
    (let [log (atom [])
          sys (lc/system {:a (->VersionedComponent :a log 1 false)
                          :b (make-component :b log :deps [:a])
                          :c (make-component :c log :deps [:a])
                          :d (make-component :d log :deps [:b :c])})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (lc/reload running
                              (lc/system {:a (->VersionedComponent :a log 2 false)
                                          :b (make-component :b log :deps [:a])
                                          :c (make-component :c log :deps [:a])
                                          :d (make-component :d log :deps [:b :c])}))))
          (.then (fn [_]
                   (let [stops  (op-order @log :stop)
                         starts (op-order @log :start)]
                     (is (= #{:a :b :c :d} (set stops)))
                     (is (= #{:a :b :c :d} (set starts)))
                     (is (before? stops :d :b))
                     (is (before? stops :d :c))
                     (is (before? stops :b :a))
                     (is (before? stops :c :a))
                     (is (before? starts :a :b))
                     (is (before? starts :a :c))
                     (is (before? starts :b :d))
                     (is (before? starts :c :d)))
                   (done)))))))

;; ============================================================
;; Combined & full-teardown reloads
;; ============================================================

;; 40. One reload that changes, adds, and removes components at once
(deftest reload-combined-change-add-remove
  (async done
    (let [log (atom [])
          db    (make-component :db log)
          cache (lc/using (->VersionedComponent :cache log 1 false) [:db])
          api   (make-component :api log :deps [:cache])
          sys   (lc/system {:db db :cache cache :api api})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (let [new-cache (lc/using (->VersionedComponent :cache log 2 false) [:db])
                         new-sys   (lc/system {:db db
                                               :cache new-cache
                                               :logger (make-component :logger log)})]
                     (lc/reload running new-sys))))
          (.then (fn [new-running]
                   (is (= [[:stop :api] [:stop :cache]] (into [] (take 2) @log)))
                   (is (= #{:cache :logger} (set (op-order @log :start))))
                   (is (nil? (get (:components new-running) :api)))
                   (is (some? (get (:components new-running) :logger)))
                   (is (some? (get (:components new-running) :cache)))
                   (done)))))))

;; 41. Reload into the empty system stops everything in reverse order
(deftest reload-to-empty-system
  (async done
    (let [log (atom [])
          db  (make-component :db log)
          sys (lc/system {:db db :cache (make-component :cache log :deps [:db])})]
      (-> (lc/start sys)
          (.then (fn [running]
                   (reset! log [])
                   (lc/reload running (lc/system {}))))
          (.then (fn [new-running]
                   (is (= [[:stop :cache] [:stop :db]] @log))
                   (is (= {} (:components new-running)))
                   (done)))))))

;; ============================================================
;; Error ex-info discriminator (:lifecycle/error-type)
;; ============================================================

;; 42. Missing dep carries :lifecycle/error-type :missing-dep
(deftest error-type-missing-dep
  (try
    (lc/system {:a (lc/using (->TestComponent :a (atom []) nil false) [:missing])})
    (is false "should throw")
    (catch :default e
      (is (= :missing-dep (:lifecycle/error-type (ex-data e))))
      (is (= :missing (:missing-dep (ex-data e))))
      (is (= :a (:failed-component (ex-data e)))))))

;; 43. Cycle carries :lifecycle/error-type :cycle and reports the cycle keys
(deftest error-type-cycle
  (try
    (lc/system {:a (lc/using (->TestComponent :a (atom []) nil false) [:b])
                :b (lc/using (->TestComponent :b (atom []) nil false) [:a])})
    (is false "should throw")
    (catch :default e
      (is (= :cycle (:lifecycle/error-type (ex-data e))))
      (is (= #{:a :b} (set (:cycle (ex-data e))))))))

;; 44. Start failure carries :lifecycle/error-type :start-failed
(deftest error-type-start-failed
  (async done
    (let [log (atom [])
          sys (lc/system {:good (make-component :good log)
                          :bad  (lc/using (->FailStartComponent :bad log) [:good])})]
      (-> (lc/start sys)
          (.then (fn [_] (is false "start should reject") (done)))
          (.catch (fn [e]
                    (is (= :start-failed (:lifecycle/error-type (ex-data e))))
                    (is (= :bad (:failed-component (ex-data e))))
                    (done)))))))

;; ============================================================
;; Lifecycle phase marker
;; ============================================================

;; 45. Phase transitions: defined -> started -> stopped
(deftest phase-transitions
  (async done
    (let [log (atom [])
          sys (lc/system {:db (make-component :db log)})]
      (is (= :defined (:phase sys)))
      (-> (lc/start sys)
          (.then (fn [running]
                   (is (= :started (:phase running)))
                   (lc/stop running)))
          (.then (fn [stopped]
                   (is (= :stopped (:phase stopped)))
                   (done)))))))

;; 46. reload rejects a system that was never started
(deftest reload-rejects-unstarted-system
  (async done
    (let [log (atom [])
          sys (lc/system {:db (make-component :db log)})]
      (-> (lc/reload sys (lc/system {:db (make-component :db log)}))
          (.then (fn [_]
                   (is false "reload should reject an unstarted system")
                   (done)))
          (.catch (fn [e]
                    (is (= :not-started (:lifecycle/error-type (ex-data e))))
                    (is (= :defined (:phase (ex-data e))))
                    (is (= [] @log))
                    (done)))))))

;; 47. reload rejects an already-stopped system
(deftest reload-rejects-stopped-system
  (async done
    (let [log (atom [])
          sys (lc/system {:db (make-component :db log)})]
      (-> (lc/start sys)
          (.then (fn [running] (lc/stop running)))
          (.then (fn [stopped]
                   (lc/reload stopped (lc/system {:db (make-component :db log)}))))
          (.then (fn [_]
                   (is false "reload should reject a stopped system")
                   (done)))
          (.catch (fn [e]
                    (is (= :not-started (:lifecycle/error-type (ex-data e))))
                    (is (= :stopped (:phase (ex-data e))))
                    (done)))))))
