# cljs.lifecycle

A protocol-based lifecycle management library for ClojureScript, inspired by
Stuart Sierra's Component. Define components as records implementing the
`Lifecycle` protocol, declare dependencies with `using`, and manage systems
that start/stop in topological order with incremental hot-reload.

## Commands

- Run tests: `pnpm shadow-cljs -A:test compile test`

## Usage

```clojure
(ns myapp.system
  (:require [lifecycle.core :as lc]))

;; Sync component
(defrecord Cache [db store]
  lc/Lifecycle
  (start [this]
    (assoc this :store (atom {})))
  (stop [this]
    (assoc this :store nil)))

;; Async component — start/stop return Promises. `config` is injected by `using`.
(defrecord Database [uri config conn]
  lc/Lifecycle
  (start [this]
    (-> (js/Promise.resolve {:uri uri :port (:port config) :connected true})
        (.then (fn [conn] (assoc this :conn conn)))))
  (stop [this]
    (-> (js/Promise.resolve nil)
        (.then (fn [_] (assoc this :conn nil))))))

;; Build the system — sync and async components mix freely
(def my-system
  (lc/system
    {:config {:port 3000}  ;; plain values pass through (default Lifecycle)
     :db     (lc/using (->Database "postgres://localhost/mydb" nil nil) [:config])
     :cache  (lc/using (->Cache nil nil) [:db])}))

;; Start / stop / reload — always returns a Promise
(-> (lc/start my-system)
    (.then (fn [running]
             ;; (:db (:components running)) => started Database record
             running)))

(-> (lc/stop running-system)
    (.then (fn [_] (println "stopped"))))

;; Hot-reload: only changed components + dependents restart
(-> (lc/reload running-system new-system)
    (.then (fn [reloaded] reloaded)))
```

## Notes

- **Error model**: `system` throws *synchronously* on a missing dependency or a
  dependency cycle. `start`, `stop`, and `reload` never throw — they return a
  Promise that *rejects*. Every error is an `ex-info` whose data carries
  `:lifecycle/error-type` (`:missing-dep`, `:cycle`, or `:start-failed`); a
  failed start/reload also carries `:failed-component` and a partially
  torn-down `:system`.

- **Reload change-detection** compares component definitions by value (`=`): a
  component restarts only when its value, declared deps, or `Versioned` version
  differ. A spec that closes over a function, atom, or other identity-compared
  value is never value-equal across two fresh builds, so it would restart on
  *every* reload. Implement `Versioned` and return a stable, `=`-comparable
  version to opt such a component into incremental reload.

- **SystemMap** is produced only by `system` / `start` / `reload` — don't build
  it by hand (that bypasses validation and breaks reload). `:components` holds
  the running instances (the raw definitions before `start`), `:definitions` the
  original specs used for reload diffing, `:order` the topological start order,
  and `:phase` the lifecycle stage (`:defined` → `:started` → `:stopped`).
  `reload` and `stop` expect an already-started system; `reload` *rejects* with
  `:lifecycle/error-type :not-started` if handed a system whose `:phase` is not
  `:started`.
