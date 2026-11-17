;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.cljc.checker.check.get
  (:require [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.check.invoke-kw :as invoke-kw]
            [typed.cljc.checker.check.utils :as cu]))

(defn invoke-get
  "Given an unanalyzed :host-call or :invoke invocation
  of clojure.lang.RT/get or clojure.core/get (respectively)
  with fully type checked :args, checks expr at expected type
  and returns a fully analyzed expression with a TCResult u/expr-type entry,
  or returns nil."
  [{:keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:pre [(#{:invoke :host-call} (:op expr))
         (every? (every-pred (comp #{:unanalyzed} :op)
                             (complement u/expr-type))
                 args)
         ((some-fn nil? r/TCResult?) expected)]
   :post [((some-fn nil?
                    (comp r/TCResult? u/expr-type))
           %)]}
  (when (#{2 3} (count args))
    (when (keyword? (-> args second :form))
      (let [expr (cond-> (-> expr
                             (update :args #(mapv (fn [e] (check-expr e nil opts)) %)))
                   (#{:host-call} (:op expr))
                   (update :target check-expr nil opts))
            [ttarget kwr tdefault] (map u/expr-type (:args expr))]
        (assoc expr
               u/expr-type (invoke-kw/invoke-keyword
                             expr
                             kwr
                             ttarget
                             tdefault
                             expected
                             opts))))))

(defn invoke-req!
  "Clojure 1.13 (req! m k) / RT/req: like get, but throws when k is absent.
  Typed as a lookup with no nil default: an optional key yields its value type,
  a key unknown to a complete HMap is a 'Property does not exist' error."
  [{:keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  (when (= 2 (count args))
    (when (keyword? (-> args second :form))
      (let [expr (cond-> (update expr :args #(mapv (fn [e] (check-expr e nil opts)) %))
                   (#{:host-call} (:op expr)) (update :target check-expr nil opts))
            [ttarget kwr] (map u/expr-type (:args expr))
            targett (c/-resolve (r/ret-t ttarget) opts)]
        (invoke-kw/check-missing-property! targett (r/ret-t kwr) (assoc opts :clojure.core.typed.util-vars/current-env (:env expr)))
        (assoc expr
               u/expr-type (invoke-kw/invoke-keyword expr kwr ttarget (r/ret (r/Bottom)) expected opts))))))
