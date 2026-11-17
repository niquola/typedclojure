;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.provider.spec
  "Static types from clojure.spec: (s/fdef f :args ... :ret ...) becomes the
  Typed Clojure type of f, so specs are checked at compile time like t/ann.

  s/keys maps are open by default (as in spec). With
  -Dtyped.clojure.spec.closed-keys=true they become complete HMaps, enabling
  'Property does not exist' / excess-key errors as in TypeScript."
  (:require [clojure.spec.alpha :as s]))

(def ^:private pred->type
  '{clojure.core/any? typed.clojure/Any
    clojure.core/some? (typed.clojure/Not nil)
    clojure.core/nil? (typed.clojure/Val nil)
    clojure.core/int? typed.clojure/AnyInteger
    clojure.core/integer? typed.clojure/AnyInteger
    clojure.core/pos-int? typed.clojure/AnyInteger
    clojure.core/nat-int? typed.clojure/AnyInteger
    clojure.core/neg-int? typed.clojure/AnyInteger
    clojure.core/number? typed.clojure/Num
    clojure.core/double? Double
    clojure.core/string? typed.clojure/Str
    clojure.core/keyword? typed.clojure/Kw
    clojure.core/qualified-keyword? typed.clojure/Kw
    clojure.core/simple-keyword? typed.clojure/Kw
    clojure.core/symbol? typed.clojure/Sym
    clojure.core/boolean? typed.clojure/Bool
    clojure.core/uuid? java.util.UUID
    clojure.core/inst? java.util.Date
    clojure.core/map? (typed.clojure/Map typed.clojure/Any typed.clojure/Any)
    clojure.core/vector? (typed.clojure/Vec typed.clojure/Any)
    clojure.core/coll? (typed.clojure/Coll typed.clojure/Any)
    clojure.core/fn? typed.clojure/AnyFunction
    clojure.core/ifn? typed.clojure/AnyFunction})

(defn- closed? [] (= "true" (System/getProperty "typed.clojure.spec.closed-keys")))

(declare spec->type)

(defn- key-type [k seen]
  (if (s/get-spec k) (spec->type (s/form k) seen) 'typed.clojure/Any))

(defn- keys->type [args seen]
  (let [{:keys [req req-un opt opt-un]} (apply hash-map args)
        flat (fn [ks] (remove #(and (seq? %) (not (keyword? %))) ks)) ;; (or ...) groups ignored
        un (fn [ks] (into {} (map (fn [k] [(keyword (name k)) (key-type k seen)])) (flat ks)))
        q  (fn [ks] (into {} (map (fn [k] [k (key-type k seen)])) (flat ks)))]
    (list 'typed.clojure/HMap
          :mandatory (merge (q req) (un req-un))
          :optional (merge (q opt) (un opt-un))
          :complete? (closed?))))

(defn- regex->argv
  "s/cat → fixed argument vector; returns nil for unsupported regexes."
  [form seen]
  (when (and (seq? form) (= `s/cat (first form)))
    (mapv #(spec->type % seen) (take-nth 2 (rest (rest form))))))

(defn spec->type
  "Convert a spec form (as returned by s/form) to Typed Clojure type syntax."
  ([form] (spec->type form #{}))
  ([form seen]
   (cond
     (keyword? form) (if (seen form)
                       'typed.clojure/Any
                       (if-let [sp (s/get-spec form)]
                         (spec->type (s/form sp) (conj seen form))
                         'typed.clojure/Any))
     (symbol? form) (get pred->type form 'typed.clojure/Any)
     (set? form) (cons 'typed.clojure/U (map (fn [v] (list 'typed.clojure/Val v)) form))
     (seq? form)
     (let [[op & args] form]
       (condp = op
         `s/keys (keys->type args seen)
         `s/nilable (list 'typed.clojure/Nilable (spec->type (first args) seen))
         `s/or (cons 'typed.clojure/U (map #(spec->type % seen) (take-nth 2 (rest args))))
         `s/and (spec->type (first args) seen)            ;; refinements are runtime-only
         `s/coll-of (let [[p & {:keys [kind]}] args
                          t (spec->type p seen)]
                      (if (= kind `vector?)
                        (list 'typed.clojure/Vec t)
                        (list 'typed.clojure/Seqable t)))
         `s/every (list 'typed.clojure/Seqable (spec->type (first args) seen))
         `s/map-of (list 'typed.clojure/Map (spec->type (first args) seen) (spec->type (second args) seen))
         `s/tuple (vec (cons 'typed.clojure/HVec [(mapv #(spec->type % seen) args)]))
         `s/merge (let [ts (map #(spec->type % seen) args)]
                    (if (every? #(and (seq? %) (= 'typed.clojure/HMap (first %))) ts)
                      (let [ms (map #(apply hash-map (rest %)) ts)]
                        (list 'typed.clojure/HMap
                              :mandatory (apply merge (map :mandatory ms))
                              :optional (apply merge (map :optional ms))
                              :complete? (closed?)))
                      'typed.clojure/Any))
         `s/fspec (let [{:keys [args ret]} (apply hash-map args)
                        argv (regex->argv args seen)]
                    (if argv
                      (conj argv :-> (if ret (spec->type ret seen) 'typed.clojure/Any))
                      'typed.clojure/AnyFunction))
         'typed.clojure/Any))
     :else 'typed.clojure/Any)))

(defn var-type
  "Var type provider (see typedclojure_config :var-type-providers)."
  [var-qsym opts]
  (when (qualified-symbol? var-qsym)
    (when-let [sp (s/get-spec var-qsym)]
      (let [form (s/form sp)
            tsyn (spec->type form)]
        (when (vector? tsyn)
          ((requiring-resolve 'typed.clj.checker.parse-unparse/parse-type)
           tsyn
           (assoc opts :typed.clj.checker.parse-unparse/parse-type-in-ns (ns-name *ns*))))))))
