;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.ts-hints
  "TypeScript-style 'Did you mean ...?' hints for keys and literal values."
  (:require [clojure.string :as str]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]))

(defn edit-distance [a b]
  (let [a (vec a) b (vec b) n (count b)]
    (peek (reduce (fn [prev [i ca]]
                    (reduce (fn [row [j cb]]
                              (conj row (min (inc (peek row))
                                             (inc (nth prev (inc j)))
                                             (+ (nth prev j) (if (= ca cb) 0 1)))))
                            [(inc i)] (map-indexed vector b)))
                  (vec (range (inc n))) (map-indexed vector a)))))

(defn- norm [x]
  (-> (if (or (keyword? x) (symbol? x)) (name x) (str x))
      str/lower-case
      (str/replace #"[-_]" "")))

(defn closest
  "Closest candidate to x by normalized edit distance (case, - and _ ignored), or nil."
  [x candidates]
  (let [nx (norm x)]
    (some->> candidates
             (map (fn [c] [c (edit-distance nx (norm c))]))
             (filter (fn [[c d]] (<= d (max 2 (quot (count (norm c)) 3)))))
             (sort-by second)
             ffirst)))

(defn literal-values
  "If t (resolved) is a union of singleton Val types, their values; else nil."
  [t opts]
  (let [t (c/fully-resolve-type t opts)
        ms (if (r/Union? t) (:types t) [t])
        ms (map #(c/fully-resolve-type % opts) ms)]
    (when (and (seq ms) (every? r/Value? ms))
      (map :val ms))))

(defn literal-hint
  "' Did you mean :get?' when actual is a literal close to one of expected's literals."
  [actual expected opts]
  (let [a (c/fully-resolve-type actual opts)]
    (when (r/Value? a)
      (when-let [vs (literal-values expected opts)]
        (when-let [h (closest (:val a) (remove #{(:val a)} vs))]
          (str " Did you mean " (pr-str h) "?"))))))
