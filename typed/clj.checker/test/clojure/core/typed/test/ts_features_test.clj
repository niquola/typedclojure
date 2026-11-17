(ns ^:typed.clojure clojure.core.typed.test.ts-features-test
  "TypeScript-inspired diagnostics: unknown property on complete HMaps, and
  'Why:' explanations for argument mismatches (excess/missing/wrong-typed keys)."
  (:require [typed.clj.checker.test-utils :refer [is-tc-e is-tc-err tc-err]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- messages [res] (map :message (:type-errors res)))
(defn- has-msg? [res re] (boolean (some #(re-find re %) (messages res))))

(deftest unknown-property-on-complete-hmap
  (testing "typo in key of a complete HMap is an error with a suggestion"
    (let [res (tc-err (fn [p :- (t/HMap :mandatory {:id t/Int :birth_date t/Str} :complete? true)]
                        (:birthDate p)))]
      (is (has-msg? res #"Property :birthDate does not exist") (pr-str (messages res)))
      (is (has-msg? res #"Did you mean :birth_date\?"))))
  (testing "declared, optional and partial-map lookups stay legal"
    (is-tc-e (fn [p :- (t/HMap :mandatory {:id t/Int} :complete? true)] (:id p)))
    (is-tc-e (fn [p :- (t/HMap :optional {:x t/Int} :complete? true)] (:x p)))
    (is-tc-e (fn [p :- (t/HMap :mandatory {:id t/Int})] (:anything p)))
    (is-tc-e (fn [p :- (t/Nilable (t/HMap :mandatory {:id t/Int} :complete? true))] (:id p))))
  (testing "explicit default opts out"
    (is-tc-e (fn [p :- (t/HMap :mandatory {:id t/Int} :complete? true)] (:other p 1)))))

(deftest why-explanations
  (testing "excess property with suggestion"
    (let [res (tc-err (let [f (t/ann-form (fn [_] 1) [(t/HMap :mandatory {:table t/Str} :optional {:limit t/Int} :complete? true) :-> t/Int])]
                        (f {:table "x" :limt 1})))]
      (is (has-msg? res #":limt does not exist in type") (pr-str (messages res)))
      (is (has-msg? res #"Did you mean :limit\?"))))
  (testing "wrong property type"
    (let [res (tc-err (let [f (t/ann-form (fn [_] 1) [(t/HMap :mandatory {:method (t/U ':get ':post)} :complete? true) :-> t/Int])]
                        (f {:method "get"})))]
      (is (has-msg? res #"Type of property :method is") (pr-str (messages res))))))
