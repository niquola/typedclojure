(ns ^:typed.clojure clojure.core.typed.test.ts-features2-test
  "TypeScript-inspired: literal did-you-mean, Pick/Omit/Partial/Required, exhaustive case."
  (:require [typed.clj.checker.test-utils :refer [is-tc-e tc-err]]
            [clojure.test :refer [deftest is testing]]))

(defn- messages [res] (map :message (:type-errors res)))
(defn- has-msg? [res re] (boolean (some #(re-find re %) (messages res))))

(deftest literal-did-you-mean
  (let [res (tc-err (t/ann-form :gte (t/U ':get ':post)))]
    (is (has-msg? res #"Did you mean :get\?") (pr-str (messages res))))
  (let [res (tc-err (t/ann-form "GET" (t/U ':get ':post)))]
    (is (has-msg? res #"Did you mean :get\?") (pr-str (messages res))))
  (let [res (tc-err (let [f (t/ann-form (fn [_] 1) [(t/HMap :mandatory {:method (t/U ':get ':post)} :complete? true) :-> t/Int])]
                      (f {:method :pots})))]
    (is (has-msg? res #"Did you mean :post\?") (pr-str (messages res)))))

(deftest mapped-types
  (testing "Pick keeps listed keys"
    (is-tc-e {:id 1} (t/Pick (t/HMap :mandatory {:id t/Int :name t/Str} :complete? true) :id))
    (is (tc-err {:id 1 :name "x"} (t/Pick (t/HMap :mandatory {:id t/Int :name t/Str} :complete? true) :id))))
  (testing "Omit drops keys"
    (is-tc-e {:name "x"} (t/Omit (t/HMap :mandatory {:id t/Int :name t/Str} :complete? true) :id))
    (is (tc-err {:name "x" :id 1} (t/Omit (t/HMap :mandatory {:id t/Int :name t/Str} :complete? true) :id))))
  (testing "Partial makes all keys optional"
    (is-tc-e {} (t/Partial (t/HMap :mandatory {:id t/Int :name t/Str} :complete? true)))
    (is-tc-e {:id 1} (t/Partial (t/HMap :mandatory {:id t/Int :name t/Str} :complete? true)))
    (is (tc-err {:id "1"} (t/Partial (t/HMap :mandatory {:id t/Int :name t/Str} :complete? true)))))
  (testing "Required makes optional keys mandatory"
    (is (tc-err {:id 1} (t/Required (t/HMap :mandatory {:id t/Int} :optional {:name t/Str} :complete? true))))
    (is-tc-e {:id 1 :name "x"} (t/Required (t/HMap :mandatory {:id t/Int} :optional {:name t/Str} :complete? true)))))

(deftest exhaustive-case
  (testing "missing literal is reported"
    (let [res (tc-err (fn [m :- (t/U ':get ':post ':delete)] :- t/Int
                        (case m :get 1 :post 2)))]
      (is (has-msg? res #"case is not exhaustive: no clause for :delete") (pr-str (messages res)))))
  (testing "full coverage and explicit default are fine"
    (is-tc-e (fn [m :- (t/U ':get ':post)] :- t/Int (case m :get 1 :post 2)))
    (is-tc-e (fn [m :- (t/U ':get ':post ':delete)] :- t/Int (case m :get 1 0)))
    (is-tc-e (fn [m :- t/Kw] :- t/Int (case m :get 1 :post 2)))))
