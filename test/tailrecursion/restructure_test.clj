(ns tailrecursion.restructure-test
  (:require [clojure.test :refer :all]
            [tailrecursion.restructure :refer [over compile-over over-plan explain]]
            [clojure.string :as str]))

(deftest readme-example-1
  (let [data {:a [{:aa 1 :bb 2} {:cc 3}] :b [{:dd 4}]}
        result (over [{_ [{_ n}]} data]
                 {n (cond-> n (even? n) inc)})]
    (is (= {:a [{:aa 1 :bb 3} {:cc 3}] :b [{:dd 5}]} result))))

(deftest readme-example-2
  (let [users
        {:alice {:active true  :email "ALICE@EXAMPLE.COM"}
         :bob   {:active false :email "bob@example.com"}
         :cara  {:active true  :email "CARA@EXAMPLE.COM"}}
        out (over [{_ {:keys [active email] :as u}} users]
              {u?    active
               email (str/lower-case email)})]
    (is (= {:alice {:active true :email "alice@example.com"}
            :cara  {:active true :email "cara@example.com"}}
           out))))

(deftest readme-example-3
  (let [order
        {:id 42
         :lines [{:sku "A" :qty 2}
                 {:sku ""  :qty 1}
                 {:sku "B" :qty 0}]}
        out (over [{:keys [lines]} order
                   [{:keys [sku qty] :as line}] lines]
              {line? (seq sku)
               qty   (or qty 0)})]
    (is (= {:id 42
            :lines [{:sku "A" :qty 2}
                    {:sku "B" :qty 0}]}
           out))))

(deftest example-2
  (let [users
        {:alice {:active true  :email "ALICE@EXAMPLE.COM" :phones ["111" "" "222"] :tags #{:keep :old}}
         :bob   {:active false :email "bob@example.com"   :phones ["333"]          :tags #{:keep}}
         :cara  {:active true  :email "CARA@EXAMPLE.COM"  :phones [""]             :tags #{:keep}}}
        cleaned
        (over [{_ {:keys [active tags phones email] :as v}} users
               [t] tags
               [p] phones]
          {v?    (and active (tags :keep))
           email (str/lower-case email)
           t?    (not= t :old)
           p?    (seq p)})]
    (is (= {:alice {:active true :email "alice@example.com" :phones ["111" "222"] :tags #{:keep}}
            :cara  {:active true :email "cara@example.com"  :phones []            :tags #{:keep}}}
           cleaned))))

(deftest seq-elision
  (let [data [1 2 3 4]
        result (over [[n] data]
                 {n? (odd? n)})]
    (is (= [1 3] result))))

(deftest map-entry-elision
  (let [data {:a 1 :b 2 :c 3}
        result (over [{k v} data]
                 {v? (odd? v)})]
    (is (= {:a 1 :c 3} result))))

(deftest map-destructure-rewrite
  (let [data {:u {:a 1 :b 2}}
        result (over [{_ {:keys [a b] :as m}} data]
                 {a (inc a)
                  b (dec b)
                  m? (pos? a)})]
    (is (= {:u {:a 2 :b 1}} result))))

(deftest or-defaults-and-rewrite
  (let [data {:u {:a 1}}
        out (over [{_ {:keys [b] :or {b 10}}} data]
              {b b})]
    (is (= {:u {:a 1 :b 10}} out)))
  (let [data {:u {:a 1}}
        out (over [{_ {:keys [b] :or {b 10}}} data]
              {})]
    (is (= {:u {:a 1}} out))))

(deftest as-update-idiom
  (let [data {:u {:x 1 :y 2}}
        out (over [{_ {:as m}} data]
              {m (-> m (update :x inc) (dissoc :y))})]
    (is (= {:u {:x 2}} out))))

(deftest type-errors
  (is (thrown-with-msg?
        clojure.lang.ExceptionInfo
        #"Pattern type mismatch"
        (over [[n] {:a 1}]
          {n (inc n)})))
  (is (thrown-with-msg?
        clojure.lang.ExceptionInfo
        #"Pattern type mismatch"
        (over [{k v} [1 2 3]]
          {v (inc v)}))))

(deftest structural-sharing
  (let [data {:a 1 :b 2}
        result (over [{k v} data]
                 {})]
    (is (identical? data result))))

(deftest type-preservation
  (let [data [1 2 3]
        out (over [[n] data]
              {n (inc n)})]
    (is (vector? out))
    (is (= [2 3 4] out)))
  (let [data #{1 2 3}
        out (over [[n] data]
              {n? (odd? n)})]
    (is (set? out))
    (is (= #{1 3} out))))

(deftest compile-over-fn
  (let [f (compile-over [{_ n} ::input]
            {n (inc n)})]
    (is (= {:x 2} (f {:x 1})))))

(deftest plan-introspection
  (let [plan (over-plan '[{_ n} ::input] '{n (inc n)})]
    (is (map? plan))
    (is (contains? plan :bindings)))
  (let [plan (explain '[{_ n} ::input] '{n (inc n)})]
    (is (map? plan))))

(deftest invalid-body
  (is (thrown? clojure.lang.ExceptionInfo
        (explain '[{_ n} ::input] '{x (inc 1)}))))

(deftest selector-errors
  (letfn [(exdata [f]
            (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (is (= :parse (:phase (exdata #(explain '{_ n} '{n n})))) )
    (is (= :parse (:phase (exdata #(explain '[{_ n} ::input _] '{n n})))) )
    (is (= :parse (:phase (exdata #(explain '[[a b] ::input] '{a a})))) )
    (is (= :parse (:phase (exdata #(explain '[{{a b} ::input}] '{a a})))) )
    (is (= :parse (:phase (exdata #(explain '[{[a] b} ::input] '{b b})))) )
    (is (= :validate (:phase (exdata #(explain '[{_ n} x {y z} w] '{n n})))) )))

(deftest destructure-errors
  (letfn [(exdata [f]
            (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (is (= :parse (:phase (exdata #(explain '[{_ {:keys a}} ::input] '{a a})))) )
    (is (= :parse (:phase (exdata #(explain '[{_ {:keys [a 1]}} ::input] '{a a})))) )
    (is (= :parse (:phase (exdata #(explain '[{_ {:as 1}} ::input] '{a a})))) )
    (is (= :parse (:phase (exdata #(explain '[{_ {:or 1}} ::input] '{a a})))) )
    (is (= :parse (:phase (exdata #(explain '[{_ {:foo 1}} ::input] '{a a})))) )))

(deftest body-errors
  (letfn [(exdata [f]
            (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (is (= :validate (:phase (exdata #(explain '[{_ n} ::input] '(n)))) ))
    (is (= :validate (:phase (exdata #(explain '[{_ n} ::input] '{:k 1})))) )
    (is (= :validate (:phase (exdata #(explain '[{_ n} ::input] '{a/b 1})))) )
    (is (= :validate (:phase (exdata #(explain '[{_ n} ::input] '{m? true})))) )
    (is (= :validate (:phase (exdata #(explain '[{_ n} ::input] '{m 1})))) )))

(deftest binding-errors
  (letfn [(exdata [f]
            (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (is (= :validate (:phase (exdata #(explain '[{k k} ::input] '{k k})))) )
    (is (= :validate (:phase (exdata #(explain '[{_ n} ::input {n m} n] '{m m})))) )))

(deftest nested-destructure-errors-and-workaround
  (letfn [(exdata [f]
            (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (is (= :parse (:phase (exdata #(explain '[{_ {:keys [{:keys [email]}]}} ::input]
                                       '{email email})))) ))
  (let [users {:alice {:profile {:email "A@EXAMPLE.COM"}}
               :bob   {:profile {:email "B@EXAMPLE.COM"}}}
        cleaned (over [{_ {:keys [profile]}} users
                       {:keys [email]} profile]
                  {email (str/lower-case email)})]
    (is (= {:alice {:profile {:email "a@example.com"}}
            :bob   {:profile {:email "b@example.com"}}}
           cleaned))))

(deftest strs-destructure
  (let [data {"a" 1 "b" 2}
        out (over [{:strs [a]} data]
              {a (inc a)})]
    (is (= {"a" 2 "b" 2} out)))
  (let [data {"a" 1 "b" 2}
        out (over [{:strs [a]} data]
              {a? false})]
    (is (= {"b" 2} out))))

(deftest strs-destructure-errors
  (letfn [(exdata [f]
            (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (is (= :parse (:phase (exdata #(explain '[{_ {:strs a}} ::input] '{a a})))) )
    (is (= :parse (:phase (exdata #(explain '[{_ {:strs [a 1]}} ::input] '{a a})))) )))

(deftest as-guard-elides-entry
  (let [data {:a {:x 1} :b {:x 0}}
        out (over [{_ {:keys [x] :as v}} data]
              {v? (pos? x)})]
    (is (= {:a {:x 1}} out))))

(deftest key-guard-elides-key
  (let [data {:a {:x 1 :y 2}}
        out (over [{_ {:keys [x y]}} data]
              {y? (odd? y)})]
    (is (= {:a {:x 1}} out))))

(deftest key-rewrite
  (let [data {:a 1 :b 2}
        out (over [{k v} data]
              {k (keyword (str (name k) "*"))})]
    (is (= {:a* 1 :b* 2} out))))

(deftest multi-step-scope
  (let [data {:a {:xs [1 2]} :b {:xs [3]}}
        out (over [{_ {:keys [xs]}} data
                   [n] xs]
              {n (inc n)})]
    (is (= {:a {:xs [2 3]} :b {:xs [4]}} out))))

(deftest post-order-semantics
  (let [data {:a {:n 0}}
        out (over [{_ {:keys [n] :as v}} data]
              {n (inc n)
               v? (pos? n)})]
    (is (= {:a {:n 1}} out))))

(deftest keyword-and-set-as-fn-idioms
  (let [data {:a {:tag :keep} :b {:tag :drop}}
        out (over [{_ {:keys [tag] :as v}} data]
              {v? (= tag :keep)})]
    (is (= {:a {:tag :keep}} out)))
  (let [data {:a {:tags #{:keep}} :b {:tags #{}}}
        out (over [{_ {:keys [tags] :as v}} data]
              {v? (tags :keep)})]
    (is (= {:a {:tags #{:keep}}} out))))

(deftest get-in-idiom
  (let [data {:a {:profile {:email "A@EXAMPLE.COM"}}}
        out (over [{_ {:as v}} data]
              {v (assoc v :domain (second (re-find #"@(.+)" (get-in v [:profile :email]))))})]
    (is (= {:a {:profile {:email "A@EXAMPLE.COM"} :domain "EXAMPLE.COM"}} out))))

(deftest sharing-unchanged-branches
  (let [data {:a {:x 1} :b {:y 2}}
        out (over [{_ {:keys [x]}} data]
              {x? (some? x)
               x  (when x (inc x))})]
    (is (= {:a {:x 2} :b {:y 2}} out))
    (is (identical? (:b data) (:b out)))
    (is (not (identical? (:a data) (:a out))))))

(deftest meta-preservation
  (let [data (with-meta {:a 1} {:m 1})
        out (over [{k v} data]
              {v (inc v)})]
    (is (= {:a 2} out))
    (is (= (meta data) (meta out)))))

(deftest nil-inputs
  (is (nil? (over [[n] nil] {n (inc n)})))
  (is (nil? (over [{k v} nil] {v (inc v)})))
  (is (nil? (over [{_ {:keys [a]}} nil] {a (inc a)}))))

(deftest list-and-seq-preservation
  (let [data (list 1 2 3)
        out (over [[n] data] {n (inc n)})]
    (is (list? out))
    (is (= '(2 3 4) out)))
  (let [data (range 3)
        out (over [[n] data] {})]
    (is (seq? out))
    (is (identical? data out))
    (is (= '(0 1 2) out))))

(deftest seq-coercion-on-change
  (let [data (range 3)
        out (over [[n] data] {n (inc n)})]
    (is (seq? out))
    (is (not (list? out)))
    (is (= '(1 2 3) out))))

(deftest seqable-errors
  (letfn [(exdata [f]
            (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (let [data (exdata #(over [[n] "abc"] {n n}))]
      (is (= :runtime (:phase data)))
      (is (= :seqable (:expected data))))))

(deftest conflict-errors
  (letfn [(exdata [f]
            (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (is (= :validate (:phase (exdata #(explain '[{_ {:keys [a] :as m}} ::input]
                                       '{a (inc a) m (assoc m :a 0)})))) )))

(deftest root-elision
  (is (= tailrecursion.restructure/elide
         (over [v {:a 1}] {v? false}))))

(deftest key-collision
  (let [data (array-map :a 1 :b 2)
        out (over [{k v} data]
              {k :a
               v (inc v)})]
    (is (= {:a 3} out))))

(deftest record-and-sorted-map
  (defrecord R [a b])
  (let [r (->R 1 2)
        out (over [{k v} r]
              {v (inc v)})]
    (is (= {:a 2 :b 3} out))
    (is (not (record? out))))
  (let [m (sorted-map :b 2 :a 1)
        out (over [{k v} m]
              {v (inc v)})]
    (is (sorted? out))
    (is (= {:a 2 :b 3} out))))

(deftest nested-meta-preservation
  (let [inner (with-meta {:x 1} {:m 1})
        data {:a inner}
        out (over [{_ {:keys [x] :as v}} data]
              {x (inc x)})]
    (is (= {:a {:x 2}} out))
    (is (= {:m 1} (meta (:a out))))))
