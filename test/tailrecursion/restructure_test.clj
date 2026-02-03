(ns tailrecursion.restructure-test
  (:require [clojure.test :refer [deftest is]]
            [tailrecursion.restructure :refer
             [over over-> over->> compile-over over-plan]]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defmacro thrown? [cls & body] `(try ~@body false (catch ~cls _# true)))

(defmacro thrown-with-msg?
  [cls re & body]
  `(try ~@body false (catch ~cls e# (boolean (re-find ~re (.getMessage e#))))))

(defn- exdata [f] (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- form-tags
  [form sym-name]
  (let [tags (atom #{})]
    (walk/postwalk (fn [x]
                     (when (and (symbol? x) (= (name x) sym-name))
                       (swap! tags conj (:tag (meta x))))
                     x)
                   form)
    (disj @tags nil)))

(deftest readme-example-1
  (let [data {:a [{:aa 1, :bb 2} {:cc 3}], :b [{:dd 4}]}
        result (over [{_ [{_ n}]} data] {n (cond-> n (even? n) inc)})]
    (is (= {:a [{:aa 1, :bb 3} {:cc 3}], :b [{:dd 5}]} result))))

(deftest threading-helpers
  (let [data {:a [{:aa 1, :bb 2} {:cc 3}], :b [{:dd 4}]}
        out (-> data
                (over-> [{_ [{_ n}]}] {n (cond-> n (even? n) inc)})
                (assoc :touched true))]
    (is (= {:a [{:aa 1, :bb 3} {:cc 3}], :b [{:dd 5}], :touched true} out)))
  (let [data [1 2 3 4]
        out (->> data
                 (over->> [[n]] {n? (even? n), n n})
                 (map inc)
                 (into []))]
    (is (= [3 5] out))))

(deftest threading-helpers-multi-step
  (let [data {:a [{:x 1} {:x 2}], :b [{:x 3}]}
        out (-> data
                (over-> [{_ [{:keys [x]}]}] {x (inc x)})
                (over-> [{_ [{:keys [x]}]}] {x (* x 2)})
                (assoc :done true))]
    (is (= {:a [{:x 4} {:x 6}], :b [{:x 8}], :done true} out)))
  (let [data [1 2 3 4]
        out (->> data
                 (over->> [[n]] {n? (odd? n), n (* n 2)})
                 (map inc)
                 (over->> [[n]] {n? (>= n 5), n (dec n)})
                 (into []))]
    (is (= [6] out))))

(deftest threading-helpers-multiple-sources
  (let [data {:a {:x 1}, :b {:x 2}}
        out (-> data
                (over-> [{:keys [a]} {:keys [x]} a] {x (inc x)}))]
    (is (= {:a {:x 2}, :b {:x 2}} out)))
  (let [data [[1 2] [3]]
        out (->> data
                 (over->> [[m] [n] m] {n (inc n)})
                 (into []))]
    (is (= [[2 3] [4]] out))))

(deftest seq-destructure
  (let [data [[:x 1] [:y 2]]
        out (over [(seq [k v]) data] {v (inc v)})]
    (is (= [[:x 2] [:y 3]] out)))
  (let [data [[1 2 3] [4 5 6]]
        out (over [(seq [a b & more]) data] {b (inc b)})]
    (is (= [[1 3 3] [4 6 6]] out)))
  (let [data [[1 2 3] [4 5 6]]
        out (over [(seq [a b & more]) data] {more (mapv inc more)})]
    (is (= [[1 2 4] [4 5 7]] out)))
  (let [data [[1 2] [3 5]]
        out (over [(seq [a b]) data] {b? (odd? b), b b})]
    (is (= [[3 5]] out)))
  (let [data [[1 2 3] [4 5]]
        out (over [(seq [a b & more :as all]) data]
                  {all (vec (concat [b a] more))})]
    (is (= [[2 1 3] [5 4]] out)))
  (let [data [[1] [1 2] [1 2 3]]
        out (over [(seq [a & more]) data] {more? (seq more), a a})]
    (is (= [[1 2] [1 2 3]] out)))
  (let [data [[1]]
        out (over [(seq [a b]) data] {b (or b 0)})]
    (is (= [[1 0]] out)))
  (let [data []
        out (over [(seq [a b]) data] {a a})]
    (is (= [] out))
    (is (identical? data out))))

(deftest readme-example-2
  (let [users {:alice {:active true, :email "ALICE@EXAMPLE.COM"},
               :bob {:active false, :email "bob@example.com"},
               :cara {:active true, :email "CARA@EXAMPLE.COM"}}
        out (over [{_ {:keys [active email], :as u}} users]
                  {u? active, email (str/lower-case email)})]
    (is (= {:alice {:active true, :email "alice@example.com"},
            :cara {:active true, :email "cara@example.com"}}
           out))))

(deftest readme-example-3
  (let [order {:id 42,
               :lines [{:sku "A", :qty 2} {:sku "", :qty 1} {:sku "B", :qty 0}]}
        out (over [{:keys [lines]} order [{:keys [sku qty], :as line}] lines]
                  {line? (seq sku), qty (or qty 0)})]
    (is (= {:id 42, :lines [{:sku "A", :qty 2} {:sku "B", :qty 0}]} out))))

(deftest selector-type-hints
  (let [form (macroexpand '(compile-over [^long n ::input] {n (inc n)}))]
    (is (contains? (form-tags form "n") 'long)))
  (let [form (macroexpand
               '(compile-over [{:keys [^String name]} ::input] {name name}))]
    (is (some #{'String String java.lang.String} (form-tags form "name"))))
  (let [form (macroexpand
               '(compile-over [{:keys [^long n], :or {n 1}} ::input] {n n}))]
    (is (contains? (form-tags form "n") 'long)))
  (let [form (macroexpand '(compile-over [{^long id :id} ::input] {id id}))]
    (is (contains? (form-tags form "id") 'long)))
  (let [form (macroexpand
               '(compile-over [{^String title "title"} ::input] {title title}))]
    (is (some #{'String String java.lang.String} (form-tags form "title"))))
  (let [form (macroexpand
               '(compile-over [{:strs [^String title]} ::input] {title title}))]
    (is (some #{'String String java.lang.String} (form-tags form "title"))))
  (let [form (macroexpand '(compile-over
                            [{:as ^String m, :keys [^long x]} ::input]
                            {m m, x x}))]
    (is (contains? (form-tags form "x") 'long))
    (is (some #{'String String java.lang.String} (form-tags form "m"))))
  (let [form (macroexpand '(compile-over [{^String k v} ::input] {k k, v v}))]
    (is (some #{'String String java.lang.String} (form-tags form "k"))))
  (let [form (macroexpand '(compile-over [{k ^long v} ::input] {k k, v v}))]
    (is (contains? (form-tags form "v") 'long)))
  (let [form (macroexpand
               '(compile-over [{^String k ^long v} ::input] {k k, v v}))]
    (is (some #{'String String java.lang.String} (form-tags form "k")))
    (is (contains? (form-tags form "v") 'long)))
  (let [form (macroexpand '(compile-over [{_ ^long v} ::input] {v v}))]
    (is (contains? (form-tags form "v") 'long))
    (is (empty? (form-tags form "_"))))
  (let [form
          (macroexpand
            '(compile-over [^String s ::input {:keys [^long n]} s] {n n, s s}))]
    (is (some #{'String String java.lang.String} (form-tags form "s")))
    (is (contains? (form-tags form "n") 'long)))
  (let [form (macroexpand
               '(compile-over [{:keys [^long x]} ::input] {x? (pos? x)}))]
    (is (contains? (form-tags form "x") 'long))
    (is (empty? (form-tags form "x?"))))
  (let [form (macroexpand
               '(compile-over [(seq [^long a ^String b]) ::input] {a a, b b}))]
    (is (contains? (form-tags form "a") 'long))
    (is (some #{'String String java.lang.String} (form-tags form "b")))))

(deftest identity-when-unchanged
  (let [data {:a ["x" "y"], :b {:c "z"}}
        out (over [{:keys [a b]} data [s] a] {s (str s)})]
    (is (identical? data out)))
  (let [data {:a "x"}
        out (over [{k v} data] {v (str v)})]
    (is (identical? data out)))
  (let [data {:a [{:x "x"}]}
        out (over [{_ [{:keys [x]}]} data] {x (str x)})]
    (is (identical? data out)))
  (let [data {:a #{"x" "y"}}
        out (over [{:keys [a]} data [s] a] {s (str s)})]
    (is (identical? data out))))

(deftest identity-when-meta-changes
  (let [data (with-meta {:x 1} {:m 1})
        out (over [m data] {m (with-meta m {:m 2})})]
    (is (= {:x 1} out))
    (is (= {:m 2} (meta out)))
    (is (not (identical? data out)))))

(deftest identity-when-nested-meta-changes
  (let [inner (with-meta {:x 1} {:m 1})
        data {:a inner}
        out (over [{_ v} data] {v (with-meta v {:m 2})})]
    (is (= {:a {:x 1}} out))
    (is (= {:m 2} (meta (:a out))))
    (is (not (identical? data out)))))

(deftest guard-var-capture
  (let [data {:x 1}
        out (over [{:keys [x]} data]
                  {x? (pos? x), x (let [x? :shadow] [x x?])})]
    (is (= {:x [1 :shadow]} out))))

(deftest example-2
  (let [users {:alice {:active true,
                       :email "ALICE@EXAMPLE.COM",
                       :phones ["111" "" "222"],
                       :tags #{:keep :old}},
               :bob {:active false,
                     :email "bob@example.com",
                     :phones ["333"],
                     :tags #{:keep}},
               :cara {:active true,
                      :email "CARA@EXAMPLE.COM",
                      :phones [""],
                      :tags #{:keep}}}
        cleaned (over [{_ {:keys [active tags phones email], :as v}} users [t]
                       tags [p] phones]
                      {v? (and active (tags :keep)),
                       email (str/lower-case email),
                       t? (not= t :old),
                       p? (seq p)})]
    (is (= {:alice {:active true,
                    :email "alice@example.com",
                    :phones ["111" "222"],
                    :tags #{:keep}},
            :cara {:active true,
                   :email "cara@example.com",
                   :phones [],
                   :tags #{:keep}}}
           cleaned))))

(deftest seq-elision
  (let [data [1 2 3 4]
        result (over [[n] data] {n? (odd? n)})]
    (is (= [1 3] result))))

(deftest map-entry-elision
  (let [data {:a 1, :b 2, :c 3}
        result (over [{k v} data] {v? (odd? v)})]
    (is (= {:a 1, :c 3} result))))

(deftest map-destructure-rewrite
  (let [data {:u {:a 1, :b 2}}
        result (over [{_ {:keys [a b], :as m}} data]
                     {a (inc a), b (dec b), m? (pos? a)})]
    (is (= {:u {:a 2, :b 1}} result))))

(deftest or-defaults-and-rewrite
  (let [data {:u {:a 1}}
        out (over [{_ {:keys [b], :or {b 10}}} data] {b b})]
    (is (= {:u {:a 1, :b 10}} out)))
  (let [data {:u {:a 1}}
        out (over [{_ {:keys [b], :or {b 10}}} data] {})]
    (is (= {:u {:a 1}} out))))

(deftest or-defaults-guard-false
  (let [data {:u {}}
        out (over [{_ {:keys [b], :or {b 10}}} data] {b? false})]
    (is (= {:u {}} out))
    (is (identical? data out))))

(deftest as-update-idiom
  (let [data {:u {:x 1, :y 2}}
        out (over [{_ {:as m}} data]
                  {m (-> m
                         (update :x inc)
                         (dissoc :y))})]
    (is (= {:u {:x 2}} out))))

(deftest type-errors
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"Pattern type mismatch"
                        (over [[n] {:a 1}] {n (inc n)})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"Pattern type mismatch"
                        (over [{k v} [1 2 3]] {v (inc v)}))))

(deftest error-shapes
  (let [data (try (over [[n] {:a 1}] {n (inc n)})
                  (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    (is (= :runtime (:phase data)))
    (is (= :seqable (:expected data)))
    (is (contains? data :path))
    (is (contains? data :value))))

(deftest error-message-with-hint
  (is (thrown-with-msg?
        clojure.lang.ExceptionInfo
        #"Pattern type mismatch(?s).*hint: Expected a map pattern"
        (over [{:keys [lines]} {:lines [{:sku "A"}]} {line-sku :sku} lines]
              {line-sku line-sku}))))

(deftest error-message-with-seqable-hint
  (is (thrown-with-msg?
        clojure.lang.ExceptionInfo
        #"Pattern type mismatch(?s).*hint: Expected a seq pattern"
        (over [[n] {:a 1}] {n n}))))

(deftest error-message-no-bracketed-hint
  (try (over [{:keys [lines]} {:lines [{:sku "A"}]} {line-sku :sku} lines]
             {line-sku line-sku})
       (is false)
       (catch clojure.lang.ExceptionInfo e
         (let [m (.getMessage e)]
           (is (re-find #"hint: Expected a map pattern" m))
           (is (nil? (re-find (re-pattern "hint: \\[") m)))))))

(deftest error-message-no-hint-for-non-type-errors
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"(?s)^((?!hint:).)*$"
                        (over-plan '[{_ n} ::input] '{m 1}))))

(deftest structural-sharing
  (let [data {:a 1, :b 2}
        result (over [{k v} data] {})]
    (is (identical? data result))))

(deftest map-entry-identity-on-no-change
  (let [data (with-meta {:a 1, :b 2} {:m 1})
        out (over [{k v} data] {k k, v v})]
    (is (identical? data out))))

(deftest type-preservation
  (let [data [1 2 3]
        out (over [[n] data] {n (inc n)})]
    (is (vector? out))
    (is (= [2 3 4] out)))
  (let [data #{1 2 3}
        out (over [[n] data] {n? (odd? n)})]
    (is (set? out))
    (is (= #{1 3} out))))

(deftest compile-over-fn
  (let [f (compile-over [{_ n} ::input] {n (inc n)})]
    (is (= {:x 2} (f {:x 1})))))

(deftest plan-introspection
  (let [plan (over-plan '[{_ n} ::input] '{n (inc n)})]
    (is (map? plan))
    (is (contains? plan :bindings))))

(deftest invalid-body
  (is (thrown? clojure.lang.ExceptionInfo
               (over-plan '[{_ n} ::input] '{x (inc 1)}))))

(deftest selector-errors
  (letfn [(exdata [f]
            (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (is (= :parse (:phase (exdata #(over-plan '{_ n} '{n n})))))
    (is (= :parse (:phase (exdata #(over-plan '[{_ n} ::input _] '{n n})))))
    (is (= :parse (:phase (exdata #(over-plan '[[a b] ::input] '{a a})))))
    (is (= :parse (:phase (exdata #(over-plan '[{{a b} ::input}] '{a a})))))
    (is (= :parse (:phase (exdata #(over-plan '[{[a] b} ::input] '{b b})))))
    (is (thrown? clojure.lang.Compiler$CompilerException
                 (eval '(over ::input [{_ n} ::input] {n n}))))
    (is (= :validate
           (:phase (exdata #(over-plan '[{_ n} x {y z} w] '{n n})))))))

(deftest destructure-errors
  (letfn [(exdata [f]
            (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (is (= :parse
           (:phase (exdata #(over-plan '[{_ {:keys a}} ::input] '{a a})))))
    (is (= :parse
           (:phase (exdata #(over-plan '[{_ {:keys [a 1]}} ::input] '{a a})))))
    (is (= :parse (:phase (exdata #(over-plan '[{_ {:as 1}} ::input] '{a a})))))
    (is (= :parse (:phase (exdata #(over-plan '[{_ {:or 1}} ::input] '{a a})))))
    (is (= :parse
           (:phase (exdata #(over-plan '[{_ {:foo 1}} ::input] '{a a})))))
    (is (= :parse
           (:phase (exdata #(over-plan '[{_ {:keys [a], :b :b}} ::input]
                                       '{a a})))))
    (is (= :parse
           (:phase (exdata #(over-plan '[{_ {:keys [a], b 1}} ::input]
                                       '{a a})))))))

(deftest destructure-renamed-keys
  (let [user {:id 7, :name "Ana"}
        out (over [{:keys [name], user-id :id} user] {user-id (inc user-id)})]
    (is (= {:id 8, :name "Ana"} out)))
  (let [data {"id" 7, "name" "Ana"}
        out (over [{user-id "id"} data] {user-id (inc user-id)})]
    (is (= {"id" 8, "name" "Ana"} out)))
  (let [data {:id 7, :name "Ana"}
        out (over [{:keys [name], user-id :id, :as m} data] {m? (seq name)})]
    (is (= {:id 7, :name "Ana"} out))))

(deftest body-errors
  (letfn [(exdata [f]
            (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (is (= :validate (:phase (exdata #(over-plan '[{_ n} ::input] '(n))))))
    (is (= :validate (:phase (exdata #(over-plan '[{_ n} ::input] '{:k 1})))))
    (is (= :validate (:phase (exdata #(over-plan '[{_ n} ::input] '{a/b 1})))))
    (is (= :validate
           (:phase (exdata #(over-plan '[{_ n} ::input] '{m? true})))))
    (is (= :validate (:phase (exdata #(over-plan '[{_ n} ::input] '{m 1})))))))

(deftest binding-errors
  (letfn [(exdata [f]
            (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (is (= :validate (:phase (exdata #(over-plan '[{k k} ::input] '{k k})))))
    (is (= :validate
           (:phase (exdata #(over-plan '[{_ n} ::input {n m} n] '{m m})))))))

(deftest nested-destructure-errors-and-workaround
  (letfn [(exdata [f]
            (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (is (= :parse
           (:phase (exdata #(over-plan '[{_ {:keys [{:keys [email]}]}} ::input]
                                       '{email email}))))))
  (let [users {:alice {:profile {:email "A@EXAMPLE.COM"}},
               :bob {:profile {:email "B@EXAMPLE.COM"}}}
        cleaned (over [{_ {:keys [profile]}} users {:keys [email]} profile]
                      {email (str/lower-case email)})]
    (is (= {:alice {:profile {:email "a@example.com"}},
            :bob {:profile {:email "b@example.com"}}}
           cleaned))))

(deftest strs-destructure
  (let [data {"a" 1, "b" 2}
        out (over [{:strs [a]} data] {a (inc a)})]
    (is (= {"a" 2, "b" 2} out)))
  (let [data {"a" 1, "b" 2}
        out (over [{:strs [a]} data] {a? false})]
    (is (= {"b" 2} out))))

(deftest strs-destructure-errors
  (letfn [(exdata [f]
            (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (is (= :parse
           (:phase (exdata #(over-plan '[{_ {:strs a}} ::input] '{a a})))))
    (is (= :parse
           (:phase (exdata #(over-plan '[{_ {:strs [a 1]}} ::input]
                                       '{a a})))))))

(deftest as-guard-elides-entry
  (let [data {:a {:x 1}, :b {:x 0}}
        out (over [{_ {:keys [x], :as v}} data] {v? (pos? x)})]
    (is (= {:a {:x 1}} out))))

(deftest key-guard-elides-key
  (let [data {:a {:x 1, :y 2}}
        out (over [{_ {:keys [x y]}} data] {y? (odd? y)})]
    (is (= {:a {:x 1}} out))))

(deftest key-rewrite
  (let [data {:a 1, :b 2}
        out (over [{k v} data] {k (keyword (str (name k) "*"))})]
    (is (= {:a* 1, :b* 2} out))))

(deftest multi-step-scope
  (let [data {:a {:xs [1 2]}, :b {:xs [3]}}
        out (over [{_ {:keys [xs]}} data [n] xs] {n (inc n)})]
    (is (= {:a {:xs [2 3]}, :b {:xs [4]}} out))))

(deftest post-order-semantics
  (let [data {:a {:n 0}}
        out (over [{_ {:keys [n], :as v}} data] {n (inc n), v? (pos? n)})]
    (is (= {:a {:n 1}} out))))

(deftest keyword-and-set-as-fn-idioms
  (let [data {:a {:tag :keep}, :b {:tag :drop}}
        out (over [{_ {:keys [tag], :as v}} data] {v? (= tag :keep)})]
    (is (= {:a {:tag :keep}} out)))
  (let [data {:a {:tags #{:keep}}, :b {:tags #{}}}
        out (over [{_ {:keys [tags], :as v}} data] {v? (tags :keep)})]
    (is (= {:a {:tags #{:keep}}} out))))

(deftest get-in-idiom
  (let [data {:a {:profile {:email "A@EXAMPLE.COM"}}}
        out (over [{_ {:as v}} data]
                  {v (assoc v
                       :domain (second (re-find #"@(.+)"
                                                (get-in v
                                                        [:profile :email]))))})]
    (is (= {:a {:profile {:email "A@EXAMPLE.COM"}, :domain "EXAMPLE.COM"}}
           out))))

(deftest sharing-unchanged-branches
  (let [data {:a {:x 1}, :b {:y 2}}
        out (over [{_ {:keys [x]}} data] {x? (some? x), x (when x (inc x))})]
    (is (= {:a {:x 2}, :b {:y 2}} out))
    (is (identical? (:b data) (:b out)))
    (is (not (identical? (:a data) (:a out))))))

(deftest meta-preservation
  (let [data (with-meta {:a 1} {:m 1})
        out (over [{k v} data] {v (inc v)})]
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
      (is (= :seqable (:expected data))))
    (let [data (exdata #(over [(seq [k v]) 1] {v v}))]
      (is (= :runtime (:phase data)))
      (is (= :seqable (:expected data))))))

(deftest error-message-has-caret-snippet
  (let [msg (try (over [[n] "abc"] {n n})
                 (catch clojure.lang.ExceptionInfo e (ex-message e)))]
    (is (re-find #"\^{2,}" msg))))

(deftest conflict-errors
  (letfn [(exdata [f]
            (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (is (= :validate
           (:phase (exdata #(over-plan '[{_ {:keys [a], :as m}} ::input]
                                       '{a (inc a), m (assoc m :a 0)})))))))

(deftest root-elision
  (is (= tailrecursion.restructure/elide (over [v {:a 1}] {v? false}))))

(deftest key-collision
  (let [data (array-map :a 1 :b 2)
        out (over [{k v} data] {k :a, v (inc v)})]
    (is (= {:a 3} out))))

(deftest record-and-sorted-map
  (defrecord R [a b])
  (let [r (->R 1 2)
        out (over [{k v} r] {v (inc v)})]
    (is (= {:a 2, :b 3} out))
    (is (not (record? out))))
  (let [m (sorted-map :b 2 :a 1)
        out (over [{k v} m] {v (inc v)})]
    (is (sorted? out))
    (is (= {:a 2, :b 3} out))))

(deftest nested-meta-preservation
  (let [inner (with-meta {:x 1} {:m 1})
        data {:a inner}
        out (over [{_ {:keys [x], :as v}} data] {x (inc x)})]
    (is (= {:a {:x 2}} out))
    (is (= {:m 1} (meta (:a out))))))

(deftest selector-and-pattern-parse-errors
  (is (= :parse (:phase (exdata #(over-plan '{_ n} '{n n})))))
  (is (= :parse (:phase (exdata #(over-plan '[{_ n} ::input _] '{n n})))))
  (is (= :parse (:phase (exdata #(over-plan '[{{a b} ::input}] '{a a})))))
  (is (= :parse (:phase (exdata #(over-plan '[{_ {:keys a}} ::input] '{a a})))))
  (is (= :parse (:phase (exdata #(over-plan '[{_ {:strs a}} ::input] '{a a})))))
  (is (= :parse (:phase (exdata #(over-plan '[{_ {:or 1}} ::input] '{a a})))))
  (is (= :parse (:phase (exdata #(over-plan '[{_ {:as 1}} ::input] '{a a})))))
  (is (= :parse
         (:phase (exdata #(over-plan '[{_ {:keys [a 1]}} ::input] '{a a})))))
  (is (= :parse
         (:phase (exdata #(over-plan '[{_ {:strs [a 1]}} ::input] '{a a})))))
  (is (= :parse
         (:phase (exdata #(over-plan '[{_ {:keys [a], 1 :x}} ::input]
                                     '{a a})))))
  (is (= :validate
         (:phase (exdata #(over-plan '[{_ {:keys [a], a :a}} ::input]
                                     '{a a}))))))

(deftest body-validation-more
  (is (= :validate (:phase (exdata #(over-plan '[{_ n} ::input] '(n))))))
  (is (= :validate (:phase (exdata #(over-plan '[{_ n} ::input] '[n])))))
  (is (= :validate (:phase (exdata #(over-plan '[{_ n} ::input] '{:k 1})))))
  (is (= :validate (:phase (exdata #(over-plan '[{_ n} ::input] '{a/b 1})))))
  (is (= :validate (:phase (exdata #(over-plan '[{_ n} ::input] '{m? true})))))
  (is (= :validate (:phase (exdata #(over-plan '[{_ n} ::input] '{m 1})))))
  (is (= :validate (:phase (exdata #(over-plan '[{_ _} ::input] '{_? true})))))
  (is (= :validate (:phase (exdata #(over-plan '[{_ _} ::input] '{_ 1}))))))

(deftest guard-semantics
  (let [data {:a 1, :b 2}
        out (over [{k v} data] {v? (if (= k :a) 0 nil)})]
    (is (= {:a 1} out)))
  (let [cnt (atom 0)
        data [1 2 3]
        out (over [[n] data] {n? (do (swap! cnt inc) true)})]
    (is (= [1 2 3] out))
    (is (= 3 @cnt)))
  (let [cnt (atom 0)
        data {:a {:x 1}}
        out (over [{_ {:keys [x], :as m}} data]
                  {m? false, x? (do (swap! cnt inc) true)})]
    (is (= {} out))
    (is (= 1 @cnt))))

(deftest rewrite-semantics
  (let [data {:a 1}
        out (over [m data] {m tailrecursion.restructure/elide})]
    (is (= tailrecursion.restructure/elide out)))
  (let [data {:a 1, :b 2}
        out (over [{k v} data] {v tailrecursion.restructure/elide})]
    (is (= {:a tailrecursion.restructure/elide,
            :b tailrecursion.restructure/elide}
           out)))
  (let [data [1 2 3]
        out (over [[n] data] {n tailrecursion.restructure/elide})]
    (is (= [] out)))
  (let [data {:a 1}
        out (over [{k v} data] {k tailrecursion.restructure/elide})]
    (is (= {:tailrecursion.restructure/elide 1} out)))
  (let [data {:a 1} out (over [{k v} data] {v 1N})] (is (identical? data out)))
  (let [data {:a 1M}
        out (over [{k v} data] {v 1})]
    (is (= {:a 1} out))
    (is (not (identical? data out)))))

(deftest identity-and-sharing-more
  (let [inner (with-meta {:x 1} {:m 1})
        data {:a 1, :b inner}
        out (over [{:keys [a]} data] {a a})]
    (is (identical? data out))
    (is (identical? (:b data) (:b out))))
  (let [data {:a 1, :b 2}
        out (over [{k v} data] {v? true})]
    (is (identical? data out)))
  (let [data [{:x 1} {:x 2}]
        out (over [[{:keys [x]}] data] {x (inc x)})]
    (is (= [{:x 2} {:x 3}] out))
    (is (not (identical? (nth data 0) (nth out 0))))
    (is (not (identical? (nth data 1) (nth out 1)))))
  (let [data [{:x 1} {:x 2}]
        out (over [[{:keys [x]}] data] {x? (= x 1), x (inc x)})]
    (is (= [{:x 2} {}] out)))
  (let [data [{:x 1} {:x 2}]
        out (over [[{:keys [x]}] data] {x (if (= x 1) (inc x) x)})]
    (is (= [{:x 2} {:x 2}] out))
    (is (identical? (nth data 1) (nth out 1)))))

(deftest map-traversal-more
  (let [data (array-map :b 1 :a 0)
        out (over [{k v} data] {k (if (= k :b) :a k), v? (pos? v)})]
    (is (= {:a 1} out)))
  (let [data {:a {:x 1}}
        out (over [{_ {:keys [a], :as m}} data]
                  {m tailrecursion.restructure/elide})]
    (is (= {} out)))
  (let [data {"a" 1}
        out (over [{:strs [a], :keys [b], :or {b 2}} data] {a a, b b})]
    (is (= {"a" 1, :b 2} out)))
  (let [data {:id 1}
        out (over [{user-id :id, :or {user-id 7}} data] {user-id user-id})]
    (is (= {:id 1} out)))
  (let [data {:id nil}
        out (over [{user-id :id, :or {user-id 7}} data] {user-id user-id})]
    (is (= {:id nil} out)))
  (let [data {:id nil}
        out (over [{user-id :id, :or {user-id 7}} data]
                  {user-id? false, user-id user-id})]
    (is (= {} out)))
  (let [data {:a {:x 1}}
        out (over [{_ {:keys [x], :as m}} data] {x (inc x)})]
    (is (= {:a {:x 2}} out))))

(deftest seq-traversal-more
  (let [data [1 nil 3]
        out (over [[n] data] {n n})]
    (is (= [1 nil 3] out))
    (is (identical? data out)))
  (let [data [{:x 1} {:x 2}]
        out (over [[{:keys [x]}] data] {x (inc x)})]
    (is (= [{:x 2} {:x 3}] out)))
  (let [data [1 2 3]
        out (over [[n] data] {n? (odd? n), n (inc n)})]
    (is (= [2 4] out)))
  (let [data #{1 2 3} out (over [[n] data] {n (inc n)})] (is (= #{2 3 4} out)))
  (let [data (exdata #(over [[n] "abc"] {n n}))]
    (is (= :runtime (:phase data)))
    (is (= :seqable (:expected data))))
  (let [data (exdata #(over [[n] {:a 1}] {n n}))]
    (is (= :runtime (:phase data)))
    (is (= :seqable (:expected data)))))

(deftest seq-destructure-more
  (let [data [[1 2]]
        out (over [(seq [a b :as all]) data] {all? false})]
    (is (= [] out)))
  (let [data [[1 2 3]]
        out (over [(seq [a b & more]) data] {more []})]
    (is (= [[1 2]] out)))
  (let [data [[1 2 3]]
        out (over [(seq [a b & more]) data] {more nil})]
    (is (= [[1 2]] out)))
  (let [data [[1 2 3]]
        out (over [(seq [a b & more]) data] {more '(9 8)})]
    (is (= [[1 2 9 8]] out)))
  (let [data [[1]]
        out (over [(seq [a b]) data] {b? (nil? b), b (or b 0)})]
    (is (= [[1 0]] out)))
  (let [data [[1 2]]
        out (over [(seq [_ b]) data] {b (inc b)})]
    (is (= [[1 3]] out)))
  (let [data [[1 2 3]]
        out (over [(seq [a b & _]) data] {a (inc a)})]
    (is (= [[2 2 3]] out)))
  (let [form (macroexpand '(compile-over [(seq [^long _ a]) ::input] {a a}))]
    (is (contains? (form-tags form "_") 'long))))

(deftest threading-macroexpansion-more
  (let [data [1 2 3]
        out (->> data
                 (over->> [[n]] {n (inc n)})
                 (over->> [[n]] {n (inc n)})
                 (into []))]
    (is (= [3 4 5] out)))
  (let [data {:a 1}
        out (-> data
                (over-> [{:keys [a]}] {a (inc a)})
                (over-> [{:keys [a]}] {a (inc a)}))]
    (is (= {:a 3} out)))
  (let [data {:a 1}
        out (as-> data v (over-> v [{:keys [a]}] {a (inc a)}))]
    (is (= {:a 2} out)))
  (let [data [1 2 3]
        out (->> data
                 (#(over-> % [[n]] {n (inc n)}))
                 (into []))]
    (is (= [2 3 4] out)))
  (let [data [1 2 3]
        out (-> data
                (#(over->> [[n]] {n (inc n)} %))
                (into []))]
    (is (= [2 3 4] out))))

(deftest compile-over-macroexpansion-more
  (let [form (macroexpand
               '(compile-over [{:as ^String m, :keys [a]} ::input] {m m}))]
    (is (some #{'String String java.lang.String} (form-tags form "m"))))
  (let [form (macroexpand
               '(compile-over [{^long user-id :id} ::input] {user-id user-id}))]
    (is (contains? (form-tags form "user-id") 'long)))
  (let [form (macroexpand '(compile-over
                            [(seq [a b & more :as all]) ::input]
                            {a a, all all}))
        syms (filter symbol? (tree-seq coll? seq form))]
    (is (every? (set syms) ['a 'b 'more 'all]))
    (is (empty? (filter #(contains? #{'a 'b 'more 'all} %)
                  (filter (fn [s] (.contains (name s) "__")) syms)))))
  (let [form (macroexpand
               '(compile-over [{:keys [^long x]} ::input] {x? (pos? x)}))]
    (is (empty? (form-tags form "x?")))))

(deftest data-type-preservation-more
  (defrecord R2 [a])
  (let [r (->R2 1) out (over [{k v} r] {v v})] (is (identical? r out)))
  (let [m (sorted-map :b 2 :a 1)
        out (over [{k v} m] {v v})]
    (is (identical? m out)))
  (let [m (array-map :b 2 :a 1)
        out (over [{k v} m] {v v})]
    (is (= (seq m) (seq out)))
    (is (identical? m out)))
  (let [tm (transient {:a 1})
        data (exdata #(over [{k v} tm] {v v}))]
    (is (= :runtime (:phase data)))
    (is (= :map (:expected data))))
  (let [v (with-meta [1 2] {:m 1})
        out (over [[n] v] {n (inc n)})]
    (is (= [2 3] out))
    (is (= {:m 1} (meta out))))
  (let [l (with-meta (list 1 2) {:m 1})
        out (over [[n] l] {n (inc n)})]
    (is (= '(2 3) out))
    (is (= {:m 1} (meta out))))
  (let [s (with-meta #{1 2} {:m 1})
        out (over [[n] s] {n (inc n)})]
    (is (= #{2 3} out))
    (is (= {:m 1} (meta out)))))

(deftest plan-and-compile-determinism
  (let [p1 (over-plan '[{_ n} ::input] '{n (inc n)})
        p2 (over-plan '[{_ n} ::input] '{n (inc n)})]
    (is (= p1 p2)))
  (let [data {:a 1}
        f (compile-over [{_ n} ::input] {n (inc n)})
        out1 (f data)
        out2 (over [{_ n} data] {n (inc n)})]
    (is (= out1 out2))))
