(ns tailrecursion.restructure-clos-test
  (:require [clojure.test :refer [deftest is]]
            [tailrecursion.restructure.clos :as clos]))

(defmacro thrown? [cls & body] `(try ~@body false (catch ~cls _# true)))

(clos/defgeneric dispatch)

(clos/defmethod dispatch [(x Object)] :object)
(clos/defmethod dispatch [(x String)] :string)
(clos/defmethod dispatch [(x :k)] :value)
(clos/defmethod dispatch [(x (pred even?))] :even)

(deftest basic-dispatch
  (is (= :string (dispatch "a")))
  (is (= :object (dispatch :x)))
  (is (= :value (dispatch :k)))
  (is (= :even (dispatch 2))))


(clos/defgeneric next-method)

(clos/defmethod next-method [(x Number)] (str "num-" x))
(clos/defmethod next-method [(x Long)] (str "long-" (clos/call-next-method)))

(deftest call-next-method-chain
  (is (= "long-num-1" (next-method 1)))
  (is (= "num-1.0" (next-method 1.0))))

(clos/defgeneric combo {:combination :standard})

(def ^:private combo-log (atom []))

(clos/defmethod combo :before [(x Number)] (swap! combo-log conj [:before x]))
(clos/defmethod combo :after [(x Number)] (swap! combo-log conj [:after x]))
(clos/defmethod combo :around
  [(x Number)]
  (swap! combo-log conj [:around :enter x])
  (let [v (clos/call-next-method)]
    (swap! combo-log conj [:around :exit x])
    v))
(clos/defmethod combo [(x Number)] (swap! combo-log conj [:primary x]) (inc x))

(deftest standard-combination-order
  (reset! combo-log [])
  (is (= 2 (combo 1)))
  (is (= [[:around :enter 1] [:before 1] [:primary 1] [:after 1]
          [:around :exit 1]]
         @combo-log)))

(clos/defgeneric list-combo {:combination :list})

(clos/defmethod list-combo [(x Object)] :object)
(clos/defmethod list-combo [(x String)] :string)

(deftest list-combination
  (is (= [:string :object] (list-combo "x")))
  (is (= [:object] (list-combo 1))))

(clos/defgeneric tie-break)
(clos/defmethod tie-break [(x Number)] :first)
(clos/defmethod tie-break [(x Number)] :second)

(deftest tie-breaking-by-definition-order (is (= :first (tie-break 1))))

(clos/defgeneric qualifier-filter {:combination :standard})
(def ^:private qualifier-log (atom []))

(clos/defmethod qualifier-filter :before
  [(x Number)]
  (swap! qualifier-log conj :before-number))
(clos/defmethod qualifier-filter :before
  [(x String)]
  (swap! qualifier-log conj :before-string))
(clos/defmethod qualifier-filter [(x String)]
  (swap! qualifier-log conj :primary-string)
  :ok)
(clos/defmethod qualifier-filter [(x Object)]
  (swap! qualifier-log conj :primary-object)
  :ok)

(deftest qualifier-ordering-with-inapplicable
  (reset! qualifier-log [])
  (is (= :ok (qualifier-filter "x")))
  (is (= [:before-string :primary-string] @qualifier-log)))

(defprotocol PTag)

(clos/defgeneric proto-dispatch)

(clos/defmethod proto-dispatch [(x (satisfies PTag))] :protocol)
(clos/defmethod proto-dispatch [(x Object)] :object)

(deftest satisfies-specializer
  (let [v (reify PTag)]
    (is (= :protocol (proto-dispatch v)))
    (is (= :object (proto-dispatch :x)))))

(clos/defgeneric arity-check)

(clos/defmethod arity-check [(x Object)] :ok)

(deftest arity-mismatch
  (is
    (thrown?
      clojure.lang.ExceptionInfo
      (binding [*ns* (the-ns 'tailrecursion.restructure-clos-test)]
        (eval '(tailrecursion.restructure.clos/defmethod
                arity-check
                [(x Object) (y Object)]
                :bad))))))

(clos/defgeneric node-kind)

(clos/defmethod node-kind [(n (key= :op :bind))] :bind)
(clos/defmethod node-kind [(n (has-keys :kpat :vpat))] :map-entry)
(clos/defmethod node-kind [(n clojure.lang.IPersistentMap)] :map)
(clos/defmethod node-kind [n] :any)

(deftest map-specializers
  (is (= :bind (node-kind {:op :bind, :sym 'x})))
  (is (= :map-entry (node-kind {:kpat :a, :vpat :b})))
  (is (= :map (node-kind {:x 1})))
  (is (= :any (node-kind 1))))

(clos/defgeneric specializer-demo)

(clos/defmethod specializer-demo [(m {:a 1, :b 2})] :map=)
(clos/defmethod specializer-demo [(m (key= :a 1))] :key=)
(clos/defmethod specializer-demo [(m (keys= :a :b))] :keys=)
(clos/defmethod specializer-demo [(m (map-of keyword? int?))] :map-of)
(clos/defmethod specializer-demo [(v #{:x :y})] :in)
(clos/defmethod specializer-demo [v] :default)

(deftest new-specializers
  (is (= :map= (specializer-demo {:a 1, :b 2})))
  (is (= :key= (specializer-demo {:a 1, :b 3})))
  (is (= :keys= (specializer-demo {:a 2, :b 3})))
  (is (= :map-of (specializer-demo {:k 1, :z 2})))
  (is (= :in (specializer-demo :x)))
  (is (= :default (specializer-demo :z))))

(clos/defgeneric value-vs-class)
(clos/defmethod value-vs-class [(x 1)] :value)
(clos/defmethod value-vs-class [(x Number)] :number)
(clos/defmethod value-vs-class [x] :other)

(deftest value-specializer-uses-equals
  (is (= :value (value-vs-class 1)))
  (is (= :number (value-vs-class 1.0)))
  (is (= :number (value-vs-class 2))))

(clos/defgeneric meta-dispatch)
(clos/defmethod meta-dispatch [(m {:a 1})] :value)
(clos/defmethod meta-dispatch [(m clojure.lang.IPersistentMap)] :map)

(deftest meta-ignored-in-value-dispatch
  (is (= :value (meta-dispatch (with-meta {:a 1} {:m 1}))))
  (is (= :map (meta-dispatch {:a 2}))))

(deftest no-legacy-equality-aliases
  (let [pubs (ns-publics 'tailrecursion.restructure.clos)]
    (is (nil? (get pubs 'eql)))
    (is (nil? (get pubs 'equal)))
    (is (nil? (get pubs 'eq)))))

(clos/defgeneric in-seq)
(clos/defmethod in-seq [(x (in [:a :b]))] :hit)
(clos/defmethod in-seq [x] :miss)

(deftest in-sequential (is (= :hit (in-seq :a))) (is (= :miss (in-seq :c))))

(clos/defgeneric isa-dispatch)
(derive ::warning ::problem)
(clos/defmethod isa-dispatch [(x (isa? ::problem))] :problem)
(clos/defmethod isa-dispatch [x] :other)

(deftest isa-specializer
  (is (= :problem (isa-dispatch ::warning)))
  (is (= :other (isa-dispatch ::ok))))

(clos/defgeneric isa-vs-class)
(clos/defmethod isa-vs-class [(x (isa? ::problem))] :isa)
(clos/defmethod isa-vs-class [(x clojure.lang.Keyword)] :class)

(deftest isa-vs-class-specificity (is (= :isa (isa-vs-class ::warning))))

(clos/defgeneric pred-exn {:pred-exceptions :error})
(clos/defmethod pred-exn [(x (pred (fn [_] (throw (ex-info "boom" {})))))] :bad)
(clos/defmethod pred-exn [x] :ok)

(deftest pred-exceptions
  (is (thrown? clojure.lang.ExceptionInfo (pred-exn :x))))

(clos/defgeneric pred-ignore {:pred-exceptions :ignore})
(clos/defmethod pred-ignore [(x (pred (fn [_] (throw (ex-info "boom" {})))))]
  :bad)
(clos/defmethod pred-ignore [x] :ok)

(deftest pred-exceptions-ignore (is (= :ok (pred-ignore :x))))

(clos/defgeneric two-arg)
(clos/defmethod two-arg [(a 1) (b Number)] :value-first)
(clos/defmethod two-arg [(a Number) (b Number)] :numbers)
(clos/defmethod two-arg [a b] :default)

(deftest two-arg-value-and-class
  (is (= :value-first (two-arg 1 2)))
  (is (= :numbers (two-arg 2 2)))
  (is (= :default (two-arg :x 1))))

(clos/defgeneric two-arg-map)
(clos/defmethod two-arg-map [(m {:a 1}) (x Number)] :map=)
(clos/defmethod two-arg-map [(m (keys= :a)) (x Number)] :keys=)
(clos/defmethod two-arg-map [m x] :default)

(deftest two-arg-map-specializers
  (is (= :map= (two-arg-map {:a 1} 1)))
  (is (= :keys= (two-arg-map {:a 2} 1)))
  (is (= :default (two-arg-map {:a 1} :x))))

(clos/defgeneric next-method-missing)
(clos/defmethod next-method-missing [(x Number)] (clos/call-next-method))

(deftest call-next-method-missing
  (is (thrown? clojure.lang.ExceptionInfo (next-method-missing 1))))

(clos/defgeneric value-nil)
(clos/defmethod value-nil [(x nil)] :nil)
(clos/defmethod value-nil [x] :other)

(deftest value-specializer-nil
  (is (= :nil (value-nil nil)))
  (is (= :other (value-nil 1))))

(clos/defgeneric value-bool)
(clos/defmethod value-bool [(x true)] :true)
(clos/defmethod value-bool [(x false)] :false)
(clos/defmethod value-bool [x] :other)

(deftest value-specializer-bool
  (is (= :true (value-bool true)))
  (is (= :false (value-bool false)))
  (is (= :other (value-bool nil))))

(clos/defgeneric in-map)
(clos/defmethod in-map [(x (in {:a 1}))] :hit)
(clos/defmethod in-map [x] :miss)

(deftest in-map-specializer
  (is (= :hit (in-map :a)))
  (is (= :miss (in-map :b))))

(clos/defgeneric map=-extra)
(clos/defmethod map=-extra [(m {:a 1})] :map=)
(clos/defmethod map=-extra [m] :other)

(deftest map=-extra-keys
  (is (= :map= (map=-extra {:a 1, :b 2})))
  (is (= :other (map=-extra {:a 2}))))

(clos/defgeneric keys-precedence)
(clos/defmethod keys-precedence [(m (has-keys :a))] :keys)
(clos/defmethod keys-precedence [(m (keys= :a))] :keys=)

(deftest keys-precedence-test (is (= :keys (keys-precedence {:a 1}))))

(clos/defgeneric value-vs-key)
(clos/defmethod value-vs-key [(m {:a 1})] :value)
(clos/defmethod value-vs-key [(m (key= :a 1))] :key=)
(clos/defmethod value-vs-key [m] :other)

(deftest value-vs-key-precedence
  (is (= :value (value-vs-key {:a 1})))
  (is (= :other (value-vs-key {:a 2}))))

(clos/defgeneric key-vs-keys)
(clos/defmethod key-vs-keys [(m (key= :a 1))] :key=)
(clos/defmethod key-vs-keys [(m (has-keys :a))] :keys)

(deftest key-vs-keys-precedence (is (= :key= (key-vs-keys {:a 1}))))

(clos/defgeneric pred-vs-class)
(clos/defmethod pred-vs-class [(x (pred (fn [x] (and (number? x) (even? x)))))]
  :pred)
(clos/defmethod pred-vs-class [(x Number)] :class)

(deftest pred-vs-class-precedence
  (is (= :pred (pred-vs-class 2)))
  (is (= :class (pred-vs-class 3))))

(defprotocol PSat)

(clos/defgeneric sat-vs-class)
(clos/defmethod sat-vs-class [(x (satisfies PSat))] :sat)
(clos/defmethod sat-vs-class [(x Object)] :class)

(deftest satisfies-vs-class-precedence
  (is (= :sat
         (sat-vs-class (reify PSat))))
  (is (= :class (sat-vs-class 1))))

(clos/defgeneric pred-truthy)
(clos/defmethod pred-truthy [(x (pred (fn [_] :ok)))] :pred)
(clos/defmethod pred-truthy [x] :other)

(deftest pred-truthy-values (is (= :other (pred-truthy :x))))

(clos/defgeneric mapof-empty)
(clos/defmethod mapof-empty [(m (map-of keyword? int?))] :map-of)
(clos/defmethod mapof-empty [m] :other)

(deftest map-of-empty
  (is (= :map-of (mapof-empty {})))
  (is (= :other (mapof-empty {:a "x"}))))

(clos/defgeneric keys-empty)
(clos/defmethod keys-empty [(m (keys=))] :empty)
(clos/defmethod keys-empty [m] :other)

(deftest keys=-empty
  (is (= :empty (keys-empty {})))
  (is (= :other (keys-empty {:a 1}))))

(clos/defgeneric mapof-ignore {:pred-exceptions :ignore})
(clos/defmethod mapof-ignore [(m (map-of (fn [_] (throw (ex-info "boom" {})))
                                         int?))]
  :map-of)
(clos/defmethod mapof-ignore [m] :other)

(deftest map-of-pred-exceptions-ignore (is (= :other (mapof-ignore {:a 1}))))

(clos/defgeneric combo-empty-list {:combination :list})
(deftest combo-list-empty (is (= [] (combo-empty-list 1))))

(clos/defgeneric combo-empty-and {:combination :and})
(deftest combo-and-empty (is (true? (combo-empty-and 1))))

(clos/defgeneric combo-empty-or {:combination :or})
(deftest combo-or-empty (is (false? (combo-empty-or 1))))

(clos/defgeneric combo-empty-plus {:combination :+})
(deftest combo-plus-empty (is (= 0 (combo-empty-plus 1))))

(clos/defgeneric combo-empty-max {:combination :max})
(clos/defgeneric combo-empty-min {:combination :min})
(deftest combo-max-min-empty
  (is (nil? (combo-empty-max 1)))
  (is (nil? (combo-empty-min 1))))

(clos/defgeneric around-only)
(clos/defmethod around-only :around [(x Number)] (clos/call-next-method))

(deftest around-without-primary
  (is (thrown? clojure.lang.ExceptionInfo (around-only 1))))

(clos/defgeneric next-args)
(clos/defmethod next-args [(x Number)] x)
(clos/defmethod next-args :around [(x Long)] (clos/call-next-method 5))

(deftest call-next-method-explicit-args (is (= 5 (next-args 1))))

(clos/defgeneric next-method-flag)
(clos/defmethod next-method-flag [(x Number)] (clos/next-method?))
(clos/defmethod next-method-flag [(x Long)] (clos/next-method?))

(deftest next-method-flags
  (is (true? (next-method-flag 1)))
  (is (false? (next-method-flag 1.0))))

(clos/defgeneric any-value)
(clos/defmethod any-value [x] :any)
(clos/defmethod any-value [(x 1)] :value)

(deftest any-vs-value (is (= :value (any-value 1))) (is (= :any (any-value 2))))

(clos/defgeneric any-map-pred)
(clos/defmethod any-map-pred [(m {:a 1})] :map=)
(clos/defmethod any-map-pred [(x (pred number?))] :pred)
(clos/defmethod any-map-pred [x] :any)

(deftest any-map-pred-test
  (is (= :map= (any-map-pred {:a 1})))
  (is (= :pred (any-map-pred 1)))
  (is (= :any (any-map-pred :x))))

(clos/defgeneric three-arg)
(clos/defmethod three-arg [(a Number) (b Number) (c Number)] :nums)
(clos/defmethod three-arg [a b c] :any)

(deftest three-arg-dispatch
  (is (= :nums (three-arg 1 2 3)))
  (is (= :any (three-arg 1 :x 3))))

(clos/defgeneric qual-multi {:combination :standard})
(def ^:private qual-multi-log (atom []))
(clos/defmethod qual-multi :before
  [(a Number) (b String)]
  (swap! qual-multi-log conj :before-num-str))
(clos/defmethod qual-multi :before
  [(a Number) (b Number)]
  (swap! qual-multi-log conj :before-num-num))
(clos/defmethod qual-multi [(a Number) (b String)]
  (swap! qual-multi-log conj :primary-num-str)
  :ok)

(deftest qualifiers-multi-arg-filtering
  (reset! qual-multi-log [])
  (is (= :ok (qual-multi 1 "x")))
  (is (= [:before-num-str :primary-num-str] @qual-multi-log)))
