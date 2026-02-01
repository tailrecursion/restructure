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

(clos/defgeneric pred-exn {:pred-exceptions :error})
(clos/defmethod pred-exn [(x (pred (fn [_] (throw (ex-info "boom" {})))))] :bad)
(clos/defmethod pred-exn [x] :ok)

(deftest pred-exceptions
  (is (thrown? clojure.lang.ExceptionInfo (pred-exn :x))))
