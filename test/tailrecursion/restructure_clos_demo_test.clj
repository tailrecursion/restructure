(ns tailrecursion.restructure-clos-demo-test
  (:require [clojure.test :refer [deftest is]]
            [tailrecursion.restructure.clos :as clos]))

;; Demo: arithmetic polymorphism with method combinations

(clos/defgeneric describe-number {:combination :standard})

(clos/defmethod describe-number :before [(x Number)] (str "before:" x))
(clos/defmethod describe-number :after [(x Number)] (str "after:" x))
(clos/defmethod describe-number :around
  [(x Number)]
  (str "[" (clos/call-next-method) "]"))
(clos/defmethod describe-number [(x Long)] (str "long:" x))
(clos/defmethod describe-number [(x Number)] (str "num:" x))

(deftest demo-standard-combination
  (is (= "[long:1]" (describe-number 1)))
  (is (= "[num:1.0]" (describe-number 1.0))))

;; Demo: value and shape dispatch

(clos/defgeneric classify {:combination :standard})

(clos/defmethod classify [(x :ok)] :ok)
(clos/defmethod classify [(x #{:warn :error})] :bad)
(clos/defmethod classify [(m {:status :ok})] :status-ok)
(clos/defmethod classify [(m (has-keys :status :code))] :status)
(clos/defmethod classify [(m (keys= :a :b))] :ab)
(clos/defmethod classify [(m (map-of keyword? int?))] :kw-int)
(clos/defmethod classify [x] :other)

(deftest demo-specializers
  (is (= :ok (classify :ok)))
  (is (= :bad (classify :warn)))
  (is (= :status-ok (classify {:status :ok, :code 200})))
  (is (= :status (classify {:status :nope, :code 500})))
  (is (= :ab (classify {:a 1, :b 2})))
  (is (= :kw-int (classify {:x 1, :y 2})))
  (is (= :other (classify 1))))

;; Demo: next-method chaining with predicates

(clos/defgeneric stringify {:combination :standard})

(clos/defmethod stringify [(x (pred string?))] (str "s:" x))
(clos/defmethod stringify [(x (pred number?))] (str "n:" x))
(clos/defmethod stringify :around
  [(x (pred number?))]
  (str "<" (clos/call-next-method) ">"))

(deftest demo-next-method
  (is (= "s:hi" (stringify "hi")))
  (is (= "<n:1>" (stringify 1))))

;; Demo: list combination

(clos/defgeneric tags {:combination :list})

(clos/defmethod tags [(x Number)] :number)
(clos/defmethod tags [(x Long)] :long)
(clos/defmethod tags [(x (pred odd?))] :odd)

(deftest demo-list-combination
  (is (= [:odd :long :number] (tags 1)))
  (is (= [:long :number] (tags 2))))
