(ns tailrecursion.restructure.kondo
  (:require [clj-kondo.hooks-api :as api]
            [clojure.string :as str]))

(defn- guard-symbol?
  [s]
  (and (string? s) (not (str/includes? s "/")) (str/ends-with? s "?")))

(defn- guard->target [s] (subs s 0 (dec (count s))))

(defn- destructure-map?
  [m]
  (and (map? m)
       (or (contains? m :keys)
           (contains? m :strs)
           (contains? m :as)
           (contains? m :or))))

(defn- pattern-bindings
  [pat]
  (cond (symbol? pat) (if (= '_ pat) [] [pat])
        (vector? pat) (if (= 1 (count pat)) (pattern-bindings (first pat)) [])
        (map? pat) (if (destructure-map? pat)
                     (let [ks (get pat :keys)
                           ss (get pat :strs)
                           as (get pat :as)]
                       (concat (filter symbol? ks)
                               (filter symbol? ss)
                               (when (symbol? as) [as])))
                     (when (= 1 (count pat))
                       (let [[k v] (first pat)]
                         (concat (pattern-bindings k) (pattern-bindings v)))))
        :else []))

(defn- selector-bindings
  [selector]
  (when (vector? selector)
    (->> (partition 2 selector)
         (map first)
         (mapcat pattern-bindings)
         (distinct))))

(defn- body-bindings
  [body]
  (when (map? body)
    (->> (keys body)
         (filter symbol?)
         (mapcat (fn [s]
                   (if (guard-symbol? (name s))
                     [s (symbol (guard->target (name s)))]
                     [s])))
         (distinct))))

(defn- add-bindings
  [body-node bindings]
  (if (seq bindings)
    (let [node (api/list-node (list (api/token-node 'let)
                                    (api/vector-node
                                      (mapcat (fn [s] [(api/token-node (symbol
                                                                         s))
                                                       (api/token-node nil)])
                                        bindings))
                                    body-node))]
      (with-meta node {:clj-kondo/ignore [:type-mismatch :unused-binding]}))
    body-node))

(defn- macro-call->body-node
  [form]
  (let [args (rest (:children form))] (nth args 1)))

(defn- macro-call->selector-node
  [form]
  (let [args (rest (:children form))] (nth args 0)))

(defn over
  [{:keys [node]}]
  (let [selector-node (macro-call->selector-node node)
        body-node (macro-call->body-node node)
        selector (api/sexpr selector-node)
        body (api/sexpr body-node)
        bindings (distinct (concat (selector-bindings selector)
                                   (body-bindings body)))]
    {:node (add-bindings body-node bindings)}))

(defn compile-over
  [{:keys [node]}]
  (let [selector-node (macro-call->selector-node node)
        body-node (macro-call->body-node node)
        selector (api/sexpr selector-node)
        body (api/sexpr body-node)
        bindings (distinct (concat (selector-bindings selector)
                                   (body-bindings body)))]
    {:node (add-bindings body-node bindings)}))
