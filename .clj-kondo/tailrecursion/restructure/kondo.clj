(ns tailrecursion.restructure.kondo
  (:require [clj-kondo.hooks-api :as api]
            [clojure.string :as str]))

(defn- guard-symbol?
  [s]
  (and (string? s) (not (str/includes? s "/")) (str/ends-with? s "?")))

(defn- guard->target [s] (subs s 0 (dec (count s))))

(def ^:private destructure-special-keys #{:keys :strs :as :or})

(defn- destructure-entry?
  [[k v]]
  (and (symbol? k)
       (not (contains? destructure-special-keys k))
       (or (keyword? v) (string? v))))

(defn- destructure-map?
  [m]
  (and (map? m)
       (or (some #(contains? m %) destructure-special-keys)
           (some destructure-entry? m))))

(defn- pattern-bindings
  [pat]
  (cond (symbol? pat) (if (= '_ pat) [] [pat])
        (vector? pat) (if (= 1 (count pat)) (pattern-bindings (first pat)) [])
        (map? pat)
          (if (destructure-map? pat)
            (let [ks (get pat :keys)
                  ss (get pat :strs)
                  as (get pat :as)
                  entries (->> pat
                               (remove (fn [[k _]]
                                         (contains? destructure-special-keys
                                                    k)))
                               (filter (fn [[k v]]
                                         (and (symbol? k)
                                              (or (keyword? v) (string? v))))))]
              (concat (filter symbol? ks)
                      (filter symbol? ss)
                      (map first entries)
                      (when (symbol? as) [as])))
            (when (= 1 (count pat))
              (let [[k v] (first pat)]
                (concat (pattern-bindings k) (pattern-bindings v)))))
        :else []))

(defn- selector-bindings
  [selector]
  (when (vector? selector)
    (let [patterns (if (odd? (count selector))
                     (cons (first selector)
                           (map first (partition 2 (rest selector))))
                     (map first (partition 2 selector)))]
      (->> patterns
           (mapcat pattern-bindings)
           (distinct)))))

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
  (let [args (rest (:children form))]
    (if (= 2 (count args)) (nth args 1) (nth args 2))))

(defn- macro-call->selector-node
  [form]
  (let [args (rest (:children form))]
    (if (= 2 (count args)) (nth args 0) (nth args 1))))

(defn- macro-call->topic-node
  [form]
  (let [args (rest (:children form))] (when (= 3 (count args)) (nth args 0))))

(defn- selector-source-nodes
  [selector-node]
  (when (= (:tag selector-node) :vector)
    (->> (:children selector-node)
         (keep-indexed (fn [i n] (when (odd? i) n))))))

(defn- wrap-body
  [body-node sources]
  (if (seq sources)
    (api/list-node (cons (api/token-node 'do) (cons body-node sources)))
    body-node))

(defn over
  [{:keys [node]}]
  (let [selector-node (macro-call->selector-node node)
        body-node (macro-call->body-node node)
        topic-node (macro-call->topic-node node)
        selector (api/sexpr selector-node)
        body (api/sexpr body-node)
        bindings (distinct (concat (selector-bindings selector)
                                   (body-bindings body)))
        sources (cond-> (selector-source-nodes selector-node)
                  topic-node (cons topic-node))
        wrapped (wrap-body body-node sources)]
    {:node (add-bindings wrapped bindings)}))

(defn over->
  [{:keys [node]}]
  (let [args (rest (:children node))
        topic-node (first args)
        selector-node (second args)
        body-node (nth args 2)
        selector (api/sexpr selector-node)
        body (api/sexpr body-node)
        bindings (distinct (concat (selector-bindings selector)
                                   (body-bindings body)))
        sources (cons topic-node (selector-source-nodes selector-node))
        wrapped (wrap-body body-node sources)]
    {:node (add-bindings wrapped bindings)}))

(defn over->>
  [{:keys [node]}]
  (let [args (rest (:children node))
        selector-node (first args)
        body-node (second args)
        topic-node (nth args 2)
        selector (api/sexpr selector-node)
        body (api/sexpr body-node)
        bindings (distinct (concat (selector-bindings selector)
                                   (body-bindings body)))
        sources (cons topic-node (selector-source-nodes selector-node))
        wrapped (wrap-body body-node sources)]
    {:node (add-bindings wrapped bindings)}))

(defn compile-over
  [{:keys [node]}]
  (let [selector-node (macro-call->selector-node node)
        body-node (macro-call->body-node node)
        selector (api/sexpr selector-node)
        body (api/sexpr body-node)
        bindings (distinct (concat (selector-bindings selector)
                                   (body-bindings body)))
        sources (selector-source-nodes selector-node)
        wrapped (wrap-body body-node sources)]
    {:node (add-bindings wrapped bindings)}))

(defn- method-arg-syms
  [arglist]
  (->> arglist
       (mapcat (fn [arg]
                 (cond (symbol? arg) [arg]
                       (and (seq? arg) (symbol? (first arg))) [(first arg)]
                       :else [])))))

(defn defgeneric
  [{:keys [node]}]
  (let [form (api/sexpr node)
        name (second form)]
    {:node (api/list-node (list (api/token-node 'def)
                                (api/token-node name)
                                (api/token-node nil)))}))

(defn defmethod
  [{:keys [node]}]
  (let [children (vec (:children node))
        maybe-qual (nth children 2)
        qual? (keyword? (api/sexpr maybe-qual))
        arg-node (nth children (if qual? 3 2))
        body-nodes (subvec children (if qual? 4 3))
        arg-syms (method-arg-syms (api/sexpr arg-node))
        bindings (api/vector-node (mapcat (fn [s] [(api/token-node s)
                                                   (api/token-node nil)])
                                    arg-syms))
        body (if (seq body-nodes) body-nodes [(api/token-node nil)])
        wrapped (api/list-node (cons (api/token-node 'do)
                                     (concat body
                                             (map api/token-node arg-syms)
                                             [(api/token-node nil)])))
        node (api/list-node (list (api/token-node 'let) bindings wrapped))]
    {:node (with-meta node
             {:clj-kondo/ignore [:type-mismatch :unused-value]})}))
