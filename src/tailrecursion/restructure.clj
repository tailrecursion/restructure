(ns tailrecursion.restructure
  "Rewrite nested Clojure data with a declared shape."
  (:require [clojure.string :as str]
            [tailrecursion.restructure.parser :as p]))

(def ^:no-doc elide ::elide)

(defn ^:no-doc elide? [x] (identical? x elide))

(defn- error
  ([msg data] (throw (ex-info msg data)))
  ([phase msg data] (throw (ex-info msg (assoc data :phase phase)))))

(defn- type-error
  [path expected actual]
  (error :runtime
         "Pattern type mismatch"
         {:path path,
          :expected expected,
          :actual (if (nil? actual) nil (type actual)),
          :value actual}))

(defn ^:no-doc ensure-map
  [v path]
  (when-not (map? v) (type-error path :map v))
  v)

(defn ^:no-doc ensure-seqable
  [v path]
  (when-not (or (sequential? v) (set? v) (seq? v)) (type-error path :seqable v))
  v)

(defn- unchanged-coll?
  [coll out]
  (and (= (count coll) (count out)) (every? true? (map identical? coll out))))

(defn ^:no-doc traverse-seq
  "Traverse a sequential or set, applying f to each element.
   f returns elide to remove an element."
  [coll f path]
  (if (nil? coll)
    coll
    (do
      (ensure-seqable coll path)
      (cond (set? coll) (let [out (reduce (fn [acc x]
                                            (let [y (f x)]
                                              (if (elide? y) acc (conj acc y))))
                                    (empty coll)
                                    coll)]
                          (if (= out coll) coll out))
            (vector? coll)
              (let [cnt (count coll)
                    out (loop [i 0
                               acc (transient [])]
                          (if (< i cnt)
                            (let [x (nth coll i)
                                  y (f x)]
                              (recur (inc i) (if (elide? y) acc (conj! acc y))))
                            (persistent! acc)))]
                (if (unchanged-coll? coll out) coll out))
            :else (let [outv (reduce (fn [acc x]
                                       (let [y (f x)]
                                         (if (elide? y) acc (conj acc y))))
                               []
                               coll)
                        out (if (list? coll) (apply list outv) (seq outv))]
                    (if (unchanged-coll? coll outv) coll out))))))

(defn ^:no-doc traverse-map
  "Traverse map entries, applying f to each entry.
   f returns elide to drop entry or a vector [k v]."
  [m f path]
  (if (nil? m)
    m
    (do (ensure-map m path)
        (let [base (if (record? m) {} (empty m))
              out (reduce-kv
                    (fn [acc k v]
                      (let [r (f k v)]
                        (if (elide? r) acc (assoc acc (nth r 0) (nth r 1)))))
                    base
                    m)
              out (if (identical? (meta m) (meta out))
                    out
                    (with-meta out (meta m)))]
          (if (= out m) m out)))))

;; Parsing / validation

(defn- destructure-map?
  [m]
  (and (map? m)
       (or (contains? m :keys)
           (contains? m :strs)
           (contains? m :as)
           (contains? m :or))))

(defn- parse-destructure
  [m path]
  (let [ks (get m :keys)
        ss (get m :strs)
        as (get m :as)
        orv (get m :or)]
    (when (and ks (not (vector? ks)))
      (error :parse
             "Expected :keys vector"
             {:path path, :pattern m, :expected :keys-vector}))
    (when (and ks (some (fn [s] (not (symbol? s))) ks))
      (error :parse
             "Expected symbols in :keys"
             {:path path, :pattern m, :expected :symbols}))
    (when (and ss (not (vector? ss)))
      (error :parse
             "Expected :strs vector"
             {:path path, :pattern m, :expected :strs-vector}))
    (when (and ss (some (fn [s] (not (symbol? s))) ss))
      (error :parse
             "Expected symbols in :strs"
             {:path path, :pattern m, :expected :symbols}))
    (when (and as (not (symbol? as)))
      (error :parse
             "Expected symbol for :as"
             {:path path, :pattern m, :expected :symbol}))
    (when (and orv (not (map? orv)))
      (error :parse
             "Expected map for :or"
             {:path path, :pattern m, :expected :map}))
    {:op :destructure,
     :keys (vec (or ks [])),
     :strs (vec (or ss [])),
     :as as,
     :or orv,
     :path path,
     :form m}))

(declare pattern-parser)

(defn- parse-pattern-form
  [form path]
  (let [[ok ast] (p/parse-all pattern-parser {:form form, :path path})]
    (when-not ok
      (error :parse
             "Unsupported pattern"
             {:path path, :pattern form, :expected :pattern}))
    ast))

(p/define-parser symbol-pattern-parser
                 (p/map-result (fn [{:keys [form path]}]
                                 {:op :bind, :sym form, :path path, :form form})
                               (p/satisfy #(symbol? (:form %)))))

(p/define-parser vector-pattern-parser
                 (p/map-result
                   (fn [{:keys [form path]}]
                     (when (not= 1 (count form))
                       (error
                         :parse
                         "Only single-element vector patterns are supported"
                         {:path path, :pattern form, :expected :single-vector}))
                     {:op :seq-elems,
                      :pat (parse-pattern-form (first form) (conj path 0)),
                      :path path,
                      :form form})
                   (p/satisfy #(vector? (:form %)))))

(p/define-parser
  destructure-pattern-parser
  (p/map-result
    (fn [{:keys [form path]}]
      (when (some (fn [[k _]] (not (#{:keys :strs :as :or} k))) form)
        (error :parse
               "Destructure maps may only contain :keys, :strs, :as, :or"
               {:path path, :pattern form}))
      (parse-destructure form path))
    (p/satisfy #(and (map? (:form %)) (destructure-map? (:form %))))))

(p/define-parser
  map-entry-pattern-parser
  (p/map-result
    (fn [{:keys [form path]}]
      (when (not= 1 (count form))
        (error :parse
               "Map-entry traversal requires a single entry"
               {:path path, :pattern form, :expected :single-entry}))
      (let [[k v] (first form)
            kpat (parse-pattern-form k (conj path :key))
            vpat (parse-pattern-form v (conj path :val))]
        (when (#{:seq-elems :map-entries} (:op kpat))
          (error :parse
                 "Key pattern may not be a traversal"
                 {:path path, :pattern form, :cause :key-traversal}))
        {:op :map-entries, :kpat kpat, :vpat vpat, :path path, :form form}))
    (p/satisfy #(and (map? (:form %)) (not (destructure-map? (:form %)))))))

(p/define-parser pattern-parser
                 (p/alternative 'symbol-pattern-parser
                                'vector-pattern-parser
                                'destructure-pattern-parser
                                'map-entry-pattern-parser))

(defn- parse-selector
  [sel]
  (when-not (vector? sel)
    (error :parse
           "Selector must be a vector"
           {:selector sel, :expected :vector}))
  (when (odd? (count sel))
    (error :parse
           "Selector must have even number of forms"
           {:selector sel, :expected :pattern-source-pairs}))
  (let [pairs (partition 2 sel)
        steps (mapv (fn [i [pat src]]
                      (let [path [:step i]
                            pat-ast (parse-pattern-form pat path)]
                        {:pattern pat-ast, :source src, :path path}))
                (range)
                pairs)]
    {:selector sel, :steps steps}))

;; Binding analysis

(defn- binding-info
  [sym kind path & {:keys [key]}]
  {:sym sym, :kind kind, :path path, :key key})


(declare validate-node)

(defmulti pat-bindings :op)

(defmethod pat-bindings :bind
  [pat]
  (let [pat (validate-node pat)
        sym (:sym pat)
        path (:path pat)]
    (if (= '_ sym) [] [(binding-info sym :value path)])))

(defmethod pat-bindings :destructure
  [pat]
  (let [pat (validate-node pat)
        keys (:keys pat)
        strs (:strs pat)
        as (:as pat)
        path (:path pat)]
    (vec (concat (for [s keys
                       :when (not= '_ s)]
                   (binding-info s :map-key path :key (keyword (name s))))
                 (for [s strs
                       :when (not= '_ s)]
                   (binding-info s :map-key path :key (name s)))
                 (when (and as (not= '_ as)) [(binding-info as :map path)])))))

(defmethod pat-bindings :seq-elems
  [pat]
  (let [pat (validate-node pat)] (pat-bindings (:pat pat))))

(defmethod pat-bindings :map-entries
  [pat]
  (let [pat (validate-node pat)]
    (vec (concat (pat-bindings (:kpat pat)) (pat-bindings (:vpat pat))))))

(defmethod pat-bindings :default
  [pat]
  (error :validate "Unknown pattern op" {:pattern pat}))

(defn- collect-bindings [pat] (pat-bindings pat))

(defn- merge-bindings
  [a b]
  (reduce (fn [acc {:keys [sym], :as info}]
            (when (contains? acc sym)
              (error :validate
                     "Duplicate binding"
                     {:binding sym, :path (:path info)}))
            (assoc acc sym info))
    a
    b))

(defn- attach-children
  [binding-children sym child]
  (update binding-children sym (fnil conj []) child))

(defn- analyze-bindings
  [sel-ast]
  (let [steps (:steps sel-ast)
        first-step (first steps)
        bindings (merge-bindings {} (collect-bindings (:pattern first-step)))
        binding-children (atom {})]
    (loop [i 1
           bindings bindings]
      (if (< i (count steps))
        (let [{:keys [pattern source path]} (nth steps i)]
          (when-not (symbol? source)
            (error :validate
                   "Source must be a bound symbol"
                   {:path path, :source source}))
          (when-not (contains? bindings source)
            (error :validate
                   "Source is not bound"
                   {:path path, :source source}))
          (swap! binding-children attach-children source pattern)
          (recur (inc i) (merge-bindings bindings (collect-bindings pattern))))
        {:bindings bindings, :binding-children @binding-children}))))

;; Body analysis

(defn- guard-symbol?
  [s]
  (and (symbol? s) (nil? (namespace s)) (str/ends-with? (name s) "?")))

(defn- guard->target [s] (symbol (subs (name s) 0 (dec (count (name s))))))

(defn- analyze-body
  [body bindings]
  (when-not (map? body)
    (error :validate "Body must be a map literal" {:body body, :expected :map}))
  (let [rewrites (atom {})
        guards (atom {})
        guard-nodes (atom {})]
    (doseq [[k v] body]
      (when-not (symbol? k)
        (error :validate "Body keys must be symbols" {:body body, :key k}))
      (when (namespace k)
        (error :validate
               "Body keys must be unqualified symbols"
               {:body body, :key k}))
      (if (guard-symbol? k)
        (let [tgt (guard->target k)]
          (when-not (contains? bindings tgt)
            (error :validate
                   "Guard symbol is not bound"
                   {:binding tgt, :key k}))
          (when (contains? @guards tgt)
            (error :validate "Duplicate guard" {:binding tgt}))
          (swap! guards assoc tgt v)
          (swap! guard-nodes assoc
            tgt
            {:sym tgt, :expr v, :path (:path (bindings tgt))}))
        (do (when-not (contains? bindings k)
              (error :validate
                     "Rewrite symbol is not bound"
                     {:binding k, :key k}))
            (when (contains? @rewrites k)
              (error :validate "Duplicate rewrite" {:binding k}))
            (swap! rewrites assoc k v))))
    {:rewrites @rewrites, :guards @guards, :guard-nodes @guard-nodes}))

(defn- detect-conflicts
  [bindings rewrites]
  (doseq [[sym {:keys [kind]}] bindings]
    (when (and (= kind :map) (contains? rewrites sym))
      (let [node-path (:path (bindings sym))
            key-rewrites (filter (fn [[s info]]
                                   (and (= (:path info) node-path)
                                        (= (:kind info) :map-key)
                                        (contains? rewrites s)))
                           bindings)]
        (when (seq key-rewrites)
          (error :validate
                 "Conflicting rewrites: :as and :keys"
                 {:binding sym, :path node-path}))))))

;; Pass 5: usage analysis (conservative)

(defn- collect-symbols
  [form]
  (let [syms (atom #{})]
    (letfn [(walk [f]
              (cond (symbol? f) (when (nil? (namespace f)) (swap! syms conj f))
                    (seq? f) (let [op (first f)]
                               (if (= 'quote op) nil (doseq [x f] (walk x))))
                    (coll? f) (doseq [x f] (walk x))
                    :else nil))]
      (walk form) @syms)))

(defn- analyze-usage
  [body rewrites guards steps]
  (let [body-syms
          (reduce (fn [acc [_ v]] (into acc (collect-symbols v))) #{} body)
        src-syms (into #{} (map :source (rest steps)))]
    (into #{} (concat (keys rewrites) (keys guards) body-syms src-syms))))

;; Codegen helpers

(defn- node-path [pat] (:path pat))

(defn- render-path
  [path]
  (let [part (fn [p]
               (cond (integer? p) (str "step " p)
                     (= p :key) "key"
                     (= p :val) "value"
                     :else (str p)))]
    (str/join " / " (map part path))))

(defn- validate-path
  [path ctx]
  (when (and (seq path) (not= :step (first path)))
    (error :validate
           "Invalid pattern path"
           (assoc ctx
             :path path
             :path-str (render-path path))))
  path)

(defn- validate-node
  [pat]
  (let [path (:path pat)
        ctx {:pattern (:form pat), :op (:op pat)}]
    (validate-path path ctx)
    (case (:op pat)
      :bind (when-not (symbol? (:sym pat))
              (error :validate "Invalid bind node" (assoc ctx :path path)))
      :seq-elems
        (when-not (:pat pat)
          (error :validate "Missing :pat in seq node" (assoc ctx :path path)))
      :map-entries (do (when-not (:kpat pat)
                         (error :validate
                                "Missing :kpat in map-entry node"
                                (assoc ctx :path path)))
                       (when-not (:vpat pat)
                         (error :validate
                                "Missing :vpat in map-entry node"
                                (assoc ctx :path path))))
      :destructure nil
      (error :validate "Unknown pattern op" (assoc ctx :path path)))
    pat))

(declare emit-with-pattern emit-pattern-value)

(defn- emit-apply-children
  [env v children]
  (reduce (fn [expr child]
            (let [v# (gensym "v")]
              `(let [~v# ~expr]
                 (if (elide? ~v#) elide ~(emit-pattern-value env child v#)))))
    v
    children))

(defn- emit-bind-scope
  [env sym value-expr body-fn]
  (let [children (get-in env [:binding-children sym])
        guard-present? (contains? (:guards env) sym)
        guard-expr (get-in env [:guards sym])
        rewrite-present? (contains? (:rewrites env) sym)
        rewrite-expr (get-in env [:rewrites sym])
        used? (contains? (:used-syms env) sym)
        v# (gensym "v")
        out# (gensym "out")]
    (cond (= '_ sym) (body-fn value-expr)
          (and (not guard-present?)
               (not rewrite-present?)
               (empty? children)
               (not used?))
            (body-fn value-expr)
          :else `(let [~v# ~value-expr
                       ~v# ~(if (seq children)
                              (emit-apply-children env v# children)
                              v#)]
                   (if (elide? ~v#)
                     elide
                     (let [~sym ~v#]
                       (let [keep?# ~(if guard-present? guard-expr true)]
                         (if keep?#
                           (let [~sym ~(if rewrite-present? rewrite-expr sym)
                                 ~out# ~sym]
                             ~(body-fn out#))
                           elide))))))))

(defn- emit-destructure-scope
  [env pat value-expr body-fn]
  (let [ks (:keys pat)
        ss (:strs pat)
        as (:as pat)
        orv (:or pat)
        dform (cond-> {}
                (seq ks) (assoc :keys ks)
                (seq ss) (assoc :strs ss)
                as (assoc :as as)
                orv (assoc :or orv))
        all-syms (vec (concat ks ss))
        key->k (into {}
                     (concat (map (fn [s] [s (keyword (name s))]) ks)
                             (map (fn [s] [s (name s)]) ss)))
        v# (gensym "m")
        had-syms (zipmap all-syms
                         (map (fn [s] (gensym (str "had_" (name s)))) all-syms))
        key-children (fn [s] (get-in env [:binding-children s]))
        key-drop-syms (zipmap all-syms
                              (map (fn [s] (gensym (str (name s) "_drop")))
                                all-syms))
        had-bindings (vec (mapcat (fn [s] [(get had-syms s)
                                           `(contains? ~v# ~(get key->k s))])
                            all-syms))
        child-bindings
          (vec (mapcat (fn [s] [s
                                (if (seq (key-children s))
                                  (emit-apply-children env s (key-children s))
                                  s)])
                 all-syms))
        key-bindings
          (vec
            (mapcat
              (fn [s]
                (let [guard-present? (contains? (:guards env) s)
                      guard-expr (get-in env [:guards s])
                      rewrite-present? (contains? (:rewrites env) s)
                      rewrite-expr (get-in env [:rewrites s])
                      drop-sym (get key-drop-syms s)]
                  [drop-sym
                   `(or (elide? ~s) (not ~(if guard-present? guard-expr true)))
                   s `(if ~drop-sym ~s ~(if rewrite-present? rewrite-expr s))]))
              all-syms))
        m1# (gensym "m1")
        map-bindings (vec (reduce (fn [acc s]
                                    (let [drop-sym (get key-drop-syms s)
                                          had? (get had-syms s)
                                          k (get key->k s)
                                          rewrite? (contains? (:rewrites env)
                                                              s)]
                                      (conj acc
                                            m1#
                                            `(if ~drop-sym
                                               (if ~had? (dissoc ~m1# ~k) ~m1#)
                                               (if (or ~had? ~rewrite?)
                                                 (assoc ~m1# ~k ~s)
                                                 ~m1#)))))
                            [m1# v#]
                            all-syms))]
    `(let [~v# ~value-expr]
       (if (nil? ~v#)
         nil
         (do
           (ensure-map ~v# ~(node-path pat))
           (let [~@had-bindings]
             (let [~dform ~v#]
               (let [~@child-bindings]
                 (let [~@key-bindings]
                   (let [~@map-bindings]
                     ~(if as
                        (let [as-children (get-in env [:binding-children as])
                              as-guard-present? (contains? (:guards env) as)
                              as-guard (get-in env [:guards as])
                              as-rewrite-present? (contains? (:rewrites env) as)
                              as-rewrite (get-in env [:rewrites as])
                              as-drop-sym (gensym "as_drop")
                              as-val-sym (gensym "as_val")
                              as-child-expr
                                (if (seq as-children)
                                  (emit-apply-children env as as-children)
                                  as)
                              as-guard-expr (if as-guard-present? as-guard true)
                              as-rewrite-expr
                                (if as-rewrite-present? as-rewrite as)
                              elide?-sym 'tailrecursion.restructure/elide?
                              elide-sym 'tailrecursion.restructure/elide]
                          (list 'let
                                [as m1# as as-child-expr as-drop-sym
                                 (list 'or
                                       (list elide?-sym as)
                                       (list 'not as-guard-expr)) as
                                 (list 'if as-drop-sym as as-rewrite-expr)
                                 as-val-sym as]
                                (list 'if
                                      as-drop-sym
                                      elide-sym
                                      (body-fn as-val-sym))))
                        (body-fn m1#))))))))))))

(defmulti emit-with-pattern (fn [_env pat _value-expr _body-fn] (:op pat)))

(defmethod emit-with-pattern :bind
  [env pat value-expr body-fn]
  (emit-bind-scope env (:sym pat) value-expr body-fn))

(defmethod emit-with-pattern :destructure
  [env pat value-expr body-fn]
  (emit-destructure-scope env pat value-expr body-fn))

(defmethod emit-with-pattern :seq-elems
  [env pat value-expr body-fn]
  (let [f (get-in env [:node-fns (:id pat)])
        v# (gensym "v")]
    `(let [~v# (~f ~value-expr)] (if (elide? ~v#) elide ~(body-fn v#)))))

(defmethod emit-with-pattern :map-entries
  [env pat value-expr body-fn]
  (let [f (get-in env [:node-fns (:id pat)])
        v# (gensym "v")]
    `(let [~v# (~f ~value-expr)] (if (elide? ~v#) elide ~(body-fn v#)))))

(defmethod emit-with-pattern :default
  [_env pat _value-expr _body-fn]
  (error :codegen "Unknown pattern op" {:pattern pat}))

(defn- emit-pattern-value
  [env pat value-expr]
  (emit-with-pattern env pat value-expr (fn [v] v)))

(defmulti assign-ids (fn [pat _id _nodes] (:op pat)))

(defmethod assign-ids :seq-elems
  [pat id nodes]
  (let [nid (swap! id inc)
        child (assign-ids (:pat pat) id nodes)
        p' (assoc pat
             :id nid
             :pat child)]
    (swap! nodes conj p')
    p'))

(defmethod assign-ids :map-entries
  [pat id nodes]
  (let [nid (swap! id inc)
        kpat (assign-ids (:kpat pat) id nodes)
        vpat (assign-ids (:vpat pat) id nodes)
        p' (assoc pat
             :id nid
             :kpat kpat
             :vpat vpat)]
    (swap! nodes conj p')
    p'))

(defmethod assign-ids :bind [pat _id _nodes] pat)
(defmethod assign-ids :destructure [pat _id _nodes] pat)
(defmethod assign-ids :default [pat _id _nodes] pat)

(defn- collect-all-traversals
  [root binding-children]
  (let [id (atom 0)
        nodes (atom [])
        root' (assign-ids root id nodes)
        children' (into {}
                        (map (fn [[sym pats]] [sym
                                               (mapv #(assign-ids % id nodes)
                                                 pats)])
                          binding-children))]
    {:root root', :binding-children children', :nodes @nodes}))

(defmulti emit-traversal-fn (fn [_env pat] (:op pat)))

(defmethod emit-traversal-fn :seq-elems
  [env pat]
  (let [elem-sym (gensym "elem")
        f-sym (gensym "seqnode")]
    {:sym f-sym,
     :form `(fn [coll#]
              (traverse-seq coll#
                            (fn [~elem-sym]
                              ~(emit-pattern-value env (:pat pat) elem-sym))
                            ~(node-path pat)))}))

(defmethod emit-traversal-fn :map-entries
  [env pat]
  (let [k-sym (gensym "k")
        v-sym (gensym "v")
        f-sym (gensym "mapnode")]
    {:sym f-sym,
     :form `(fn [m#]
              (traverse-map m#
                            (fn [~k-sym ~v-sym]
                              ~(emit-with-pattern env
                                                  (:kpat pat)
                                                  k-sym
                                                  (fn [kval]
                                                    (emit-with-pattern
                                                      env
                                                      (:vpat pat)
                                                      v-sym
                                                      (fn [vval]
                                                        `[~kval ~vval])))))
                            ~(node-path pat)))}))

(defmethod emit-traversal-fn :default [_env _pat] nil)

(defn- plan-over*
  "Compile-time planner: parse, validate, and analyze selector/body into a plan."
  [sel body]
  (let [parse-pass (parse-selector sel)
        binding-pass (analyze-bindings parse-pass)
        body-pass (analyze-body body (:bindings binding-pass))]
    (detect-conflicts (:bindings binding-pass) (:rewrites body-pass))
    (let [used (analyze-usage body
                              (:rewrites body-pass)
                              (:guards body-pass)
                              (:steps parse-pass))]
      {:v 1,
       :selector sel,
       :steps (:steps parse-pass),
       :bindings (:bindings binding-pass),
       :binding-children (:binding-children binding-pass),
       :rewrites (:rewrites body-pass),
       :guards (:guards body-pass),
       :guard-nodes (:guard-nodes body-pass),
       :used-syms used})))

(defn- codegen*
  "Generate a compiled function form from a plan."
  [plan]
  (let [{:keys [steps binding-children], :as plan} plan
        root (:pattern (first steps))
        trav (collect-all-traversals root binding-children)
        nodes (:nodes trav)
        binding-children (:binding-children trav)
        node-fns (into {}
                       (map (fn [p] [(:id p)
                                     (gensym (str (name (:op p)) "_fn"))])
                         nodes))
        env (assoc plan
              :node-fns node-fns
              :binding-children binding-children)
        fn-defs (vec (keep (fn [p]
                             (let [{:keys [form]} (emit-traversal-fn env p)]
                               (when form
                                 (let [[_ args & body] form]
                                   (list* (node-fns (:id p)) args body)))))
                           nodes))
        root-pattern (:root trav)
        input-sym (gensym "input")
        root-expr (emit-pattern-value env root-pattern input-sym)]
    `(letfn [~@fn-defs]
       (fn [~input-sym] ~root-expr))))

(defn- compile-over-form
  "Return the compiled function form for selector+body.
   Intended for macro use and testing."
  [sel body]
  (codegen* (plan-over* sel body)))

;; Public API

(defmacro compile-over
  "Compile a selector + body into a function of one argument.
   The function expects the selector's first source value as its argument."
  [sel body]
  (compile-over-form sel body))

(defmacro over
  "Apply a compiled selector+body to the selector's source expression."
  [sel body]
  (let [sel-ast (parse-selector sel)
        source (:source (first (:steps sel-ast)))]
    `((compile-over ~sel ~body) ~source)))

(defn over-plan
  "Return the compiler plan for selector+body as data."
  [sel body]
  (plan-over* sel body))
