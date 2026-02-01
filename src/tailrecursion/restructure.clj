(ns tailrecursion.restructure
  "Rewrite nested Clojure data with a declared shape."
  (:require [clojure.string :as str]
            [tailrecursion.restructure.parser :as p]
            [tailrecursion.restructure.clos :as clos]))

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

(defn ^:no-doc equiv-with-meta? [a b] (and (= a b) (= (meta a) (meta b))))

(defn- unchanged-coll?
  [coll out]
  (and (= (count coll) (count out))
       (every? true? (map equiv-with-meta? coll out))))

(clos/defgeneric traverse-seq*)

(clos/defmethod traverse-seq* [(coll (pred set?)) f _path]
  (let [out (reduce (fn [acc x]
                      (let [y (f x)] (if (elide? y) acc (conj acc y))))
              (empty coll)
              coll)]
    (if (= out coll) coll out)))

(clos/defmethod traverse-seq* [(coll (pred vector?)) f _path]
  (let [cnt (count coll)
        out (loop [i 0
                   acc (transient [])]
              (if (< i cnt)
                (let [x (nth coll i)
                      y (f x)]
                  (recur (inc i) (if (elide? y) acc (conj! acc y))))
                (persistent! acc)))]
    (if (unchanged-coll? coll out) coll out)))

(clos/defmethod traverse-seq* [(coll (pred list?)) f _path]
  (let [outv (reduce (fn [acc x]
                       (let [y (f x)] (if (elide? y) acc (conj acc y))))
               []
               coll)
        out (apply list outv)]
    (if (unchanged-coll? coll outv) coll out)))

(clos/defmethod traverse-seq* [(coll (pred seq?)) f _path]
  (let [outv (reduce (fn [acc x]
                       (let [y (f x)] (if (elide? y) acc (conj acc y))))
               []
               coll)
        out (seq outv)]
    (if (unchanged-coll? coll outv) coll out)))

(defn ^:no-doc traverse-seq
  "Traverse a sequential or set, applying f to each element.
   f returns elide to remove an element."
  [coll f path]
  (if (nil? coll)
    coll
    (do (ensure-seqable coll path) (traverse-seq* coll f path))))

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

(defn- make-bind [sym path form] {:op :bind, :sym sym, :path path, :form form})

(defn- make-seq-elems
  [pat path form]
  {:op :seq-elems, :pat pat, :path path, :form form})

(defn- make-map-entries
  [kpat vpat path form]
  {:op :map-entries, :kpat kpat, :vpat vpat, :path path, :form form})

(defn- make-destructure
  [m path entries]
  {:op :destructure,
   :keys (vec (or (get m :keys) [])),
   :strs (vec (or (get m :strs) [])),
   :entries entries,
   :as (get m :as),
   :or (get m :or),
   :path path,
   :form m})

(defn- make-step
  [pattern source path]
  {:pattern pattern, :source source, :path path})

(defn- destructure-ctx
  [m path]
  {:pattern m, :path path, :expected :destructure-map})

(clos/defgeneric validate-destructure {:combination :list})

(clos/defmethod validate-destructure [(ctx (pred #(contains? (:pattern %)
                                                             :keys)))]
  (let [{:keys [pattern]} ctx
        ks (get pattern :keys)]
    (when-not (vector? ks)
      (error :parse "Expected :keys vector" (assoc ctx :expected :keys-vector)))
    (when (some (fn [s] (not (symbol? s))) ks)
      (error :parse
             "Expected symbols in :keys"
             (assoc ctx :expected :symbols)))))

(clos/defmethod validate-destructure [(ctx (pred #(contains? (:pattern %)
                                                             :strs)))]
  (let [{:keys [pattern]} ctx
        ss (get pattern :strs)]
    (when-not (vector? ss)
      (error :parse "Expected :strs vector" (assoc ctx :expected :strs-vector)))
    (when (some (fn [s] (not (symbol? s))) ss)
      (error :parse
             "Expected symbols in :strs"
             (assoc ctx :expected :symbols)))))

(clos/defmethod validate-destructure [(ctx (pred #(contains? (:pattern %)
                                                             :as)))]
  (let [{:keys [pattern]} ctx
        as (get pattern :as)]
    (when (and (some? as) (not (symbol? as)))
      (error :parse "Expected symbol for :as" (assoc ctx :expected :symbol)))))

(clos/defmethod validate-destructure [(ctx (pred #(contains? (:pattern %)
                                                             :or)))]
  (let [{:keys [pattern]} ctx
        orv (get pattern :or)]
    (when (and (some? orv) (not (map? orv)))
      (error :parse "Expected map for :or" (assoc ctx :expected :map)))))

(clos/defmethod validate-destructure [ctx] ctx)

(defn- parse-destructure
  [m path]
  (let [ctx (destructure-ctx m path)
        _ (validate-destructure ctx)
        entries (into {}
                      (for [[k v] m
                            :when (and (not (contains? destructure-special-keys
                                                       k))
                                       (symbol? k))]
                        [k v]))]
    (doseq [[k v] entries]
      (when-not (or (keyword? v) (string? v))
        (error :parse
               "Destructure entry values must be keywords or strings"
               {:path path,
                :pattern m,
                :key k,
                :value v,
                :expected :keyword-or-string})))
    (doseq [[k _] m
            :when (and (not (contains? destructure-special-keys k))
                       (not (symbol? k)))]
      (error :parse
             "Destructure entry keys must be symbols"
             {:path path, :pattern m, :key k, :expected :symbol}))
    (make-destructure m path entries)))

(p/define-parser symbol-pattern-parser
                 (p/map-result (fn [{:keys [form path]}]
                                 (make-bind form path form))
                               (p/satisfy #(symbol? (:form %)))))

(declare vector-pattern-parser
         destructure-pattern-parser
         map-entry-pattern-parser
         pattern-parser)

(defn- parse-pattern-form
  [form path]
  (let [[ok ast] (p/parse-all pattern-parser {:form form, :path path})]
    (when-not ok
      (error :parse
             "Unsupported pattern"
             {:path path, :pattern form, :expected :pattern}))
    ast))

(p/define-parser
  vector-pattern-parser
  (p/map-result
    (fn [{:keys [form path]}]
      (when (not= 1 (count form))
        (error :parse
               "Only single-element vector patterns are supported"
               {:path path, :pattern form, :expected :single-vector}))
      (make-seq-elems (parse-pattern-form (first form) (conj path 0))
                      path
                      form))
    (p/satisfy #(vector? (:form %)))))

(p/define-parser
  destructure-pattern-parser
  (p/map-result
    (fn [{:keys [form path]}]
      (when (some (fn [[k _]]
                    (and (not (contains? destructure-special-keys k))
                         (not (symbol? k))))
                  form)
        (error :parse
               "Destructure entry keys must be symbols"
               {:path path, :pattern form, :expected :symbol}))
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
        (make-map-entries kpat vpat path form)))
    (p/satisfy #(and (map? (:form %)) (not (destructure-map? (:form %)))))))

(p/define-parser pattern-parser
                 (p/alternative 'symbol-pattern-parser
                                'vector-pattern-parser
                                'destructure-pattern-parser
                                'map-entry-pattern-parser))

(defn- validate-selector
  [sel]
  (when-not (vector? sel)
    (error :parse
           "Selector must be a vector"
           {:selector sel, :expected :vector}))
  (when (odd? (count sel))
    (error :parse
           "Selector must have even number of forms"
           {:selector sel, :expected :pattern-source-pairs}))
  sel)

(defn- parse-selector
  [sel]
  (let [_ (validate-selector sel)
        pairs (partition 2 sel)
        steps (mapv (fn [i [pat src]]
                      (let [path [:step i]
                            pat-ast (parse-pattern-form pat path)]
                        (make-step pat-ast src path)))
                (range)
                pairs)]
    {:selector sel, :steps steps}))

;; Body analysis

(defn ^:no-doc guard-symbol?
  [s]
  (and (symbol? s) (nil? (namespace s)) (str/ends-with? (name s) "?")))

(defn- guard->target [s] (symbol (subs (name s) 0 (dec (count (name s))))))

(defn- body-entry-ctx
  [k v bindings]
  (when-not (symbol? k)
    (error :validate "Body keys must be symbols" {:body k, :key k}))
  (when (namespace k)
    (error :validate "Body keys must be unqualified symbols" {:body k, :key k}))
  {:key k, :val v, :bindings bindings})

(clos/defgeneric analyze-body-entry)

(clos/defmethod analyze-body-entry [(entry (pred (fn [{:keys [key]}]
                                                   (guard-symbol? key))))
                                    rewrites guards guard-nodes]
  (let [{:keys [key val bindings]} entry
        tgt (guard->target key)]
    (when-not (contains? bindings tgt)
      (error :validate "Guard symbol is not bound" {:binding tgt, :key key}))
    (when (contains? @guards tgt)
      (error :validate "Duplicate guard" {:binding tgt}))
    (swap! guards assoc tgt val)
    (swap! guard-nodes assoc
      tgt
      {:sym tgt, :expr val, :path (:path (bindings tgt))})))

(clos/defmethod analyze-body-entry [entry rewrites guards _guard-nodes]
  (let [{:keys [key val bindings]} entry]
    (when-not (contains? bindings key)
      (error :validate "Rewrite symbol is not bound" {:binding key, :key key}))
    (when (contains? @rewrites key)
      (error :validate "Duplicate rewrite" {:binding key}))
    (swap! rewrites assoc key val)))

(defn- analyze-body
  [body bindings]
  (when-not (map? body)
    (error :validate "Body must be a map literal" {:body body, :expected :map}))
  (let [rewrites (atom {})
        guards (atom {})
        guard-nodes (atom {})]
    (doseq [[k v] body]
      (analyze-body-entry (body-entry-ctx k v bindings)
                          rewrites
                          guards
                          guard-nodes))
    {:rewrites @rewrites, :guards @guards, :guard-nodes @guard-nodes}))

(clos/defgeneric binding-conflicts {:combination :list})

(clos/defmethod binding-conflicts [(info (pred #(= :map (:kind %)))) sym
                                   bindings rewrites]
  (when (contains? rewrites sym)
    (let [node-path (:path (bindings sym))
          key-rewrites (filter (fn [[s i]]
                                 (and (= (:path i) node-path)
                                      (= (:kind i) :map-key)
                                      (contains? rewrites s)))
                         bindings)]
      (when (seq key-rewrites)
        (error :validate
               "Conflicting rewrites: :as and :keys"
               {:binding sym, :path node-path})))))

(clos/defmethod binding-conflicts [_info _sym _bindings _rewrites] nil)

(defn- detect-conflicts
  [bindings rewrites]
  (doseq [[sym info] bindings] (binding-conflicts info sym bindings rewrites)))

;; Pass 5: usage analysis (conservative)

(defn ^:no-doc unqualified-symbol? [s] (and (symbol? s) (nil? (namespace s))))

(clos/defgeneric collect-symbols*)

(clos/defmethod collect-symbols* [(f (pred unqualified-symbol?)) syms]
  (conj syms f))

(clos/defmethod collect-symbols* [(f (pred seq?)) syms]
  (let [op (first f)]
    (if (= 'quote op)
      syms
      (reduce (fn [acc x] (collect-symbols* x acc)) syms f))))

(clos/defmethod collect-symbols* [(f (pred coll?)) syms]
  (reduce (fn [acc x] (collect-symbols* x acc)) syms f))

(clos/defmethod collect-symbols* [_ syms] syms)

(defn- collect-symbols [form] (collect-symbols* form #{}))

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

(defn- node-ctx
  [pat]
  (let [path (:path pat)
        ctx {:pattern (:form pat), :op (:op pat), :path path}]
    (validate-path path ctx)
    ctx))

(clos/defgeneric validate-node {:combination :list})

(clos/defmethod validate-node [(pat (key= :op :bind))]
  (let [ctx (node-ctx pat)]
    (when-not (symbol? (:sym pat)) (error :validate "Invalid bind node" ctx))
    pat))

(clos/defmethod validate-node [(pat (key= :op :seq-elems))]
  (let [ctx (node-ctx pat)]
    (when-not (:pat pat) (error :validate "Missing :pat in seq node" ctx))
    pat))

(clos/defmethod validate-node [(pat (key= :op :map-entries))]
  (let [ctx (node-ctx pat)]
    (when-not (:kpat pat)
      (error :validate "Missing :kpat in map-entry node" ctx))
    (when-not (:vpat pat)
      (error :validate "Missing :vpat in map-entry node" ctx))
    pat))

(clos/defmethod validate-node [(pat (key= :op :destructure))]
  (node-ctx pat)
  pat)

(clos/defmethod validate-node [pat] nil)

(defn- validate-node!
  [pat]
  (let [vals (validate-node pat)]
    (when-not (#{:bind :seq-elems :map-entries :destructure} (:op pat))
      (error :validate "Unknown pattern op" (node-ctx pat)))
    (or (first (remove nil? vals)) pat)))

(defn- binding-info
  [sym kind path & {:keys [key]}]
  {:sym sym, :kind kind, :path path, :key key})

(clos/defgeneric pat-bindings)

(clos/defmethod pat-bindings [(pat (key= :op :bind))]
  (let [pat (validate-node! pat)
        sym (:sym pat)
        path (:path pat)]
    (if (= '_ sym) [] [(binding-info sym :value path)])))

(clos/defmethod pat-bindings [(pat (key= :op :destructure))]
  (let [pat (validate-node! pat)
        ks (:keys pat)
        strs (:strs pat)
        entries (:entries pat)
        as (:as pat)
        path (:path pat)]
    (vec (concat (for [s ks
                       :when (not= '_ s)]
                   (binding-info s :map-key path :key (keyword (name s))))
                 (for [s strs
                       :when (not= '_ s)]
                   (binding-info s :map-key path :key (name s)))
                 (for [s (keys entries)
                       :when (not= '_ s)]
                   (binding-info s :map-key path :key (get entries s)))
                 (when (and as (not= '_ as)) [(binding-info as :map path)])))))

(clos/defmethod pat-bindings [(pat (key= :op :seq-elems))]
  (let [pat (validate-node! pat)] (pat-bindings (:pat pat))))

(clos/defmethod pat-bindings [(pat (key= :op :map-entries))]
  (let [pat (validate-node! pat)]
    (vec (concat (pat-bindings (:kpat pat)) (pat-bindings (:vpat pat))))))

(clos/defmethod pat-bindings [pat]
  (error :validate "Unknown pattern op" {:pattern pat}))

;; Binding analysis

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

(clos/defgeneric emit-with-pattern)

(defn- emit-pattern-value
  [env pat value-expr]
  (emit-with-pattern env pat value-expr (fn [v] v)))

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
          :else
            `(let [~v# ~value-expr
                   ~v# ~(if (seq children)
                          (emit-apply-children env v# children)
                          v#)]
               (if (elide? ~v#)
                 elide
                 (let [~sym ~v#]
                   (let [keep?# ~(if guard-present? guard-expr true)]
                     (if keep?#
                       (let [rewrite# ~(if rewrite-present? rewrite-expr sym)
                             ~sym (if (equiv-with-meta? rewrite# ~sym)
                                    ~sym
                                    rewrite#)
                             ~out# ~sym]
                         ~(body-fn out#))
                       elide))))))))

(defn- destructure-prep
  [ctx]
  (let [{:keys [pat]} ctx
        ks (:keys pat)
        ss (:strs pat)
        as (:as pat)
        orv (:or pat)
        entries (:entries pat)
        dform (cond-> (or entries {})
                (seq ks) (assoc :keys ks)
                (seq ss) (assoc :strs ss)
                as (assoc :as as)
                orv (assoc :or orv))
        all-syms (vec (concat ks ss (keys entries)))
        key->k (into {}
                     (concat (map (fn [s] [s (keyword (name s))]) ks)
                             (map (fn [s] [s (name s)]) ss)
                             entries))
        v# (gensym "m")
        had-syms (zipmap all-syms
                         (map (fn [s] (gensym (str "had_" (name s)))) all-syms))
        key-drop-syms (zipmap all-syms
                              (map (fn [s] (gensym (str (name s) "_drop")))
                                all-syms))]
    (assoc ctx
      :ks ks
      :ss ss
      :as as
      :orv orv
      :dform dform
      :all-syms all-syms
      :key->k key->k
      :v# v#
      :had-syms had-syms
      :key-drop-syms key-drop-syms)))

(defn- destructure-had
  [ctx]
  (let [{:keys [all-syms key->k v# had-syms]} ctx
        had-bindings (vec (mapcat (fn [s] [(get had-syms s)
                                           `(contains? ~v# ~(get key->k s))])
                            all-syms))]
    (assoc ctx :had-bindings had-bindings)))

(defn- destructure-children
  [ctx]
  (let [{:keys [env all-syms]} ctx
        key-children (fn [s] (get-in env [:binding-children s]))
        child-bindings
          (vec (mapcat (fn [s] [s
                                (if (seq (key-children s))
                                  (emit-apply-children env s (key-children s))
                                  s)])
                 all-syms))]
    (assoc ctx :child-bindings child-bindings)))

(defn- destructure-guards
  [ctx]
  (let [{:keys [env all-syms key-drop-syms]} ctx
        key-bindings (vec
                       (mapcat
                         (fn [s]
                           (let [guard-present? (contains? (:guards env) s)
                                 guard-expr (get-in env [:guards s])
                                 rewrite-present? (contains? (:rewrites env) s)
                                 rewrite-expr (get-in env [:rewrites s])
                                 drop-sym (get key-drop-syms s)
                                 rewrite-sym (gensym (str (name s) "_rewrite"))]
                             [drop-sym
                              `(or (elide? ~s)
                                   (not ~(if guard-present? guard-expr true)))
                              rewrite-sym (if rewrite-present? rewrite-expr s) s
                              `(if ~drop-sym
                                 ~s
                                 (if (equiv-with-meta? ~rewrite-sym ~s)
                                   ~s
                                   ~rewrite-sym))]))
                         all-syms))]
    (assoc ctx :key-bindings key-bindings)))

(defn- destructure-map
  [ctx]
  (let [{:keys [env all-syms key->k v# had-syms key-drop-syms]} ctx
        m1# (gensym "m1")
        map-bindings (vec
                       (conj (reduce
                               (fn [acc s]
                                 (let [drop-sym (get key-drop-syms s)
                                       had? (get had-syms s)
                                       k (get key->k s)
                                       rewrite? (contains? (:rewrites env) s)
                                       orig (gensym (str (name s) "_orig"))]
                                   (conj acc
                                         orig
                                         `(get ~v# ~k)
                                         m1#
                                         `(if ~drop-sym
                                            (if ~had? (dissoc ~m1# ~k) ~m1#)
                                            (if (or ~rewrite?
                                                    (and ~had? (not= ~s ~orig)))
                                              (assoc ~m1# ~k ~s)
                                              ~m1#)))))
                               [m1# v#]
                               all-syms)
                             m1#
                             `(if (equiv-with-meta? ~m1# ~v#) ~v# ~m1#)))]
    (assoc ctx
      :map-bindings map-bindings
      :m1# m1#)))

(defn- destructure-as
  [ctx]
  (let [{:keys [env as m1# body-fn]} ctx
        as-expr
          (if as
            (let [as-children (get-in env [:binding-children as])
                  as-guard-present? (contains? (:guards env) as)
                  as-guard (get-in env [:guards as])
                  as-rewrite-present? (contains? (:rewrites env) as)
                  as-rewrite (get-in env [:rewrites as])
                  as-drop-sym (gensym "as_drop")
                  as-val-sym (gensym "as_val")
                  as-child-expr (if (seq as-children)
                                  (emit-apply-children env as as-children)
                                  as)
                  as-guard-expr (if as-guard-present? as-guard true)
                  as-rewrite-expr (if as-rewrite-present? as-rewrite as)
                  elide?-sym 'tailrecursion.restructure/elide?
                  elide-sym 'tailrecursion.restructure/elide]
              (list 'let
                    [as m1# as as-child-expr as-drop-sym
                     (list 'or (list elide?-sym as) (list 'not as-guard-expr))
                     as (list 'if as-drop-sym as as-rewrite-expr) as-val-sym as]
                    (list 'if as-drop-sym elide-sym (body-fn as-val-sym))))
            (body-fn m1#))]
    (assoc ctx :as-expr as-expr)))

(defn- destructure-emit
  [ctx]
  (let [{:keys [pat value-expr dform v# had-bindings child-bindings key-bindings
                map-bindings as-expr]}
          ctx]
    (assoc ctx
      :form `(let [~v# ~value-expr]
               (if (nil? ~v#)
                 nil
                 (do (ensure-map ~v# ~(node-path pat))
                     (let [~@had-bindings]
                       (let [~dform ~v#]
                         (let [~@child-bindings]
                           (let [~@key-bindings]
                             (let [~@map-bindings] ~as-expr)))))))))))

(defn- emit-destructure-scope
  [env pat value-expr body-fn]
  (let [ctx (reduce (fn [c f] (f c))
              {:env env, :pat pat, :value-expr value-expr, :body-fn body-fn}
              [destructure-prep destructure-had destructure-children
               destructure-guards destructure-map destructure-as
               destructure-emit])]
    (:form ctx)))

(clos/defmethod emit-with-pattern [env (pat (key= :op :bind)) value-expr
                                   body-fn]
  (emit-bind-scope env (:sym pat) value-expr body-fn))

(clos/defmethod emit-with-pattern [env (pat (key= :op :destructure)) value-expr
                                   body-fn]
  (emit-destructure-scope env pat value-expr body-fn))

(clos/defmethod emit-with-pattern [env (pat (key= :op :seq-elems)) value-expr
                                   body-fn]
  (let [f (get-in env [:node-fns (:id pat)])
        v# (gensym "v")]
    `(let [~v# (~f ~value-expr)] (if (elide? ~v#) elide ~(body-fn v#)))))

(clos/defmethod emit-with-pattern [env (pat (key= :op :map-entries)) value-expr
                                   body-fn]
  (let [f (get-in env [:node-fns (:id pat)])
        v# (gensym "v")]
    `(let [~v# (~f ~value-expr)] (if (elide? ~v#) elide ~(body-fn v#)))))

(clos/defmethod emit-with-pattern [env pat value-expr body-fn]
  (error :codegen "Unknown pattern op" {:pattern pat}))

(clos/defgeneric assign-ids)

(clos/defmethod assign-ids [(pat (key= :op :seq-elems)) id nodes]
  (let [nid (swap! id inc)
        child (assign-ids (:pat pat) id nodes)
        p' (assoc pat
             :id nid
             :pat child)]
    (swap! nodes conj p')
    p'))

(clos/defmethod assign-ids [(pat (key= :op :map-entries)) id nodes]
  (let [nid (swap! id inc)
        kpat (assign-ids (:kpat pat) id nodes)
        vpat (assign-ids (:vpat pat) id nodes)
        p' (assoc pat
             :id nid
             :kpat kpat
             :vpat vpat)]
    (swap! nodes conj p')
    p'))

(clos/defmethod assign-ids [(pat (key= :op :bind)) _id _nodes] pat)
(clos/defmethod assign-ids [(pat (key= :op :destructure)) _id _nodes] pat)
(clos/defmethod assign-ids [pat _id _nodes] pat)

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

(clos/defgeneric emit-traversal-fn)

(clos/defmethod emit-traversal-fn [env (pat (key= :op :seq-elems))]
  (let [elem-sym (gensym "elem")
        f-sym (gensym "seqnode")]
    {:sym f-sym,
     :form `(fn [coll#]
              (traverse-seq coll#
                            (fn [~elem-sym]
                              ~(emit-pattern-value env (:pat pat) elem-sym))
                            ~(node-path pat)))}))

(clos/defmethod emit-traversal-fn [env (pat (key= :op :map-entries))]
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

(clos/defmethod emit-traversal-fn [_env _pat] nil)

(defn- run-passes [ctx passes] (reduce (fn [c f] (f c)) ctx passes))

(defn- make-plan
  [ctx]
  (let [parse-pass (:parse ctx)
        binding-pass (:binding-pass ctx)
        body-pass (:body-pass ctx)]
    {:v 1,
     :selector (:selector parse-pass),
     :steps (:steps parse-pass),
     :bindings (:bindings binding-pass),
     :binding-children (:binding-children binding-pass),
     :rewrites (:rewrites body-pass),
     :guards (:guards body-pass),
     :guard-nodes (:guard-nodes body-pass),
     :used-syms (:used-syms ctx)}))

(defn- pass-parse [ctx] (assoc ctx :parse (parse-selector (:sel ctx))))

(defn- pass-bindings
  [ctx]
  (assoc ctx :binding-pass (analyze-bindings (:parse ctx))))

(defn- pass-body
  [ctx]
  (assoc ctx
    :body-pass (analyze-body (:body ctx) (:bindings (:binding-pass ctx)))))

(defn- pass-conflicts
  [ctx]
  (detect-conflicts (:bindings (:binding-pass ctx))
                    (:rewrites (:body-pass ctx)))
  ctx)

(defn- pass-usage
  [ctx]
  (let [parse-pass (:parse ctx)
        body-pass (:body-pass ctx)]
    (assoc ctx
      :used-syms (analyze-usage (:body ctx)
                                (:rewrites body-pass)
                                (:guards body-pass)
                                (:steps parse-pass)))))

(def ^:private plan-passes
  [pass-parse pass-bindings pass-body pass-conflicts pass-usage])

(defn- plan-over*
  "Compile-time planner: parse, validate, and analyze selector/body into a plan."
  [sel body]
  (let [ctx (run-passes {:sel sel, :body body} plan-passes)] (make-plan ctx)))

(defn- codegen-annotate
  [ctx]
  (let [plan (:plan ctx)
        root (:pattern (first (:steps plan)))
        trav (collect-all-traversals root (:binding-children plan))
        nodes (:nodes trav)
        node-fns (into {}
                       (map (fn [p] [(:id p)
                                     (gensym (str (name (:op p)) "_fn"))])
                         nodes))
        env (assoc plan
              :node-fns node-fns
              :binding-children (:binding-children trav))]
    (assoc ctx
      :trav trav
      :nodes nodes
      :node-fns node-fns
      :env env)))

(defn- codegen-lower
  [ctx]
  (let [{:keys [env nodes node-fns trav]} ctx
        fn-defs (vec (keep (fn [p]
                             (let [{:keys [form]} (emit-traversal-fn env p)]
                               (when form
                                 (let [[_ args & body] form]
                                   (list* (node-fns (:id p)) args body)))))
                           nodes))
        root-pattern (:root trav)
        input-sym (gensym "input")
        root-expr (emit-pattern-value env root-pattern input-sym)]
    (assoc ctx
      :ir {:fn-defs fn-defs, :root-expr root-expr, :input-sym input-sym})))

(defn- codegen-emit
  [ctx]
  (let [{:keys [fn-defs root-expr input-sym]} (:ir ctx)]
    (assoc ctx
      :form `(letfn [~@fn-defs]
               (fn [~input-sym] ~root-expr)))))

(def ^:private codegen-passes [codegen-annotate codegen-lower codegen-emit])

(defn- codegen*
  "Generate a compiled function form from a plan."
  [plan]
  (let [ctx (run-passes {:plan plan} codegen-passes)] (:form ctx)))

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

(defn- selector-with-topic
  [topic sel]
  (when-not (vector? sel)
    (error :parse
           "Selector must be a vector"
           {:selector sel, :expected :vector}))
  (when-not (odd? (count sel))
    (error :parse
           "Selector must omit first source when topic is provided"
           {:selector sel, :expected :pattern-then-pairs}))
  (let [p1 (first sel) rest (next sel)] (vec (concat [p1 topic] rest))))

(defmacro over->
  "Thread-first helper. Injects the threaded value as the first source."
  [x sel body]
  `(over ~(selector-with-topic x sel) ~body))

(defmacro over->>
  "Thread-last helper. Injects the threaded value as the first source."
  [sel body x]
  `(over ~(selector-with-topic x sel) ~body))

(defn over-plan
  "Return the compiler plan for selector+body as data."
  [sel body]
  (plan-over* sel body))
