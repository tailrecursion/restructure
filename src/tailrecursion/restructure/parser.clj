(ns tailrecursion.restructure.parser)

(def ^:dynamic *parser-ns* nil)

(defn- resolve-parser
  [p]
  (let [parser (cond (fn? p) p
                     (var? p) @p
                     (symbol? p) (let [ns (or *parser-ns* *ns*)
                                       v (ns-resolve ns p)]
                                   (when-not v
                                     (throw (ex-info "Unknown parser"
                                                     {:parser p,
                                                      :ns (ns-name ns)})))
                                   @v)
                     :else (throw (ex-info "Invalid parser" {:parser p})))]
    (when-not (fn? parser)
      (throw (ex-info "Parser is not a function" {:parser p, :value parser})))
    parser))

(defn succeed [v] (fn [tokens] [true v tokens]))

(defn fail ([] (fail nil)) ([_] (fn [tokens] [false nil tokens])))

(defn satisfy
  [pred]
  (fn [tokens]
    (let [t (first tokens)]
      (if (and (some? t) (pred t)) [true t (rest tokens)] [false nil tokens]))))

(defn token [x] (satisfy #(= x %)))

(defn map-result
  [f p]
  (fn [tokens]
    (let [[ok v remaining] ((resolve-parser p) tokens)]
      (if ok [true (f v) remaining] [false nil tokens]))))

(defn consecutive
  [f & ps]
  (fn [tokens]
    (loop [ps ps
           acc []
           ts tokens]
      (if (empty? ps)
        [true (apply f acc) ts]
        (let [[ok v remaining] ((resolve-parser (first ps)) ts)]
          (if ok
            (recur (rest ps) (conj acc v) remaining)
            [false nil tokens]))))))

(defn alternative
  [& ps]
  (fn [tokens]
    (loop [ps ps]
      (if (empty? ps)
        [false nil tokens]
        (let [[ok v remaining] ((resolve-parser (first ps)) tokens)]
          (if ok [true v remaining] (recur (rest ps))))))))

(defn optional
  ([p] (optional p nil))
  ([p default]
   (fn [tokens]
     (let [[ok v remaining] ((resolve-parser p) tokens)]
       (if ok [true v remaining] [true default tokens])))))

(defn many
  [p]
  (fn [tokens]
    (loop [ts tokens
           acc []]
      (let [[ok v remaining] ((resolve-parser p) ts)]
        (if ok
          (if (identical? ts remaining)
            (throw (ex-info "Parser did not consume input"
                            {:parser p, :tokens ts}))
            (recur remaining (conj acc v)))
          [true acc ts])))))

(defn many1 [p] (consecutive (fn [x xs] (cons x xs)) p (many p)))

(defn parse-all
  [p tokens]
  (let [[ok v remaining] ((resolve-parser p) tokens)] [ok v remaining]))

(defmacro define-parser
  [name parser-expr]
  `(def ~name
     (let [parser# ~parser-expr
           ns# (the-ns '~(ns-name *ns*))]
       (fn [tokens#] (binding [*parser-ns* ns#] (parser# tokens#))))))
