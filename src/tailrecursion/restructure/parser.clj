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

(defn succeed [v] (fn [_] [true v]))

(defn fail ([] (fail nil)) ([_] (fn [_] [false nil])))

(defn satisfy [pred] (fn [node] (if (pred node) [true node] [false nil])))

(defn map-result
  [f p]
  (fn [node]
    (let [[ok v] ((resolve-parser p) node)] (if ok [true (f v)] [false nil]))))

(defn alternative
  [& ps]
  (fn [node]
    (loop [ps ps]
      (if (empty? ps)
        [false nil]
        (let [[ok v] ((resolve-parser (first ps)) node)]
          (if ok [true v] (recur (rest ps))))))))

(defn parse-all [p node] ((resolve-parser p) node))

(defmacro define-parser
  [name parser-expr]
  `(def ~name
     (let [parser# ~parser-expr
           ns# (the-ns '~(ns-name *ns*))]
       (fn [tokens#] (binding [*parser-ns* ns#] (parser# tokens#))))))
