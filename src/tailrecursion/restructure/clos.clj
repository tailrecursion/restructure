(ns tailrecursion.restructure.clos
  "CLOS-style generic functions and method combinations."
  (:refer-clojure :exclude [defmethod]))

(def ^:dynamic *next-methods* nil)
(def ^:dynamic *next-method-args* nil)

(defn next-method? [] (boolean (seq *next-methods*)))

(defn call-next-method
  ([& args]
   (let [next-fn (first *next-methods*)
         args (if (seq args) args *next-method-args*)]
     (when-not next-fn (throw (ex-info "No next method" {:phase :dispatch})))
     (binding [*next-methods* (rest *next-methods*)
               *next-method-args* args]
       (apply next-fn args)))))

(defn- ensure-generic
  [v name]
  (when-not (and v
                 (-> (meta v)
                     :restructure/generic))
    (throw (ex-info "Not a generic function" {:name name}))))

(defn- resolve-class
  [sym]
  (let [v (resolve sym)
        c (cond (instance? Class v) v
                (and (var? v) (instance? Class @v)) @v
                :else nil)]
    c))

(defn- specializer-class [spec] (:class spec))

(defn- specializer-kind [spec] (:kind spec))

(defn- safe-apply [f x] (try (f x) (catch Throwable _ false)))

(defn- applicable?
  [spec arg]
  (case (specializer-kind spec)
    :any true
    :eql (= (:value spec) arg)
    :in (contains? (:set spec) arg)
    :key= (and (map? arg) (= (:value spec) (get arg (:key spec))))
    :keys (and (map? arg) (every? #(contains? arg %) (:keys spec)))
    :keys= (and (map? arg) (= (:keys spec) (set (keys arg))))
    :map-of (and (map? arg)
                 (every? (fn [[k v]]
                           (and (safe-apply (:kpred spec) k)
                                (safe-apply (:vpred spec) v)))
                         arg))
    :class (instance? (specializer-class spec) arg)
    :pred (safe-apply (:pred spec) arg)
    :satisfies (safe-apply (partial satisfies? (:protocol spec)) arg)
    false))

(defn- class-distance
  [^Class actual ^Class target]
  (when (and actual target (.isAssignableFrom target actual))
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [actual 0])
           seen #{}]
      (when-let [[c d] (peek queue)]
        (if (= c target)
          d
          (let [queue (pop queue)
                supers (remove nil?
                         (concat (when-let [s (.getSuperclass c)] [s])
                                 (.getInterfaces c)))
                nexts (remove seen supers)
                queue (reduce (fn [q n] (conj q [n (inc d)])) queue nexts)
                seen (into seen nexts)]
            (recur queue seen)))))))

(defn- specificity-score
  [spec arg]
  (case (specializer-kind spec)
    :eql [0 0]
    :in [1 0]
    :key= [2 0]
    :keys [3 0]
    :keys= [4 0]
    :map-of [5 0]
    :pred [6 0]
    :satisfies [6 0]
    :class [7 (class-distance (class arg) (specializer-class spec))]
    :any [8 0]
    [9 0]))

(defn- method-score
  [method args]
  (mapv specificity-score (:specializers method) args))

(defn- more-specific?
  [m1 m2 args]
  (let [s1 (method-score m1 args)
        s2 (method-score m2 args)
        cmp (compare s1 s2)]
    (if (zero? cmp) (< (:index m1) (:index m2)) (neg? cmp))))

(defn- sort-methods [methods args] (sort #(more-specific? %1 %2 args) methods))

(defn- no-next-method
  [& _]
  (throw (ex-info "No next method" {:phase :dispatch})))

(defn- method-chain
  [methods base]
  (if (empty? methods)
    base
    (let [method (first methods)
          next-fn (method-chain (rest methods) base)]
      (fn [& args]
        (binding [*next-methods* (if next-fn (list next-fn) nil)
                  *next-method-args* args]
          (apply (:fn method) args))))))

(defn- ensure-arity
  [arity args]
  (let [actual (count args)]
    (when (and arity (not= arity actual))
      (throw (ex-info "Arity mismatch" {:expected arity, :actual actual})))
    true))

(defn- combine-standard
  [methods args]
  (let [applicable (filter #(every? true?
                                    (map applicable? (:specializers %) args))
                     methods)
        grouped (group-by :qualifier applicable)
        arounds (sort-methods (get grouped :around []) args)
        befores (sort-methods (get grouped :before []) args)
        afters (reverse (sort-methods (get grouped :after []) args))
        primaries (sort-methods (get grouped :primary []) args)
        primary-fn (method-chain primaries no-next-method)
        base (fn [& as]
               (when-not primary-fn
                 (throw (ex-info "No applicable primary methods"
                                 {:phase :dispatch})))
               (doseq [m befores] (apply (:fn m) as))
               (let [ret (apply primary-fn as)]
                 (doseq [m afters] (apply (:fn m) as))
                 ret))]
    (if (seq arounds)
      (apply (method-chain arounds base) args)
      (apply base args))))

(defn- combine-simple
  [methods args op]
  (let [primaries (sort-methods (filter #(and (= :primary (:qualifier %))
                                              (every? true?
                                                      (map applicable?
                                                        (:specializers %)
                                                        args)))
                                  methods)
                                args)
        values (map #(apply (:fn %) args) primaries)]
    (case op
      :list (vec values)
      :and (every? true? values)
      :or (boolean (some true? values))
      :+ (reduce + 0 values)
      :max (when (seq values) (apply max values))
      :min (when (seq values) (apply min values))
      (when-let [f op] (reduce f values)))))

(defn- combine-default
  [methods args]
  (let [primaries (sort-methods (filter #(and (= :primary (:qualifier %))
                                              (every? true?
                                                      (map applicable?
                                                        (:specializers %)
                                                        args)))
                                  methods)
                                args)]
    (when-let [method (first primaries)] (apply (:fn method) args))))

(defn- invoke-generic
  [generic args]
  (let [{:keys [methods combination arity]} @generic
        _ (ensure-arity arity args)]
    (case combination
      :standard (combine-standard methods args)
      :list (combine-simple methods args :list)
      :and (combine-simple methods args :and)
      :or (combine-simple methods args :or)
      :+ (combine-simple methods args :+)
      :max (combine-simple methods args :max)
      :min (combine-simple methods args :min)
      (combine-default methods args))))

(defn ^:no-doc make-generic
  [name opts]
  (let [generic (atom {:name name,
                       :methods [],
                       :next-index 0,
                       :arity nil,
                       :combination (or (:combination opts) :standard)})]
    (with-meta (fn [& args] (invoke-generic generic args))
      {:restructure/generic true, :generic generic, :name name})))

(defn generic?
  [v]
  (let [var (cond (var? v) v
                  (symbol? v) (resolve v)
                  :else nil)
        m (or (some-> var
                      meta)
              (meta v))]
    (boolean (:restructure/generic m))))

(defn generic
  [v]
  (let [var (cond (var? v) v
                  (symbol? v) (resolve v)
                  :else nil)]
    (ensure-generic var v)
    (-> (meta var)
        :generic)))

(defn add-method!
  [v qualifier specializers f]
  (let [var (if (var? v) v (resolve v))]
    (ensure-generic var v)
    (let [g (-> (meta var)
                :generic)
          arity (count specializers)
          method {:qualifier qualifier,
                  :specializers specializers,
                  :fn f,
                  :arity arity,
                  :index (:next-index @g)}]
      (swap! g (fn [m]
                 (when (and (:arity m) (not= (:arity m) arity))
                   (throw (ex-info "Arity mismatch"
                                   {:expected (:arity m), :actual arity})))
                 (-> m
                     (assoc :arity (or (:arity m) arity))
                     (update :methods conj method)
                     (update :next-index inc)))))
    v))

(defmacro defgeneric
  [name & body]
  (let [[doc body]
          (if (string? (first body)) [(first body) (rest body)] [nil body])
        opts (if (map? (first body)) (first body) {})]
    `(let [f# (make-generic '~name ~opts)
           v# (def ~(with-meta name (cond-> {} doc (assoc :doc doc))) f#)]
       (alter-meta! v# merge (meta f#))
       v#)))

(defmacro op [v] `(key= :op ~v))

(defmacro defmethod
  [name & body]
  (let [emit-specializer
          (fn [spec]
            (cond (nil? spec) (throw (ex-info "Nil is not a valid specializer"
                                              {:specializer spec}))
                  (= spec :any) {:kind :any}
                  (= spec 't) {:kind :any}
                  (and (seq? spec) (= 'key= (first spec)))
                    (let [[_ k v] spec] `{:kind :key=, :key ~k, :value ~v})
                  (and (seq? spec) (= 'op (first spec)))
                    (let [v (second spec)] `{:kind :key=, :key :op, :value ~v})
                  (and (seq? spec) (= 'keys= (first spec)))
                    (let [ks (rest spec)] `{:kind :keys=, :keys ~(set ks)})
                  (and (seq? spec) (= 'has-keys (first spec)))
                    (let [ks (rest spec)] `{:kind :keys, :keys ~(vec ks)})
                  (and (seq? spec) (= 'in (first spec)))
                    (let [s (second spec)] `{:kind :in, :set ~s})
                  (and (seq? spec) (= 'map-of (first spec)))
                    (let [[_ kp vp] spec]
                      (cond (and (symbol? kp) (symbol? vp))
                              `{:kind :map-of,
                                :kpred (deref (var ~kp)),
                                :vpred (deref (var ~vp))}
                            (symbol? kp) `{:kind :map-of,
                                           :kpred (deref (var ~kp)),
                                           :vpred ~vp}
                            (symbol? vp) `{:kind :map-of,
                                           :kpred ~kp,
                                           :vpred (deref (var ~vp))}
                            :else `{:kind :map-of, :kpred ~kp, :vpred ~vp}))
                  (and (seq? spec) (= 'eql (first spec)))
                    `{:kind :eql, :value ~(second spec)}
                  (and (seq? spec) (= 'pred (first spec)))
                    (let [p (second spec)]
                      (cond (symbol? p) `{:kind :pred, :pred (deref (var ~p))}
                            :else `{:kind :pred, :pred ~p}))
                  (and (seq? spec) (= 'satisfies (first spec)))
                    (let [p (second spec)]
                      (cond (symbol? p) `{:kind :satisfies,
                                          :protocol (deref (var ~p))}
                            :else `{:kind :satisfies, :protocol ~p}))
                  (symbol? spec) (if-let [c (resolve-class spec)]
                                   {:kind :class, :class c}
                                   (throw (ex-info "Unknown class specializer"
                                                   {:specializer spec})))
                  (instance? Class spec) {:kind :class, :class spec}
                  :else (throw (ex-info "Invalid specializer"
                                        {:specializer spec}))))
        [qualifier body] (if (keyword? (first body))
                           [(first body) (rest body)]
                           [:primary body])
        args (first body)
        method-body (rest body)
        args (vec args)
        parsed (mapv (fn [a]
                       (cond (symbol? a) {:sym a, :spec {:kind :any}}
                             (and (seq? a) (= 2 (count a)) (symbol? (first a)))
                               {:sym (first a),
                                :spec (emit-specializer (second a))}
                             :else (throw (ex-info "Invalid method parameter"
                                                   {:param a}))))
                 args)
        arg-syms (mapv :sym parsed)
        spec-forms (mapv :spec parsed)]
    `(add-method! (var ~name)
                  ~qualifier
                  ~(vec spec-forms)
                  (fn ~arg-syms ~@method-body))))
