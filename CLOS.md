# CLOS

This document describes the generic function system in `tailrecursion.restructure.clos`.
It is a small, CLOS-style dispatcher with method combinations and map-aware
specializers. It is not a full Common Lisp Object System.

## Example

```
(require '[tailrecursion.restructure.clos :as clos])

(defrecord Db [^javax.sql.DataSource ds])

(clos/defgeneric with-db {:combination :standard})
(clos/defgeneric run {:combination :standard})
(clos/defgeneric close! {:combination :standard})

(clos/defmethod with-db :around [(db Db) f]
  ;; mirrors the usual with-open/with-db-transaction shape
  (try
    (f db)
    (finally (close! db))))

(clos/defmethod run :around [(db Db) (req (key= :op :tx))]
  ;; tx wrapper as a method, not manual plumbing
  (try
    (let [ret (run db (:do req))]
      ;; pretend: commit
      ret)
    (catch Throwable t
      ;; pretend: rollback
      (throw t))))

(clos/defmethod run [(db Db) (req (key= :op :query))]
  [:query (:sql req) (:params req)])

(clos/defmethod run [(db Db) (req (key= :op :execute))]
  [:exec (:sql req) (:params req)])

(clos/defmethod run [(db Db) (req (key= :op :raw))]
  [:raw (:sql req)])

(clos/defmethod run [db _]
  (throw (ex-info \"Bad request\" {:db db})))

(clos/defmethod close! [(db Db)] :db-closed)

(def db (->Db nil))

(with-db db #(run % {:op :query :sql \"select * from users where id = ?\" :params [42]}))
;; => [:query \"select * from users where id = ?\" [42]]

(with-db db #(run % {:op :tx :do {:op :execute :sql \"update users set name = ?\" :params [\"A\"]}}))
;; => [:exec \"update users set name = ?\" [\"A\"]]
```

## Terminology

- generic function: a function value created by `defgeneric`.
- method: a single implementation attached to a generic.
- specializer: the per-argument dispatch predicate used to select methods.
- qualifier: a method role in a method combination (`:primary`, `:before`, etc).

## Defining generics

```
(defgeneric name)
(defgeneric name "docstring")
(defgeneric name {:combination :standard})
```

`defgeneric` creates a var bound to a generic function. The optional options map
supports:

- `:combination` — method combination, default `:standard`.

`defgeneric` stores metadata on the var (`:restructure/generic`) and returns the
var.

Options:
- `:combination` — method combination, default `:standard`.
- `:pred-exceptions` — behavior when predicate specializers throw. Defaults to
  `:false` (treat as non-match). Use `:error` to rethrow.

## Defining methods

```
(defmethod name [args] body...)
(defmethod name :before [args] body...)
(defmethod name :after [args] body...)
(defmethod name :around [args] body...)
```

`defmethod` adds a method to a generic. The default qualifier is `:primary`.
The argument vector must consist of symbols or `(sym specializer)` pairs.

Examples of parameter forms:

```
[x]                     ; :any specializer
[x String]              ; class specializer
[x :k]                  ; literal value specializer
[m (key= :op :bind)]    ; map key/value specializer
[m {:op :bind}]         ; map literal specializer
[x #{:warn :error}]     ; set literal specializer
[x (isa? ::thing)]      ; hierarchy-based specializer
```

## Specializers

Specializers are specified in `defmethod` and compile to a specializer map.
If a specializer is provided as a symbol, it resolves to a class at expansion
time. `t` and `:any` are accepted as the universal specializer.

| Form | Applicability | Notes |
| --- | --- | --- |
| `:any`, `t` | always | universal specializer |
| `:default`, `_` | always | aliases for `:any` |
| `Class` | `instance?` | class is resolved at macroexpansion |
| literal (`:k`, `"s"`, `42`, `nil`) | `(= v arg)` | exact value match |
| set literal (`#{...}`) | membership | same as `(in s)` |
| `(in s)` | membership | set/map uses `contains?`; seqs use `some` |
| map literal (`{:k v ...}`) | map match | all entries must match `get` |
| `(isa? v)` | `(isa? arg v)` | uses Clojure hierarchy |
| `(pred f)` | `(f arg)` | predicate errors are treated as false |
| `(satisfies P)` | `(satisfies? P arg)` | protocol check; errors treated as false |
| `(key= k v)` | map with `get` equal | map-only dispatch |
| `(op v)` | alias for `(key= :op v)` | convenience macro |
| `(has-keys k1 k2 ...)` | map has all keys | uses `contains?` |
| `(keys= k1 k2 ...)` | map keys equal set | exact key set match |
| `(map-of kpred vpred)` | map with all entries matching | both predicates are applied to each entry |

Symbol predicates in `(pred ...)`, `(satisfies ...)`, and `(map-of ...)` are
resolved to vars at macroexpansion time via `(deref (var sym))`. If a function
value is supplied, it is used directly.

Predicate specializers (`pred`, `satisfies`, `map-of`) use a safe call wrapper.
Any thrown exception is treated as a non-match unless `:pred-exceptions` is set
to `:error`.

## Method selection and specificity

A method is applicable if all its specializers match the corresponding
arguments. Applicable methods are sorted by specificity, then by definition
order (earlier definitions win ties).

Specificity order (most specific to least):

1. literal
2. `in`
3. map literal
4. `key=`
5. `has-keys`
6. `keys=`
7. `map-of`
8. `isa?`
9. `pred` / `satisfies`
10. class specializer (closest superclass/interface wins)
11. `any`

For class specializers, the distance is computed by breadth-first search over
the superclass/interface graph.

## Method combinations

Supported combinations:

| `:combination` | Behavior |
| --- | --- |
| `:standard` | CLOS-style `:around`, `:before`, `:after`, `:primary` |
| `:list` | collect primary return values into a vector |
| `:and` | logical AND of primary return values |
| `:or` | logical OR of primary return values |
| `:+` | sum of primary return values |
| `:max` | max of primary return values |
| `:min` | min of primary return values |
| other | first applicable primary only |

`:standard` combination semantics:

- `:around` methods wrap the entire call chain and may call
  `call-next-method` to proceed.
- `:before` methods run in most-specific order before primaries.
- `:after` methods run in least-specific order after primaries.
- `:primary` methods form a next-method chain; `call-next-method` moves to the
  next most-specific primary.

For non-`:standard` combinations, only `:primary` methods are considered.

## Next method protocol

`call-next-method` and `next-method?` are only meaningful during a generic
function call. If no next method exists, `call-next-method` throws
`(ex-info "No next method" {:phase :dispatch})`.

## Arity rules

All methods of a generic must have the same arity. A mismatch throws
`(ex-info "Arity mismatch" {:expected n :actual m})`.

## Low-level API

The following functions are public for low-level integration:

- `generic?` — true if the argument is a generic function or var.
- `generic` — returns the generic atom from a generic var.
- `add-method!` — adds a method using explicit specializer maps.

Specializer maps are plain maps with a `:kind` key and additional fields as used
by `defmethod` expansion.

## Errors

- `Not a generic function` — `generic` or `add-method!` called on a non-generic.
- `Arity mismatch` — inconsistent arity across methods or a call with wrong
  argument count.
- `No applicable primary methods` — `:standard` combination with no primary.
- `No next method` — `call-next-method` with no remaining method.
