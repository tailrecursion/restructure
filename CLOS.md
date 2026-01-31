# CLOS

This document describes the generic function system in `tailrecursion.restructure.clos`.
It is a small, CLOS-style dispatcher with method combinations and map-aware
specializers. It is not a full Common Lisp Object System.

## Example

```
(require '[tailrecursion.restructure.clos :as clos])

(defrecord Db [^javax.sql.DataSource ds])

(clos/defgeneric with-db {:combination :standard})
(clos/defgeneric exec {:combination :standard})
(clos/defgeneric close! {:combination :standard})

(clos/defmethod with-db :around [(db Db) f]
  ;; common pattern: run, then clean up
  (try
    (f db)
    (finally (close! db))))

(clos/defmethod exec [(db Db) (q (pred string?))] [:query q])
(clos/defmethod exec [(db Db) (q (pred vector?))] [:prepared q])
(clos/defmethod exec [db _]
  (throw (ex-info \"Bad query\" {:db db})))

(clos/defmethod close! [(db Db)] :db-closed)

(defn run [db q]
  (with-db db #(exec % q)))

(def db (->Db nil))

(run db \"select 1\")
;; => [:query \"select 1\"]

(run db [\"select ?\" 42])
;; => [:prepared [\"select ?\" 42]]

(close! db)
;; => :db-closed
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
[x (eql :k)]            ; eql specializer
[m (key= :op :bind)]    ; map key/value specializer
```

## Specializers

Specializers are specified in `defmethod` and compile to a specializer map.
If a specializer is provided as a symbol, it resolves to a class at expansion
time. `t` and `:any` are accepted as the universal specializer.

| Form | Applicability | Notes |
| --- | --- | --- |
| `:any`, `t` | always | universal specializer |
| `Class` | `instance?` | class is resolved at macroexpansion |
| `(eql v)` | `(= v arg)` | exact value match |
| `(in s)` | `(contains? s arg)` | membership in a set or map |
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
Any thrown exception is treated as a non-match.

## Method selection and specificity

A method is applicable if all its specializers match the corresponding
arguments. Applicable methods are sorted by specificity, then by definition
order (earlier definitions win ties).

Specificity order (most specific to least):

1. `eql`
2. `in`
3. `key=`
4. `has-keys`
5. `keys=`
6. `map-of`
7. `pred` / `satisfies`
8. class specializer (closest superclass/interface wins)
9. `any`

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
