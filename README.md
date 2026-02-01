# restructure

Rewrite nested Clojure data with a declared shape.

`over` compiles a selector and rewrite body into code in the style of `update`/`mapv`/`update-vals`, but keeps the shape in one place. It visits only what you select.

## API

```clojure
(over selector body)         ; => rewritten value
(over-> value selector body) ; => rewritten value (thread-first helper)
(over->> value selector body); => rewritten value (thread-last helper)
```

## Examples

### 1) Update nested numbers without a full walk
```clojure
(def data
  {:left [{:p 1 :q 2}
          {:r 3}
          {:s 4}]
   :right [{:t 5}]})

(over [{_ [{_ n}]} data]
  {n (cond-> n (even? n) inc)})

;; => {:left [{:p 1 :q 3} {:r 3} {:s 5}]
;;     :right [{:t 5}]}
```
Sequential example:
```clojure
(over [[n] [1 2 3]]
  {n? (even? n)})
;; => [2]
```
Threading helpers:
```clojure
(-> data
    (over-> [{_ [{_ n}]}]
            {n (cond-> n (even? n) inc)}))
```
Map + vector traversal with one binding.

Plain Clojure:
```clojure
(update-vals data
             #(mapv (fn [m]
                      (update-vals m (fn [n] (cond-> n (even? n) inc))))
                    %))
```

### 2) Filter map entries + normalize a field
```clojure
(require '[clojure.string :as str])

(def users
  {:alice {:active true  :email "ALICE@EXAMPLE.COM"}
   :bob   {:active false :email "bob@example.com"}
   :cara  {:active true  :email "CARA@EXAMPLE.COM"}})

(over [{_ {:keys [active email] :as u}} users]
  {u?    active
   email (str/lower-case email)})

;; => {:alice {:active true :email "alice@example.com"}
;;     :cara  {:active true :email "cara@example.com"}}
```
Map-entry traversal with a guard (`u?`) to drop entries.

Plain Clojure:
```clojure
(->> users
     (reduce-kv (fn [m id u]
                  (if (:active u)
                    (assoc m id (update u :email str/lower-case))
                    m))
                {}))
```

### 3) Traverse a nested vector and drop bad items
```clojure
(def order
  {:id 42
   :lines [{:sku "A" :qty 2}
           {:sku ""  :qty 1}
           {:sku "B" :qty 0}]})

(over [{:keys [lines]} order
       [{:keys [sku qty] :as line}] lines]
  {line? (seq sku)
   qty   (or qty 0)})

;; => {:id 42
;;     :lines [{:sku "A" :qty 2}
;;             {:sku "B" :qty 0}]}
```
Sequential traversal; each element can be rewritten or dropped.

Plain Clojure:
```clojure
(update order :lines
        (fn [lines]
          (->> lines
               (filter (comp seq :sku))
               (mapv #(update % :qty (fnil identity 0))))))
```

## Selector semantics (query, not destructuring)

A selector is a vector of alternating `pattern` and `source`:

```clojure
(over [pattern1 source1
       pattern2 source2
       ...]
  body-map)
```

- The first `source` is the input expression.
- Later sources must be previously bound symbols.
- The selector looks like destructuring, but it is a **query**: it binds and traverses; it does not introduce locals the way Clojure destructuring does.

### Supported patterns

- **Map-entry traversal:** `{kpat vpat}`
  - Requires a map value; iterates entries.
  - `kpat` binds the key (no traversal). `vpat` may traverse.
  - Key traversal is intentionally not supported to keep map semantics and
    identity guarantees predictable.
- **Sequential traversal:** `[pat]`
  - Requires a sequential or set value; iterates elements.
- **Sequential destructure traversal:** `(seq [k v & more])`
  - Iterates elements and destructures each element as a vector, supporting `&`
    and `:as`.
- **Plain symbol:** `sym`
  - Binds the current value.
- **Map destructuring form:** `{:keys [...], :strs [...], :as sym, :or {...}}`
  - No traversal; binds keys and/or `:as`.
  - Nested destructuring inside `:keys`/`:strs` is rejected.
  - Explicit key bindings are allowed, Clojure-style: `{user-id :id, org-id "org_id"}`
- **Records** are treated as maps and may come back as plain maps after rewrite.

Unsupported or ambiguous patterns are rejected with `ex-info` including `:phase`, `:path`, and `:pattern`.

## Rewrite/body semantics

The body is a map from **target symbols** to expressions.

- A key `x` rewrites the binding `x`.
- A key `x?` is a **guard**. When false, the corresponding structural position is elided.

Elision rules:
- Map-entry traversal: false guard removes the entry.
- Sequential traversal: false guard removes the element.
- Map destructure key: false guard removes that key.
- `:as` binding: false guard removes the whole element at that level.

Traversal is post-order: children are processed before parents. Guards see post-child values.

## Tradeoffs

Plain Clojure is already a win when:
- A couple of `update` calls are clearer than any DSL.
- `update-vals` + `mapv` is easier to scan for simple shapes.

`clojure.walk/postwalk` is better when:
- You need full-tree traversal.
- You don’t know the shape ahead of time.

`over` fits when:
- You have a known shape and want to touch a few nested spots.
- You want the traversal shape and the rewrite adjacent.
- You want compiled code that avoids full walks and keeps structural sharing.

## Non-goals

- Not a general tree rewriting engine.
- Not a replacement for all data transformation.
- Not arbitrary destructuring: selectors are queries, not local binding forms.
- No `:let` or computed traversal paths.

## Notes

- Identity: if no effective change occurs (value and metadata equality at all rewritten
  points), `over` returns a result that is `identical?` to the input.
- See [CLOS.md](CLOS.md) for the CLOS-style generic function system in this repo.

## Check (format + test + lint)

```bash
clj -M:check
```

## License

MIT
