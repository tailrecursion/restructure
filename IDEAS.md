# Ideas

## Touched tracking without metadata

Problem:
- We want to know if `over` actually changed anything, but setting metadata on
  returned values can break identity semantics and structural sharing.

Rationale:
- Precise “touched” information is available at compile time, but attaching it
  to the returned value (e.g. via metadata) has observable identity cost.
- A separate query surface keeps results pure while still enabling downstream
  tooling or caching decisions.

Idea:
- Maintain a weak reference map keyed by the original input value to record a
  `touched?` flag.
- Expose a predicate like `(touched? before after)` or `(touched? before)`
  depending on the storage model.

Tradeoffs:
- Weak caches are best‑effort: entries can disappear at any time due to GC.
- Identity is required: the map must be keyed on reference, not `=`.
- Concurrency needs careful handling (atom + WeakHashMap wrapper or a custom
  java.util.Map with synchronization).
- “Touched?” must be defined: structural change vs. rewritten‑then‑equal.

Tentative implementation approaches:
- Return both value and touched flag in an opt‑in API:
  - `(over+ selector body) => [value touched?]`
  - `(compile-over selector body {:touched? true})`
- Weak map side‑channel:
  - `*touched-cache*` dynamic var holding a `WeakHashMap` keyed by input.
  - Compiler emits a `record-touched!` call when a rewrite/guard/elision occurs.
  - `touched?` reads from the cache; missing entry implies false/unknown.

Notes / open questions:
- Use `java.util.WeakHashMap` (or similar) to avoid leaks.
- Concurrency semantics for the cache.
- Define precise semantics: only true when a rewrite/guard/elision occurs.
- Decide whether to keep the cache per-call, per-thread, or global.
