
# Design focus

We want to be useful rather than necessarily correct: if it possible to provide
some recommendations, we want to provide them. The user should be sufficiently
informed by the docs and low scores that the quality might be low, if that would
be the case.

We want to focus on cases where the developer has either little data or a
substantial amount of data but not collected with making recommendations in mind.
Other situations (i.e. often more optimistic cases data-wise) can be covered by
extension.

If an organization has huge recommendation needs, it would probably build something
custom. Focus on smaller cases, one machine.

## Data storage model

Some features might be cut down on later if necessary, but I think this setup
may be both useful and interesting to design and implement.

## Some "project resolutions"

These are orthogonal design goals I set for myself to explore beyond corporate
dev practices many of us are moored into every day.

- Avoid classes, methods etc. as much as possible, more pure maps and functions
  (the Clojure way)
- Thoughtful dependency policy, try to keep track of them recursively and keep
  the full resulting jar as small as possible
- Don't get entangled too much into forges such as GitHub and related ceremonies
  - Project planning through todos and the ROADMAP.md file

## Setting up taps

There are Clojure tap> calls put in some places in the code. Their intended
purpose is for debugging. The general "area" of the tap information is indicated
by the :place keyword. You can use it like this (with a set containing the
:place's you want to print from):

```
(def dbg-tap (fn [t] (when (#{:diag-column} (:place t)) (println t))))
(add-tap dbg-tap)
```

And later:

```
(remove-tap dbg-tap)
```

## Specific considerations

### Outside of JVM

One of the goals of GMRS is to at least explore use cases outside of JVM (like
Python/Django) in the search for an active audience.

Experimental setup for this exists in the 0-0-2-graalvm-image branch, based on
[roman01la/graal-clojure-wasm](https://github.com/roman01la/graal-clojure-wasm).
Note:

- The GraalVM compiler stalls on anything dependent on org.bytedeco.javacpp
  which currently includes both Neanderthal and Fastmath (maybe to prior to
  3.* which is still in alpha).
- It also seems to eat even the unreferenced clj files and deps.edn deps, so
  they would need to be excluded somehow for this alias.
- On the experimental branch, I included alternative math ns implementations in
  Fastmath and pure Clojure. The latter compiles if enabled for all math imports.
- I included an experimental `wasm_test.py` file using the Python Wasmtime
  library. It fails because we cannot turn on the gc WASM proposal. The progress
  needs to be tracked on the [proposals page](https://docs.wasmtime.dev/stability-wasm-proposals.html)
  and [Python API docs](https://bytecodealliance.github.io/wasmtime-py/#wasmtime.Config).


Due to this, for now we try to limit the dependencies except for when necessary.

### Dependency assessment for 0.0.2

  Base uberjar size (0.0.1): 365M

  Basically we need dataframes (columnar structures) and enough math.

| Size delta   | Package     | Features                                        |
| ------------ | ----------- | ----------------------------------------------- |
| 223M (0.52.0)| neanderthal | (+) linear algebra, (+) manual mem alloc,       |
|              |             | (+) direct CPU/GPU compute, (-) OS-dependent deps|
| ------------ | ----------- | ----------------------------------------------- |
| 131M (2.4.0) | fastmath    | (+) handy math functions, some ML               |
| ------------ | ----------- | ----------------------------------------------- |
| install breaks | clojask   | (+) dataframes (-) constrained computation, no  |
| on log4j conflict |        | col access? (-) check if maintained             |
| ------------ | ----------- | ----------------------------------------------- |
| 263M (4.0.0) | spark sql   | (+) dataframes, rich wrangling (+) web UI,      |
| -> 628M      |             | (-) scala interop, lang changes (-) we have to  |
|              |             | run the driver/session, closes REPL on sleep    |
|------------- | ----------- | ----------------------------------------------- |
| 40M (4.0.0)  | spark mllib | (+) ML impls incl. collab filtering, distance   |
|on top of sql |             | calcs... (-) need to adapt to their ways        |
| ------------ | ----------- | ----------------------------------------------- |
| 45 MB on top of | geni     | (+) Spark API adapted for Clj (+) supports ML   |
| 613M 3.* spark |           | (-) little stale and unstable ver (-) Spark 3.* |
| ------------ | ----------- | ----------------------------------------------- |
| 40M (1.0.8)  | fijit       | (+) niceties for Scala interop (-) keep it up   |
|              |             | with Scala (-) little stale (2022) tho should work|
| ------------ | ----------- | ----------------------------------------------- |
| 21M (7.061) |tech.ml.dataset| (+) Clojuric dataframes, columnar, mlns rows   |
|              |             | (+) high perf                                   |
| ------------ | ----------- | ----------------------------------------------- |

See [benchmark](https://github.com/zero-one-group/geni-performance-benchmark/)
where tech.ml.dataset and Geni (Spark) show favorably.

