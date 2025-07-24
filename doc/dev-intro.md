
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

  ## Dependency assessments

  ### 0.0.*

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

