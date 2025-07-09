
# Design focus

We want to be useful rather than necessarily correct: if it possible to provide
some recommendations, we want to provide them. The user should be sufficiently
informed by the docs and low scores that the quality might be low, if that would
be the case.

We want to focus on cases where the developer has either little data or a
substantial amount of data but not collected with making recommendations in mind.
Other situations (i.e. often more optimistic cases data-wise) can be covered by
extension.

## Data storage model

Some features might be cut down on later if necessary, but I think this setup
may be both useful and interesting to design and implement.

## Some "project resolutions"

These are orthogonal design goals I set for myself to explore beyond corporate
dev practices many of us are moored into every day.

- Avoid classes, methods etc. as much as possible, more pure maps and functions (the Clojure way)
- Don't get entangled too much into forges such as GitHub and related ceremonies
  - Project planning through todos and the ROADMAP.md file
