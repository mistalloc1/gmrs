# GMRS

See the `doc/` directory for more documentation!

## Status

GMRS is currently an EXPERIMENTAL project. As of writing, the basic mills finish
being implemented and tested on CSVs. The next step is providing connections
for actual databases (in GMRS parlance, sets of "give" functions).

Of course, nothing can be treated as stable.

See `ROADMAP.md` for the more detailed plans and `CHANGELOG.md` for the progress.

## Contributing and communication

I do not want my open source projects to be chained to a particular forge
(a dirty explanation for what forge is would be 'GitHub-like website').
I will try to monitor forge features, like issues, wherever I mirror the repo
under *mistalloc* username, but you can also reach out freely to `szymon`
(at-sign) `szymonrutkowski.pl`.

In general, I think it is too early to take contributions and work from other
people but I'm not completely closed off.

## Vision

GMRS is a recommender system designed to fit into existing websites and apps with
minimal setup. Even if your site isn't designed with making recommendations in
mind, GMRS will fit into your existing architecture and try to come up with
a good way to provide suggestions to your users.

GMRS aims at pragmatic developers rather than data scientists as the audience.
The code is based on recognized algorithms (see `doc/mills.md`), but they can
be computationally simplified and modified to only use part of potentially large
data available. One can configure a strategy of either taking more data for
higher confidence or returning some results more quickly.

## The name

GMRS stands for Governor-Mill Recommender System. "Governors" are objects that
hold information about how to interpret the app data, which mills to use and
how. The "mills" are different functions (implementing some kind of algorithm)
that generate scored recommendations.

### Why use a bunch of invented terms throughout the codebase?

These terms have the advantage of being unambigous in the context of GMRS code.
Think how many hazy, context-dependent meanings "user" or "item" might have.

The library users should not have to worry about them that much, but down the
line I should create a handy glossary file. For now they should be defined
somewhere in the docs.

## Dev features

Run tests, after setting up a kaocha
[binstub](https://cljdoc.org/d/lambdaisland/kaocha/CURRENT/doc/1-introduction):

```
bin/kaocha --focus :unit
# to watch in the background:
bin/kaocha --focus :unit --watch
```

Run REPL (connectable to by Nvim's Conjure):

```
clj -M:repl/conjure
```

## License

Copyright © 2025-2026 Szymon Rutkowski

Licensed under the Apache License, Version 2.0 (the "License");
you may not use the contents of the distribution except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
