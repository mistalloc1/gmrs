# GMRS

GMRS is a recommender system design to fit into existing websites and apps with
minimal setup. Even if your site isn't designed with making recommendations in
mind, GMRS will fit into your existing architecture and try to come up with
a good way to provide suggestions to your users.

## Dev features

Run REPL (connectable to by Nvim's Conjure):

```
clj -M:repl/conjure
```

Run tests, after setting up a kaocha
[binstub](https://cljdoc.org/d/lambdaisland/kaocha/CURRENT/doc/1-introduction):

```
bin/kaocha --focus :unit
# to watch in the background:
bin/kaocha --focus :unit --watch
```

## License

(to be added)

Copyright © 2025 Szymon Rutkowski
