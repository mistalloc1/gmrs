# Introduction to GMRS

Governor-Mills Recommender Systems.

## Conceptual overview (more in depth)

In a recommender system, we have *options* (which may also be called candidates,
items...) that can be chosen for *cases* (which can usually be understood as
users of a website or an app, and in some cases called 'queries').

You invoke GMRS to get a ranking of possible options for each case. We call it
a *decision*. Information about the decision can be stored, referenced in later
interaction records and used in recommendations.

The interactions can later refer to earlier decisions, so we know they are
responsible for the interactions. This can be useful for calculating
Click-Through Rate (CTR).

Decisions can be referred to in the code as "decs".

Other *interactions* involving known cases and options can also be retrieved
to help in the decisions. The interactions may also include ratings.
Interactions can be referred to in the code as "inters".

### Handling cases, options, interactions

...

NOTE You need to send at least some cases and options in order for the
"autogovern" mechanism to diagnose the types of data (features) available.

#### IO settings

IO settings are always associated as :io-settings to the metadata of any items
(cases, options etc.) gotten from IO give functions.

### Recommendations

On the Clojure level, the recommendations are returned as maps of case IDs
to maps of option IDs (under key :option-id from the IO settings) and `:score`s.
The max :score is 1.0 (barring minor float corruption), but it can also be
negative down to -1.0 and, from some mills, more.

### Governors and mills

...

A mill function gets cases and options and returns options ID along with
scores for each case. It shouldn't concern itself with sorting. It is columnar
in a way, because the format is case IDs as columns and map of option ID and
score as objects in the rows.

(TODO: example here)

(TODO: decide and document when the mills will not duplicate existing interactions)

Mill is used multiple times using a pull strategy set by the governor. The pull
strategy controls getting more and more potential options from the sources
(so-called gives), until it decides it's better to use the already obtained
options rather than take more.

#### Mill functions

By convention mill functions should have at least one call form ("canonical
mill arglist") with these args:

```
cases cases-getter-partial options-getter-partial inters-getter-partial
decs-getter-partial pull-strategy
```

- Cases are the cases for which to recommend, with applied preprocessing,
  in columnar format.
- The getter partials are getter functions from io.getters (partial)ed with
  preprocessing function but with no dataprefs set.
- The pull-strategy is a pull strategy from governor.

Apart from this, the mill function can have a longer form receiving custom
configuration arguments, and its own "private" form which can typicaly
self-recur as long as the pull-strategy decides to continue the search.

## See also

- dev-intro.md (more about the design assumptions etc.)
- data-storage-model.md (how we store and retrieve cases, options ... all data)
- column-attributes.md (detected by governor column analysis)
- mills.md
