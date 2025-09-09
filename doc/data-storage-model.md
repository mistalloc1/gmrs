# Data storage model

GMRS can operate on multiple data sources (*gives*) and receivers (*sends*).
Both are set up in `*GlobalIOSetup*` variable in `gmrs.command`.

For one-time manual addition (from CSVs, etc.) users should use API calls.

We make our best to combine gives (which can be different DBs etc.) into one
stream for transparent use by GMRS functions.

GMRS writes the relevant items to all the sends. Note that this could be also
as an API plumbing: i.e. we don't have to do anything interesting with (say)
options sent to us by some HTTP API requests, we just want to save them for
later use.

Not all gives have to also be sends and vice versa. This allows for
seeding/providing the system with some read-only sources, or siphoning the
output data for uses outside of GMRS.

## Gives

Gives are functions that get IO settings map as the arg and return a lazy
sequence of sequences of `(<= page-size)` amount of records. The lazy sequences
remember the page size from the time of their creation, so you'll want to
call the give again if you want the new page size.

### Getters

Gives are generally used throught the `io/getters.clj` namespace. It wraps
all gives and provides the data from them in columnar format. The getter can
be used as an infinite lazy sequence. Each of its elements take one page
(of the configured page size) from each give that is set up.

## Sends

Sends are functions that take as the args the IO settings map and the sequence
of records to add.

## Record types

* options (candidates, items)
* cases (users, targets)
* decisions (decs) - recommendations of options for cases, with the user's "agree(ment)" {0.0, 1.0}
* interactions (inters) - between real world case and options
* governors (governs, govs, guvnas) - the GMRS control objects

## Expectations from the users

### ID and agree columns

The users should be expected to provide ID columns for cases, options and
governors. The IDs for cases and options are also set for decision and interaction
storage. This is on the *GlobalIOSetup* level; if the gives/sends use different
IDs internally, this can be handled by the drivers. IDs can be any data type.

The decision store also needs an agree column which can be either boolean or
numeric in the range {0.0, 1.0}.

The ID columns should be unique, but we *can* try to do some detection
and/or mitigation if this isn't the case.

### Keeping track of governors

Since governors are retrieved by ID, you should keep the IDs of the governors that
you use.

### Storing decisions

The decisions are not stored by GMRS by default - you should store the decisions
that you use, along with their agree value. If there's demand we could write the
decs in the future - but even then they'd have to be updated by the user because
we can't determine the agree value on the GMRS side.

## Baseless mode

Currently by default we provide some runtime Clojure gives/sends in
`toy-temp-baseless-io-setup` (`gmrs.io.baseless`). This needs to be rethought
later as some actual drivers come in. This must warn the user that *no items
will be preserved* after exiting the JVM. But the baseless mode should be
left existing in some way to ease developing GMRS and apps using it.

## "Send" saving, updating and IDs

If possible, the sends should update (replace) the relevant records when the
sent records' IDs match with existing ones.

## "Give" data driver features

This is heavily **under construction** and consideration.

### Retrieving and IDs

Governors are values saved and retrieved by key, the rest is (potentially
filtered) "streams" that we don't expect to get by ID. The client is expected to
keep track of its governor(s).

### Combining and ordering

A simple approach to combining sources (not in roadmap for 0.1.0) is interlacing
one record from each source in sequence; maybe in a bigger stride than 1 for
performance.

Another thing is expecting ordering from sources. Most likely we should expect
none, especially if we move to anything beyond SQL.

We can expect to get all the items eventually. By the default the getters
produce sequences cycling indefinitely, so every time one needs to decide how
many pages (with possible repeats) the part of the program needs.

### Filtering

Filtering should ideally:
1. be easy (in Hickeyan sense, familiar) for non-Lisp-brained devs,
2. based on some existing and somewhat tested syntax,
3. closed as to be simple to be secured,
4. manageable to be sent in an HTTP request parameter,
5. provide basic features in an extensible way: nested `or`, `and`, SQL `like`.

I'm thinking some JSON thing as a last resort. Maybe AWS has some API like this
so we could aim to be somewhat compatible.
