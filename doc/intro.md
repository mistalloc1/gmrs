# Introduction to GMRS

Governor-Mills Recommender Systems.

## Conceptual overview (more in depth)

In a recommender system, we have *options* (which may also be called candidates,
items...) that can be chosen for *cases* (which can usually be understood as
users of a website or an app, and in some cases called 'queries').

You invoke GMRS to get a ranking of possible options for each case. We call it
a *decision* - which also involves the decision of the user to accept or
decline recommendations. Information about the decision (and whether there was
an user agreement) can be stored and used in later recommendations. Decisions
can be referred to in the code as "decs".

Other *interactions* involving known cases and options can also be retrieved 
to help in the decisions. The interactions can be referred to in the code as
"inters".

### Governors and mills

...

## See also

- dev-intro.md (more about the design assumptions etc.)
- data-storage-model.md (how we store and retrieve cases, options ... all data)
- column-attributes.md (detected by governor column analysis)
- mills.md
