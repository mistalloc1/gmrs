# Column attributes

Here are possible attributes assigned to columns by governor diagnostics. They
can be then used to mark columns as features used by mills with particular kinds
of processing.

*Feature policy*: the attributes that are difficult to figure out
programatically with acceptable correctness should be configurable by the user
instead.

- `:num` - numeric
- `:tags` - default string interpretation, separated by pipes (|)
  TODO: assess the number of potential tags, to avoid an explosion

### To implement

- `:time`
- `:past` - time, containing timestamps from the past
- `:future` - time, containing timestamps from the future
- `:needs-conv` - the column needs conversion from string to the indicated format
