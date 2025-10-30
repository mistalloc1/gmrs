# Column attributes

Here are possible attributes assigned to columns by governor diagnostics. They
can be then used to mark columns as features used by mills with particular kinds
of processing.

*Feature policy*: the attributes that are difficult to figure out
programatically with acceptable correctness should be configurable by the user
instead.

- `:integer` - integer
- `:float` - float
- `:tags` - default string interpretation, separated by pipes (|)
- `:str` - string
- Time attributes:
  - `:date-zoned-with-time`
  - `:date-local`
  - `:time-local`
- Conversion marking:
  - `:<format>-needs-conv` - the column needs conversion from string to the
     indicated format

### To implement

- `:past` - time, containing timestamps from the past
- `:future` - time, containing timestamps from the future
