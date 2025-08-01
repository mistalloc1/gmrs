# Column attributes

Here are possible attributes assigned to columns by governor diagnostics. They
can be then used to mark columns as features used by mills with particular kinds
of processing.

*Feature policy*: the attributes that are difficult to figure out
programatically with acceptable correctness should be configurable by the user
instead.

- `:num` - numeric - TODO: tell when integer and float happens - probably decision needed in code
- `:integer` - integer
- `:float` - float
- `:tags` - default string interpretation, separated by pipes (|)
- `:str` - string
- `:needs-conv` - the column needs conversion from string to the indicated format
- Time attributes:
  - `:date-zoned-with-time`
  - `:date-local`
  - `:time-local`

### To implement

- `:past` - time, containing timestamps from the past
- `:future` - time, containing timestamps from the future
