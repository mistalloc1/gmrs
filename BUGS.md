# BUGS.md

Known bugs should be described here.

- Additional comments can be added under 2-level (##) and lower Markdown headings.
- Please add dates of <ADDED>, <WIP> (work in progress) and <SOLVED> statuses.
- To the <WIP> status, we can add some additional info like the version and branch.
- On <SOLVED>, we can add a "## Solution" heading at the end of the bug's section
  with the appropriate description for future learnings. This is optional.

#1 "Float columns interpreted as tags in the Anime DB test cases"

<ADDED> 2025-11-08
<SOLVED> 2025-12-22

When running the "anid" test, some features like the users' "Watching" column
are interpreted as :str/:tags and when the same data is supplied into
`(diag-column)` separately, it is correctly interpreted as :float/:float-needs-conv.

```
(diag-column
  ["1.0" "23.0" "16.0" "5.0" "1.0" "11.0" "20.0" "27.0" "0.0" "15.0" "0.0" "34.0"
   "30.0" "2.0" "13.0" "4.0" "6.0" "3.0" " 2.0" "4.0" "1.0" "221.0" "14.0" "2.0"
   "2.0" "20.0" "46.0" "7.0" "1.0" "7.0" "34.0" "10.0"])
```

This problem can crash running the test. For now, the workaround will be to either
exclude this column or provide a way to force some column types despite of the
diagnostic heuristics.

*Solution*: the "tags" code was just worked on and extended so we no longer have
this problem. As of marking as solved, there's still the inconsistency that if
the same values are supplied as floats (and not strings with floats), they could
not be classified as :tags even if they qualify due to distribution characteristics.
