# Tests

## Unit

The unit tests. They actually also include some light integration tests (named
`test-integr-` instead of just `test-`).

## E2E

End-to-end test require downloading datasets to the `dev/` folder in the
project's root dir.

### `/anid`

Download from https://www.kaggle.com/datasets/dbdmobile/myanimelist-dataset
the following files:

- anime-filtered.csv
- user-filtered.csv
- users-details-2023.csv

### `/edgar`

Download from https://www.sec.gov/data-research/sec-markets-data/edgar-log-file-data-sets
the archive for 2015-06-01 and unpack it (we want the file `log20150601.csv`).
