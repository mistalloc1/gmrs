# GMRS roadmap

## Further out

- evaluation, using the stored decision data
- rework CSV funcs to handle different delimiters and dialects

## Future versions

### For version 0.1.0

#### Milestones

**1.** Be able to handle (diagnose fields, preprocess data, run a mill) two example
datasets. These are [EDGAR Log Files](https://catalog.data.gov/dataset/edgar-log-file-data-set)
(EDGAR) and
[Anime Dataset 2023](https://www.kaggle.com/datasets/dbdmobile/myanimelist-dataset)
(AniD).

- EDGAR represents the case where we have mainly the raw website traffic.
- AniD represents the case with some observed, meaningful user profile to construct the rec cases.
- They're both freely distributable, so possible to get and verify by anybody.
- Handle shape of real world data.
- We don't have to use all of it, just load and provide some data-driven recommendations.

**2.** Provide instructions for including GMRS in web app projects and verify
they can be used. The initial focus is either Java Spring/Spring Boot ecosystem
or Clojure backend development (but the aim of the project is to be encapsulated
enough so it can be used from outside of Clojure).

- This includes having at least somewhat sound API design...
- ...and basic db connection through JDBC (probably), maybe also some NoSQL.
- Getting into Maven central could be delayed for until I'm more sure about the project.

#### Work items

- [ ] clean _get-governor, _get-options functions as private `defn-`s or separate ns %
- [x] loading CSVs through API
- [ ] detect and parse numbers and timestamps in CSVs
- [ ] handle pagination from the gives
- [ ] extend preprocessing for handling data from EDGAR
- [ ] extend preprocessing for handling data from AniD
- [ ] handle nils in preprocessing and/or existing mills
- [ ] add some type guards (and replace existing) using Clojure spec
- [ ] setup JDBC driver for gives and sends
- [ ] create instructions for web Clojure integration
- [ ] compare the API for Spring with LLM hallucinations
- [ ] compare the API for web Clojure with LLM hallucinations
- [ ] test doing the instructions in a clean environment
- [ ] see what happens with using instructions when user uses LLM help

% If separate ns, think of discouraging access from anywhere but gmrs.command.
We need to keep all the other stuff strictly separated. But the usage in
gmrs.governor (which might be overall) different should also be considered.
