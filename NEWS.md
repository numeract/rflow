# rflow 0.5.3

- added `compute()` function
- misc fixes (mostly `flow_dfr` and `flow_dfg`) and more tests
- fix `flow_dfr` and `flow_dfg` for factors 


# rflow 0.5.0

- the function supplied to `flow_dfr` receives only changed rows and no index
- new function: `flow_dfg` hadles grouped data frames
- refactor fn_key formation
- misc fixes


# rflow 0.4.1

- **extensive rewrite, breaks compatibility with previous versions**
- add: separate cache from eddy (new R6 class tree)
- updated eddy functions
- R6 class tree for flow functions
- `flow_` functions for file source and namespace sink
- `flow_dfr` for caching row-wise data frame operations
- alternative setup that does not require `make_flow_fn` functions
- lazy computation (does not re-compute a pipeline if not needed)
- extensive testing


# rflow 0.3.5

- add: is_cached to test whether a state already exists in cache
- add: delete_cache to delete the given state from rflow and eddy


# rflow 0.3.4

- fix: fail to init rflow when no data on disk
- fix: fail when retrieving an rflow element inside cached function
- add: `element()` function for R6Flow objects


# rflow 0.3.3

- `[` method for R6Flow to extract its elements


# rflow 0.3.2

- fix: make_ns_sink changed to follow make_sink pattern


# rflow 0.3.1

- fix: eddies always create cache_env and dir on disk
- fix: eddies require registered rflow when adding data


# rflow 0.3.0

- added file source
- add multi-purpose sink


# rflow 0.2.1

- collect is now an S3 method, using the S3 generic from dplyr


# rflow 0.2.0

- new functions: new_eddy, get_eddy, delete_eddy
- added tests
- added R6 obj printing
- update documentation
- update memory and disk caching issues


# rflow 0.1.0

First release
