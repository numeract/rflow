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
