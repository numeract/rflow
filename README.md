# rflow

# Flexible R Pipelines with Caching

**The package is currently under active development, please expect major 
changes while the API stabilizes.**

## Motivation

A common problem when processing data part of a pipeline is avoiding 
unnecessary calculations. For example, if a function is called over and
over with the same arguments, it should not recalculate the result each time
but it should provided the cached (pre-computed) result.

While caching of the function output resolves the first problem, a second
issue occurs when large data sets are being processed. In this case, hashing
of the input arguments might take too long. This issue can be solved
by hashing the data only once (as output) and then by noticing changes 
in the hash when in the downstream function. In other words, it is not 
the data the flows through the pipeline (as is the case with standard function),
but hashes of the data.

A third issue is output sub-setting. When working with a pipeline there is
often the case (e.g. ETL, Machine Learning) that we need to pass the whole
data frame but the function is going to use only a subset (e.g. a CV fold).
Since the main data frame has changes, caching of the result is no longer
efficient. The solution involves hashing of the subset of interest which
can be done by introducing additional intermediate functions in the pipeline.
However, there is a loss of efficiency due to excessive rehashing as the 
main data frame passes through many functions.

The package `rflow` address these inefficiencies and makes pipelines as easy
to use as in tidyverse.


## Installation

```
# install.packages("devtools")
devtools::install_github("numeract/rflow")
```


## Example

```
x0 <- 10
x1 <- 0.5
x2 <- 2

f <- function(a, b, c = 1) {a * b + c}
rf <- make_rflow(f)

(f1 <- f(x0, x1))   # 6
r1 <- rf(x0, x1)
collect(r1)         # 6

(f2 <- f(f1, x2))   # 13
r2 <- rf(r1, x2) 
collect(r2)         # 13
```
