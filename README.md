# rflow - Flexible R Pipelines with Caching

[![Travis build status](https://travis-ci.org/numeract/rflow.svg?branch=master)](https://travis-ci.org/numeract/rflow)
[![Coverage status](https://codecov.io/gh/numeract/rflow/branch/master/graph/badge.svg)](https://codecov.io/github/numeract/rflow?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/rflow)](https://cran.r-project.org/package=rflow)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
 
**The package is currently under active development, please expect major 
changes while the API stabilizes.**


## Motivation

A common problem when processing data as part of a pipeline is avoiding 
unnecessary calculations. For example, if a function is called over and
over with the same arguments, it should not recalculate the result each time
but it should provide the cached (pre-computed) result.

While caching of the function output resolves the first problem, a second
issue occurs when large data sets are being processed. In this case, hashing
of the input arguments each time might take too long. This issue can be solved
by hashing the data only once (as output) and then by noticing changes 
in the hash received by the downstream function. In other words, it is not 
the data that flows through the pipeline (as is the case with standard function),
but hashes of the data.

A third issue is output sub-setting. When working with a pipeline there is
often the case (e.g. ETL, Machine Learning) that we need to pass the whole
data frame but the function is going to use only a subset (e.g. a CV fold).
Since the main data frame has changes, caching of the result is no longer
efficient. The solution involves hashing of the subset of interest which
can be done by introducing additional intermediate functions in the pipeline.
However, there is a loss of efficiency due to excessive rehashing as the 
main data frame passes through many functions.

The package `rflow` addresses these inefficiencies and makes pipelines as easy
to use as in tidyverse.


## Installation

```
# install.packages("devtools")
devtools::install_github("numeract/rflow")
```


## Use


### Simple Example

```
x1 <- 10
x2 <- 0.5
x3 <- 2

f1 <- function(a, b, c = 1) {a * b + c}
f2 <- function(d, e) {d / e}

# passing the results downstream using functions
(o1 <- f1(x1, x2))  # 6
(o2 <- f2(o1, x3))  # 3


# variant 1: declaring flows for each function using default options
ff1 <- make_flow_fn(f1)
ff2 <- make_flow_fn(f2)

# passing to the downstream flow and collecting the results
r1 <- ff1(x1, x2)   # does not trigger re-calc
r2 <- ff2(r1, x3)   # does not trigger re-calc; first arg. is a flow arg.
collect(r1)         # 6
collect(r2)         # 3


# variant 2: arguments and functions withing one call
library(dplyr)                          # makes life easier 
flow_fn(x1, x2, fn = f1) %>%            # reuses cache created by ff1
  flow_fn(x3, fn = f2) %>%              # reuses cache created by ff2
  collect()                             # 3, no actual re-calc takes place
```


### Pipelines

1. Create your function, e.g. `f <- function(...) {...}`
- `rflow` works best with pure functions, i.e. functions
that depend only on their inputs (and not on variables outside the function 
frame) and do not produce any side effects (e.g. printing,  modifying variables 
in the global environment).

2. "flow" the function: `ff <- make_flow_fn(f))`

3. When pipelining `ff` into another `rflow` function, simply supply `ff()`
as an argument, for example: `ff(x) %>% ff2(y) %>% ff3(z)`

4. At the end of the `rflow` pipeline you must use `collect()` to collect
the actual data (and not just the cached structure). Alternatively,
use `flow_ns_sink()` to dump the data into an environment or a 
`Shiny::reactiveValues` name space.


### Shiny

Shiny from RStudio uses reactive values to know what changes took place and 
what to recompute. It is thus possible to use a series of reactive elements 
in Shiny to prevent expensive re-computations from taking place. Example:

```
rv1 <- reactive({ 
    ... input$x ... 
})

rv2 <- reactive({ 
    ... rv1() .... input$y ... 
})

rv3 <- reactive({ 
    ... rv2() .... input$z ... 
})
```

The downside is that we need one reactive element for each function in the 
pipeline - this makes data processing dependent on UI / Shiny. Using `rflow`, 
we can separate the UI from the data processing, maintaining the caching
not only for the current state but for all previously computed states.

```
rv <- reactive({ 
    rf1(input$x, ...) %>%
    rf2(input$y, ...) %>%
    rf3(input$z, ...) %>%
    collect()
})
```

While a similar workflow can be achieved with package `memoise`, it suffers from
several disadvantages (below).


### Output Subset 

(to be updated)


## Other frameworks


### Memoise

Package [memoise](https://github.com/r-lib/memoise) 
by Hadley Wickham, Jim Hester and others was the main source of inspiration.
Memoise is elegant, fast, simple to use, but it suffers from certain limitations 
that we hope to overcome in this package:

- excessive [rehashing of inputs](https://github.com/r-lib/memoise/issues/31)
- only one cache layer (although its cache framework is extensible)
- no input/output sub-setting, it uses the complete set of arguments provided
- no reactivity (yet to be implemented in `rflow`)


### Drake

Package [drake](https://github.com/ropensci/drake) by Will Landau and others 
provides a complete framework for large data sets, including using
files as inputs and outputs. The downside is that it requires additional 
overhead to get started and its focus is on the pipeline as a whole. If your
work requires many hours of computations (which increases the value of each 
result), the overhead due to the setup has a relatively lower cost - in this
scenario `drake` is an excellent choice.

Package `rflow` is somewhere between `memoise` and `drake`:

- one can start using `rflow` right away, with minimal overhead
- allows focusing on the data processing (e.g., EDA) and not on the framework


## TODO list

- reactivity 
- multi-layer cache (with file locking)
- files sinks
- parallel processing
