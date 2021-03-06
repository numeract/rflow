% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flow-functions.R
\name{collect}
\alias{collect}
\alias{collect.R6Flow}
\alias{collect.Element}
\title{Get the data from an \code{R6Flow} or an \code{Element} object}
\usage{
\method{collect}{R6Flow}(x, ...)

\method{collect}{Element}(x, ...)
}
\arguments{
\item{x}{A flow object, e.g. as returned by \code{\link{flow_fn}}.}

\item{...}{Name of the element of the output data to be selected. 
If present, it must be named \code{name}, otherwise the first 
item of the \code{...} list will be used.
The default is \code{name = NULL}, which returns all the data.
Ignored if \code{x} is an \code{Element} object.}
}
\value{
Data associated with the output of the function.
}
\description{
Get the data from an \code{R6Flow} or an \code{Element} object
}
\examples{
fn <- function(x, y) { x + y + 4 }
flowed_fn <- flow_fn(2, 3, fn = fn) 
flow_result <- flowed_fn \%>\% collect()

fn <- function(x, y) { list(x = x, y = y, z = 5) }
flowed_fn <- flow_fn(2, 3, fn = fn)
flow_result <- flowed_fn \%>\%
    collect()
flow_element <- element(flowed_fn, "x")
collected_element_value <- collect(flow_element)

}
