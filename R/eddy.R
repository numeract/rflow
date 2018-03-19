# wrappers around R6Eddy following standard R functionality


# @include r6eddy.R


# create a separate environment to keep eddies
.EDDY_ENV <- new.env(parent = emptyenv())


#' Get the default environment that keeps the eddies.
#'
#' @return An environment.
#' 
#' @seealso \code{\link{get_default_eddy}}
#' 
#' @export
get_default_env <- function() {
    .EDDY_ENV
}


#' Get the defulat eddy for the default environment.
#'
#' @param cache_path A valid path of a directory to store the cache.
#'   Use \code{NULL} (default) for no disk cache.
#' @param envir An environment where to find the default eddy.
#' 
#' @return An R6Eddy object to be used when creating an rflow.
#' 
#' @seealso \code{\link{get_default_env}}
#' 
#' @export
get_default_eddy <- function(cache_path = NULL,
                             envir = get_default_env()) {
    
    if (!base::exists('.EDDY', where = envir, inherits = FALSE)) {
        assign('.EDDY', R6Eddy$new(cache_path = cache_path), envir = envir)
    }
    
    envir$.EDDY
}
