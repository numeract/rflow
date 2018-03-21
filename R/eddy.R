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


make_eddy_name <- function(eddy_name = NULL,
                           cache_path = NULL) {
    
    if (length(eddy_name) == 0L) {
        if (length(cache_path) == 0L) {
            'eddy_memory'
        } else {
            cache_path
        }
    } else {
        eddy_name
    }
}


# TODO: review and add / complete doc
# TODO: tests

#' Create or retrieves an eddy
#' 
#' @param cache_path A valid path of a directory to store the cache.
#'   Use \code{NULL} (default) for no disk cache.
#' @param envir An environment where to find the default eddy.
#' 
#' @return An R6Eddy object to be used when creating an rflow.
#' 
#' @export
new_eddy <- function(cache_path = NULL,
                     eddy_name = NULL,
                     envir = get_default_env()) {
    # use the name `new_eddy`, not `add_eddy` to convey fresh/empty idea
    # new guarantees a clean eddy (for tests)
    
    eddy_name <- make_eddy_name(eddy_name, cache_path)
    if (base::exists(eddy_name, where = envir, inherits = FALSE)) {
        stop("Cannot create a new eddy, name already present: ", eddy_name)
    }
    if (!is.null(cache_path) && base::dir.exists(cache_path)) {
        stop("Cannot create a new eddy, path already present: ", cache_path)
    }
    
    eddy <- R6Eddy$new(
        # this will be updated when we have more options for eddies
        cache_path = cache_path
    )
    assign(eddy_name, eddy, envir = envir)
    
    eddy
}


#' @export
#' @rdname new_eddy
get_eddy <- function(cache_path = NULL,
                     eddy_name = NULL,
                     envir = get_default_env()) {
    
    eddy_name <- make_eddy_name(eddy_name, cache_path)
    if (base::exists(eddy_name, where = envir, inherits = FALSE)) {
        # eddy already present in envir, just retrieve it
        eddy <- envir[[eddy_name]]
    } else {
        # new eddy object, it may reuse cache_path if already on disk
        eddy <- R6Eddy$new(
            # this will be updated when we have more options for eddies
            cache_path = cache_path
        )
        assign(eddy_name, eddy, envir = envir)
    }
    
    eddy
}


# TODO: doc on the same help page as new_eddy and get_eddy

#' Get the default, in memory, eddy for a given (or default) environment.
#'
#' @param envir An environment where to find the default eddy.
#' 
#' @return An R6Eddy object to be used when creating an rflow.
#' 
#' @seealso \code{\link{get_default_env}}
#' 
#' @export
get_default_eddy <- function(envir = get_default_env()) {
    
    get_eddy(envir = envir)
}


# TODO: doc on different page


#' Delete eddy and ALL its data from ALL cache layers
#'
#' @export
delete_eddy <- function(cache_path = NULL,
                        eddy_name = NULL,
                        envir = get_default_env()) {
    
    eddy_name <- make_eddy_name(eddy_name, cache_path)
    if (!base::exists(eddy_name, where = envir, inherits = FALSE)) {
        stop("Cannot find eddy with name: ", eddy_name)
    } else {
        eddy <- envir[[eddy_name]]
        # TODO: finish `reset` in R6Eddy
        eddy$reset()
        rm(list = eddy_name, envir = envir, inherits = FALSE)
    }
}
