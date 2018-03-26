# wrappers around R6Eddy following standard R functionality


# create a separate environment to keep eddies
.EDDY_ENV <- new.env(parent = emptyenv())


.EDDY_DEFAULT_NAME = "eddy_memory"


#' Get the default environment that keeps the eddies.
#'
#' @return An environment.
#' 
#' @export
get_default_env <- function() {
    .EDDY_ENV
}


make_eddy_name <- function(eddy_name = NULL,
                           cache_path = NULL) {
    
    if (length(eddy_name) == 0L) {
        if (length(cache_path) == 0L) {
            .EDDY_DEFAULT_NAME
        } else {
            cache_path
        }
    } else {
        eddy_name
    }
}


# TODO: tests

#' Create or retrieve an eddy.
#' 
#' @param cache_path A valid path of a directory to store the cache.
#'   Use \code{NULL} (default) for no disk cache.
#' @param envir An environment where to find the default eddy.
#' 
#' @family eddy functions
#' 
#' @return An R6Eddy object to be used for storing rflows and their data.
#' 
#' @export
new_eddy <- function(cache_path = NULL,
                     eddy_name = NULL,
                     envir = get_default_env()) {
    # using the name `new_eddy`, not `add_eddy` to convey fresh/empty idea
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
#' 
#' @family eddy functions
#' 
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


#' Get the default, in memory, eddy for a given (or default) environment.
#'
#' @param envir An environment where to find the default eddy.
#' 
#' @return An R6Eddy object to be used for storing data and rflows.
#' 
#' @family eddy functions
#' 
#' @export
get_default_eddy <- function(envir = get_default_env()) {
    
    get_eddy(envir = envir)
}


#' Delete eddy and ALL its data from ALL cache layers.
#'
#' @family eddy functions
#' 
#' @export
delete_eddy <- function(cache_path = NULL,
                        eddy_name = NULL,
                        envir = get_default_env()) {
    
    if (!base::exists(eddy_name, where = envir, inherits = FALSE)) {
        stop("Cannot find eddy with name: ", eddy_name)
    } else {
        eddy <- envir[[eddy_name]]
        eddy$reset()
        unlink(eddy$cache_path, recursive = TRUE)
        rm(list = eddy_name, envir = envir, inherits = FALSE)
    }
}
