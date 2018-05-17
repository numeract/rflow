# wrappers around R6Eddy following standard R functionality


# !diagnostics suppress=.


# create a separate binding environment to keep eddies
.EDDY_ENV <- new.env(parent = emptyenv())


# Each eddy_env has a string variable .CURRENT_NAME that stores the name
# of the current eddy. This to avoid duplicate binding of the eddy_end
# to help with gc()
.EDDY_ENV[[".CURRENT_NAME"]] <- NA_character_


#' Get the default binding environment that keeps the eddies.
#' 
#' @return An environment.
#' 
#' @export
default_eddy_env <- function() {
    .EDDY_ENV
}


#' Create a new eddy. Does not affect the current eddy.
#' 
#' @param eddy_name Unique name for the eddy to allow retrieving later.
#' @param cache An cache object returned by one of the \code{cache} functions.
#' @param eddy_env An environment where to put (bind) the eddy.
#' 
#' @return An eddy object to be used for storing rflows.
#' 
#' @family eddy functions
#' 
#' @export
new_eddy <- function(eddy_name,
                     cache = default_cache(),
                     eddy_env = default_eddy_env()) {
    # using the name `new_eddy`, not `add_eddy` to convey fresh/empty idea
    # new guarantees a clean eddy (for tests)
    
    stopifnot(rlang::is_string(eddy_name))
    stopifnot(eddy_name != ".CURRENT_NAME")
    stopifnot(inherits(cache, "R6Cache"))
    stopifnot(is.environment(eddy_env))
    if (base::exists(eddy_name, where = eddy_env, inherits = FALSE)) {
        stop("Cannot create a new eddy, name already present: ", eddy_name)
    }
    
    eddy <- R6Eddy$new(cache = cache)
    assign(eddy_name, eddy, envir = eddy_env)
    
    eddy
}


#' Retrieves an eddy. 
#' 
#' @param eddy_name Unique name for the eddy to allow retrieving later.
#' @param eddy_env An environment where to put (bind) the eddy.
#' 
#' @return An eddy object to be used for storing rflows.
#' 
#' @family eddy functions
#' 
#' @export
get_eddy <- function(eddy_name,
                     eddy_env = default_eddy_env()) {
    
    stopifnot(rlang::is_string(eddy_name))
    stopifnot(eddy_name != ".CURRENT_NAME")
    stopifnot(is.environment(eddy_env))
    if (!base::exists(eddy_name, where = eddy_env, inherits = FALSE)) {
        stop("Cannot find eddy with name: ", eddy_name)
    }
    
    eddy <- eddy_env[[eddy_name]]
    stopifnot(inherits(eddy, "R6Eddy"))
    
    eddy
}


#' Delete eddy and ALL its data from ALL cache layers.
#' 
#' @param eddy_name Unique name for the eddy to allow retrieving later.
#' @param eddy_env An environment where to put (bind) the eddy.
#' 
#' @return Nothing (\code{NULL}).
#' 
#' @family eddy functions
#' 
#' @export
delete_eddy <- function(eddy_name,
                        eddy_env = default_eddy_env()) {
    
    stopifnot(rlang::is_string(eddy_name))
    stopifnot(eddy_name != ".CURRENT_NAME")
    stopifnot(is.environment(eddy_env))
    if (!base::exists(eddy_name, where = eddy_env, inherits = FALSE)) {
        stop("Cannot find eddy with name: ", eddy_name)
    }
    
    eddy <- eddy_env[[eddy_name]]
    eddy$terminate()
    rm(list = eddy_name, envir = eddy_env, inherits = FALSE)
    
    # update the .CURRENT_NAME, if needed
    if (eddy_env[[".CURRENT_NAME"]] == eddy_name) {
        eddy_env[[".CURRENT_NAME"]] <- NA_character_
    }
    
    invisible(NULL)
}


#' Set the current eddy to be used in future rflow calls.
#' 
#' @param eddy_name Unique name for the eddy to allow retrieving later.
#' @param eddy_env An environment where to put (bind) the eddy.
#' 
#' @return An eddy object to be used for storing rflows.
#' 
#' @family eddy functions
#' 
#' @export
set_current_eddy <- function(eddy_name,
                             eddy_env = default_eddy_env()) {
    
    stopifnot(rlang::is_string(eddy_name))
    stopifnot(eddy_name != ".CURRENT_NAME")
    stopifnot(is.environment(eddy_env))
    if (!base::exists(eddy_name, where = eddy_env, inherits = FALSE)) {
        stop("Cannot find eddy with name: ", eddy_name)
    }
    
    eddy_env[[".CURRENT_NAME"]] <- eddy_name
    
    eddy <- eddy_env[[eddy_name]]
    invisible(eddy)
}    


#' Get the current eddy for a given (or default) environment.
#' 
#' If the current eddy was not previously set with \code{set_current_eddy},
#'   it creates a new eddy that uses \code{default_cache()}.
#' 
#' @param eddy_env An environment where to put (bind) the eddy.
#' 
#' @return An eddy object to be used for storing rflows.
#' 
#' @family eddy functions
#' 
#' @export
get_current_eddy <- function(eddy_env = default_eddy_env()) {
    
    stopifnot(is.environment(eddy_env))
    
    eddy_name <- eddy_env[[".CURRENT_NAME"]]
    if (is.null(eddy_name) ||
        is.na(eddy_name) ||
        !base::exists(eddy_name, where = eddy_env, inherits = FALSE)
    ) {
        # no current name, create the default eddy
        eddy_name <- "default_eddy"
        eddy <- new_eddy(
            eddy_name,
            cache = default_cache(),
            eddy_env = eddy_env)
        eddy_env[[".CURRENT_NAME"]] <- eddy_name
    } else {
        eddy <- eddy_env[[eddy_name]]
        stopifnot(inherits(eddy, "R6Eddy"))
    }
    
    eddy
}
