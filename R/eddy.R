# wrappers around R6Eddy following standard R functionality


# !diagnostics suppress=.


# create a separate binding environment to keep eddies
.EDDY_ENV <- new.env(parent = emptyenv())


#' Get the default binding environment that keeps the eddies.
#' 
#' @return An environment.
#' 
#' @export
default_eddy_env <- function() {
    .EDDY_ENV
}


# if no eddy name is given, use this name to access the eddy inside .EDDY_ENV 
.EDDY_DEFAULT_NAME <- "default_eddy"


#' Get the default eddy name.
#' 
#' @return A character vector of length one.
#' 
#' @export
default_eddy_name <- function() {
    .EDDY_DEFAULT_NAME
}


#' Create a new eddy.
#' 
#' @param eddy_name Unique name for the eddy to allow retrieving later.
#' @param cache An cache object returned by one of the \code{cache} functions.
#' @param eddy_env An environment where to put (bind) the eddy.
#' 
#' @family eddy functions
#' 
#' @return An eddy object to be used for storing rflows.
#' 
#' @export
new_eddy <- function(eddy_name,
                     cache = default_cache(),
                     eddy_env = default_eddy_env()) {
    # using the name `new_eddy`, not `add_eddy` to convey fresh/empty idea
    # new guarantees a clean eddy (for tests)
    
    stopifnot(rlang::is_string(eddy_name))
    if (eddy_name == default_eddy_name()) {
        stop("Cannot create a new eddy, avoid the default name: ", eddy_name)
    }
    stopifnot(inherits(cache, "R6Cache"))
    stopifnot(is.environment(eddy_env))
    if (base::exists(eddy_name, where = eddy_env, inherits = FALSE)) {
        stop("Cannot create a new eddy, name already present: ", eddy_name)
    }
    
    eddy <- R6Eddy$new(cache = cache)
    assign(eddy_name, eddy, envir = eddy_env)
    
    eddy
}


#' @export
#' 
#' @family eddy functions
#' 
#' @rdname new_eddy
get_eddy <- function(eddy_name,
                     eddy_env = default_eddy_env()) {
    
    stopifnot(rlang::is_string(eddy_name))
    if (!base::exists(eddy_name, where = eddy_env, inherits = FALSE)) {
        stop("Cannot find eddy with name: ", eddy_name)
    }
    
    eddy <- eddy_env[[eddy_name]]
    stopifnot(inherits(eddy, "R6Eddy"))
    
    eddy
}


#' Delete eddy and ALL its data from ALL cache layers.
#' 
#' @family eddy functions
#' 
#' @export
delete_eddy <- function(eddy_name,
                        eddy_env = default_eddy_env()) {
    
    stopifnot(rlang::is_string(eddy_name))
    if (!base::exists(eddy_name, where = eddy_env, inherits = FALSE)) {
        stop("Cannot find eddy with name: ", eddy_name)
    }
    
    eddy <- eddy_env[[eddy_name]]
    eddy$reset(all_objects = TRUE)
    rm(list = eddy_name, envir = eddy_env, inherits = FALSE)
    
    invisible(NULL)
}


#' Set the default eddy to be used in future rflow calls.
#' 
#' @return An eddy object to be used for storing rflows.
#' 
#' @family eddy functions
#' 
#' @export
set_default_eddy <- function(eddy_name,
                             eddy_env = default_eddy_env()) {
    
    stopifnot(rlang::is_string(eddy_name))
    if (!base::exists(eddy_name, where = eddy_env, inherits = FALSE)) {
        stop("Cannot find eddy with name: ", eddy_name)
    }
    
    eddy <- eddy_env[[eddy_name]]
    eddy_env[[default_eddy_name()]] <- eddy
    
    invisible(eddy)
}    


#' Get the default eddy for a given (or default) environment.
#' 
#' If the default eddy not previously set with \code{set_default_eddy},
#'   it creates an eddy with \code{default_cache()}.
#' 
#' @return An eddy object to be used for storing rflows.
#' 
#' @family eddy functions
#' 
#' @export
get_default_eddy <- function(eddy_env = default_eddy_env()) {
    
    eddy_name <- default_eddy_name()
    if (base::exists(eddy_name, where = eddy_env, inherits = FALSE)) {
        eddy <- eddy_env[[eddy_name]]
        stopifnot(inherits(eddy, "R6Eddy"))
    } else {
        eddy_name <- "default_memory"
        eddy <- new_eddy(
            eddy_name,
            cache = default_cache(),
            eddy_env = eddy_env)
        eddy_env[[default_eddy_name()]] <- eddy
    }
    
    eddy
}
