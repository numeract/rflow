# functions to indicate data sinks for rflow objects (updated only on change)


# !diagnostics suppress=.


#' Cache a function - TODO.
#' 
#' @param fn Function to be cached, ideally a pure function.
#' @param hash_input_fn Custom function to process only a part of the input
#'   (e.g. skip one fn inputs - not recommended).
#' @param eddy R6Eddy object were the data should be stored.
#' 
#' @return The cached version of the function.
#' 
#' @export
make_sink <- function(fn,
                      hash_input_fn = NULL,
                      eddy = get_default_eddy()
) {
    # follow make_rflow, with some changes
    # sink cannot be a function as collect() because we need to store the 
    # state of each sink while dealing with multiple sinks
    
    stopifnot(is.function(fn))
    if (!is.null(hash_input_fn)) {
        stopifnot(is.function(hash_input_fn))
        fn_formals <- formals(args(fn))
        hash_input_fn_formals <- formals(args(hash_input_fn))
        stopifnot(identical(fn_formals, hash_input_fn_formals))
    }
    stopifnot(inherits(eddy, "R6Eddy"))
    
    match_call <- match.call()
    if (is.symbol(match_call$fn)) {
        fn_name <- as.character(match_call$fn)
    } else {
        fn_name <- "anonymous"
    }
    fn_key <- make_fn_key(fn, eddy)
    
    if (eddy$find_rflow(fn_key) == "memory") {
        rflow <- eddy$get_rflow(fn_key)
    } else {
        rflow <- R6Flow$new(
            fn = fn,
            fn_key = fn_key,
            fn_name = fn_name,
            fn_source_arg = NULL,
            hash_input_fn = hash_input_fn,
            split_output_fn = NULL,
            eddy = eddy
        )
        rflow$rf_fn <- rflow$rf_fn_sink
        formals(rflow$rf_fn) <- formals(args(fn))
    }
    
    rflow$rf_fn
}


#' Assigns a value to a name space (an environment or a reactiveValues).
#' 
#' @param x Value to assign.
#' @param var The name (as string) of the variable.
#' @param ns The name space, either an \code{environment} or a 
#'   \code{Shiny reactiveValues} object.
#' 
#' @return The initial value, \code{x}
#' 
#' @export
to_ns <- function(x, var, ns) {
    
    stopifnot(rlang::is_string(var))
    
    if (is.environment(ns)) {
        assign(var, x, envir = ns)
    } else {
        ns[[var]] <- x
    }
    
    x
}


#' Cache a function - TODO.
#' 
#' @param fn Function to be cached, the default works with environments and 
#'   reactiveValues.
#' @param eddy R6Eddy object were the data should be stored.
#' 
#' @return The cached version of the function.
#' 
#' @export
make_ns_sink <- function(fn = to_ns,
                         eddy = get_default_eddy()) {
    
    stopifnot(is.function(fn))
    stopifnot(inherits(eddy, "R6Eddy"))
    
    match_call <- match.call()
    if (is.symbol(match_call$fn)) {
        fn_name <- as.character(match_call$fn)
    } else {
        # called with default fn arg, does not show in match.call
        fn_name <- "to_ns"
    }
    fn_key <- make_fn_key(fn, eddy)
    
    if (eddy$find_rflow(fn_key) == "memory") {
        rflow <- eddy$get_rflow(fn_key)
    } else {
        rflow <- R6Flow$new(
            fn = fn,
            fn_key = fn_key,
            fn_name = fn_name,
            fn_source_arg = NULL,
            hash_input_fn = NULL,
            split_output_fn = NULL,
            eddy = eddy
        )
        rflow$rf_fn <- rflow$rf_fn_sink
        formals(rflow$rf_fn) <- formals(args(fn))
        rflow$calc_in_hash <- rflow$calc_in_hash_ns_sink
    }
    
    rflow$rf_fn
}
