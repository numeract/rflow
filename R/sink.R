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
    }
    
    rflow$rf_fn
}
