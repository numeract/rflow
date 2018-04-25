# functions to indicate data sources for rflow (monitor for changes, etc.)


# !diagnostics suppress=.


make_file_source <- function(fn,
                             file_arg = 1,
                             split_output_fn = NULL,
                             eddy = get_default_eddy()
) {
    # follow make_rflow, with some changes
    
    stopifnot(is.function(fn))
    stopifnot(
        rlang::is_scalar_character(file_arg) || 
        rlang::is_scalar_integerish(file_arg)
    )
    if (!is.null(split_output_fn)) stopifnot(is.function(split_output_fn))
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
            fn_source_arg = file_arg,
            hash_input_fn = NULL,
            split_output_fn = split_output_fn,
            eddy = eddy
        )
    }
    
    rflow$rf_fn
}
