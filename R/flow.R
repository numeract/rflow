#' @include r6flow.R


make_rflow <- function(fn,
                       hash_input_fn = NULL,
                       split_output_fn = NULL,
                       eddy = get_default_eddy()
) {
    # best place to capture the name of the function
    # fn_name (the binding) is irrelevant (it's the args and body that matter)
    # but it is useful from the point of view of the human mind (debug)
    mc <- match.call()
    stopifnot(is.function(fn))
    if (is.symbol(mc$fn)) {
        fn_name <- as.character(mc$fn)
    } else {
        # anonymous function
        fn_name <- "anonymous"
    }
    
    # do input validation here, keep R6Flow initialize simpler
    if (!is.null(hash_input_fn)) {
        stopifnot(is.function(hash_input_fn))
        fn_formals <- formals(args(fn))
        hash_input_fn_formals <- formals(args(hash_input_fn))
        stopifnot(identical(fn_formals, hash_input_fn_formals))
    }
    if (!is.null(split_output_fn)) stopifnot(is.function(split_output_fn))
    stopifnot(inherits(eddy, 'R6Eddy'))
    
    r6flow <- R6Flow$new(
        fn = fn,
        fn_name = fn_name,
        hash_input_fn = hash_input_fn,
        split_output_fn = split_output_fn,
        eddy = eddy
    )
    rf_fn <- r6flow$rf_fn
    
    rf_fn
}
