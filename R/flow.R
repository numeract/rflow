#' @include r6flow.R


make_rflow <- function(fn,
                       hash_input = NULL,                # TODO
                       split_output_fn = NULL,
                       eddy = get_default_eddy()
) {
    # best place to capture the name of the function
    mc <- match.call()
    fn_name <- as.character(mc$fn)
    
    stopifnot(is.function(fn))
    if (!is.null(split_output_fn)) stopifnot(is.function(split_output_fn))
    
    r6flow <- R6Flow$new(
        fn = fn,
        fn_name = fn_name,
        split_output_fn = split_output_fn,
        eddy = eddy
    )
    rf_fn <- r6flow$rf_fn
    
    rf_fn
}


# TODO: is already rflowed
# TODO: forget_rflow
