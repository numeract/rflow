#' @include r6flow.R


make_rflow <- function(fn,
                       hash_input = NULL,                # TODO
                       subset_output = NULL,             # TODO
                       eddy = get_default_eddy()
) {
    # best place to capture the name of the function
    mc <- match.call()
    fn_name <- as.character(mc$fn)
    
    r6flow <- R6Flow$new(
        fn = fn,
        fn_name = fn_name,
        eddy = eddy
    )
    rf_fn <- r6flow$rf_fn
    
    rf_fn
}



# TODO: forget_rflow
