# wrappers around R6Flow following standard R functionality


# !diagnostics suppress=.


make_fn_key <- function(fn, eddy = get_default_eddy()) {
    
    # unique fn_key = hash of fn's defined arguments and body
    fn_formals <- formals(args(fn))
    arg_chr <- paste(
        paste(names(fn_formals), as.character(fn_formals), sep = '='),
        collapse = ', '
    )
    body_chr <- as.character(body(fn))
    fn_key <- eddy$digest(c(arg_chr, body_chr))
    
    fn_key
}

#' Cache a function.
#'
#' @param fn Function to be cached.
#' @param hash_input_fn Custom input hash function (e.g. a subset of RFlow output)
#' @param split_output_fn Function to split the output into a list,
#'  in order to hash its elements separately.
#' @param eddy R6Eddy object were the data should be stored.
#' 
#' @return The cached version of the inputted function.
#'
#' @export
make_rflow <- function(fn,
                       hash_input_fn = NULL,
                       split_output_fn = NULL,
                       eddy = get_default_eddy()
) {
    # do input validation here, keep R6Flow initialize simpler
    stopifnot(is.function(fn))
    if (!is.null(hash_input_fn)) {
        stopifnot(is.function(hash_input_fn))
        fn_formals <- formals(args(fn))
        hash_input_fn_formals <- formals(args(hash_input_fn))
        stopifnot(identical(fn_formals, hash_input_fn_formals))
    }
    if (!is.null(split_output_fn)) stopifnot(is.function(split_output_fn))
    stopifnot(inherits(eddy, 'R6Eddy'))
    
    # best place to capture the name of the function
    # fn_name (the binding) is irrelevant (it's the args and body that matter)
    # but it is useful from the point of view of the human mind (debug)
    mc <- match.call()
    if (is.symbol(mc$fn)) {
        fn_name <- as.character(mc$fn)
    } else {
        # anonymous function
        fn_name <- "anonymous"
    }
    fn_key <- make_fn_key(fn, eddy)
    
    if (eddy$find_rflow(fn_key) == 'memory') {
        rflow <- eddy$get_rflow(fn_key)
    } else {
        rflow <- R6Flow$new(
            fn = fn,
            fn_key = fn_key,
            fn_name = fn_name,
            hash_input_fn = hash_input_fn,
            split_output_fn = split_output_fn,
            eddy = eddy
        )
    }
    
    rflow$rf_fn
}

#' Get data from an RFlow object.
#'
#' @param rf_fn Function cached with RFlow.
#' @param what Element of the output data to be selected.
#' 
#' @return Data associated with the output of the function.
#'
#' @export
collect <- function(rf_fn, what = NULL) {
    
    if (inherits(rf_fn, "R6FlowElement")) {
        rf_fn$self$collect(what = what)
    } else if (inherits(rf_fn, "R6Flow")) {
        rf_fn$collect(what = what)
    } else {
        stop("Not an rflow object or element of an rflow output")
    }
}
