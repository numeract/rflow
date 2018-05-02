# wrappers around R6Flow following standard R functionality


# !diagnostics suppress=.


make_fn_key <- function(fn, eddy) {
    
    # unique fn_key = hash of fn's defined arguments and body
    fn_formals <- formals(args(fn))
    arg_chr <- paste(
        paste(names(fn_formals), as.character(fn_formals), sep = "="),
        collapse = ", "
    )
    body_chr <- as.character(body(fn))
    fn_key <- eddy$digest(c(arg_chr, body_chr))
    
    fn_key
}


#' Cache a function.
#'
#' @param fn Function to be cached, ideally a pure function.
#' @param hash_input_fn Custom function to process only a part of the input
#'   (e.g. skip one fn inputs - not recommended).
#' @param split_output_fn Custom function to split the output into a list,
#'  in order to hash its elements separately.
#' @param eddy R6Eddy object were the data should be stored.
#' 
#' @return The cached version of the function.
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
    stopifnot(inherits(eddy, "R6Eddy"))
    
    # best place to capture the name of the function
    # fn_name (the binding) is irrelevant (it's the args and body that matter)
    # but it is useful from the point of view of the human mind (debug)
    match_call <- match.call()
    if (is.symbol(match_call$fn)) {
        fn_name <- as.character(match_call$fn)
    } else {
        # anonymous function
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
            split_output_fn = split_output_fn,
            eddy = eddy
        )
    }
    
    rflow$rf_fn
}


#' @importFrom dplyr collect
#' @name collect
#' @rdname collect.R6Flow
#' @export
NULL


#' Get the data from an \code{R6Flow} or an \code{R6FlowElement} object.
#' 
#' @param x Function cached with RFlow.
#' @param ... Element of the output data to be selected. If present, it should
#'   be named \code{name}, otherwise the first item of the \code{...} list
#'   will be used. The default is \code{name = NULL}, which returns all the
#'   data.
#' 
#' @return Data associated with the output of the function.
#' 
#' @export
#' @method collect R6Flow
collect.R6Flow <- function(x, ...) {
    
    arg_lst <- list(...)
    arg_name <- if ("name" %in% arg_lst || length(arg_lst) == 0L) {
        # NULL if no arguments present
        arg_lst$name
    } else {
        # there is at least one argument, not `name`; assume it is `name`
        arg_lst[[1L]]
    }
    x$collect_data(name = arg_name)
}


#' @rdname collect.R6Flow
#' @export
#' @method collect R6FlowElement
collect.R6FlowElement <- function(x, ...) {
    
    if (length(list(...)) > 0L) warning("all arguments ignored")
    
    x$self$collect_data(name = x$elem_name)
}



#' Extract an element from an \code{R6Flow} object.
#' 
#' @param x Function cached with RFlow.
#' @param name Element of the output data to be selected. The default is 
#'   \code{name = NULL}, which returns the element version of the \code{R6Flow} 
#'   input object.
#' 
#' @return An object with class \code{R6FlowElement}.
#' 
#' @export
element <- function(x, name = NULL) {
    
    stopifnot(inherits(x, "R6Flow"))
    
    x$get_element(name = name)
}


#' @rdname element
#' @export
`[.R6Flow` <- function(x, name) {
    
    if (missing(name)) {
        x$get_element()
    } else {
        x$get_element(name = name)
    }
}
