# wrappers around R6Flow following standard R functionality


# !diagnostics suppress=., 


#' Explicit cache of a function.
#' @details 
#' In order to use the functionality of an \code{R6Flow} object, the output of 
#' \code{make_flow_fn} has to be collected first.
#' @param fn Function to be cached, ideally a pure function.
#' @param fn_id Optional id to uniquely identify the function. By default,
#'   rflow functions reuse the \code{cache} if the same function is given. The id 
#'   allows the user to suppress console messages and to explicitly
#'   indicate whether to reuse the old \code{cache} or create a new one.
#' @param flow_options List of options created using \code{get_flow_options}.
#' 
#' @return The cached version of the function.
#' 
#' @examples 
#' fn <- function(x, y) { x + y + 1 }
#' make_flow_function <- make_flow_fn(fn)
#' rflow_function <- make_flow_function(2, 3)
#' flow_result <- rflow_function %>% collect()
#' 
#' # usage with rflow pipes
#' fn2 <- function(x, y) { x * y }
#' make_flow_function2 <- make_flow_fn(fn2)
#' collected_pipe_flow <- make_flow_function(1, 2) %>%
#'     make_flow_function2(2) %>%
#'     collect()
#' 
#' @export
make_flow_fn <- function(fn,
                         fn_id = NULL,
                         flow_options = get_flow_options()) {
    # best place to capture the name of the function
    # fn_name (the binding) is irrelevant (it's the args and body that matter)
    # but it is useful for fn_id hints
    stopifnot(!is.null(fn))
    match_call <- match.call()
    use <- make_key(match_call$fn, fn, fn_id, flow_options, "R6Flow")
    eddy <- flow_options$eddy
    
    if (use$action == "get") {
        flow <- eddy$get_flow(use$fn_key)
    } else {
        flow <- R6Flow$new(
            fn = fn,
            fn_key = use$fn_key,
            fn_name = use$fn_name,
            fn_id = use$fn_id,
            flow_options = flow_options
        )
    }
    
    flow$rf_fn
}


#' Implicit cache of a function and of the given call.
#' 
#' @details 
#'   Arguments \code{fn}, \code{fn_id} and \code{flow_options}, when provided,
#'   must be named. Argument \code{fn} must be always provided.
#' 
#' @param ... Named arguments to pass to \code{fn}.
#' @param fn The function to apply to the data frame. It must accept a data
#'   frame as the first argument and a numeric index as the second argument.
#' @param fn_id Optional id to uniquely identify the function. By default,
#'   rflow functions reuse the \code{cache} if the same function is given. The id 
#'   allows the user to suppress console messages and to explicitly
#'   indicate whether to reuse the old \code{cache} or create a new one.
#' @param flow_options List of options created using \code{get_flow_options}.
#' 
#' @return The flow object.
#' 
#' @examples 
#' fn <- function(x, y) { x + y + 2 }
#' flowed_fn <- flow_fn(2, 3, fn = fn)
#' collected_result <- flowed_fn %>%
#'     collect()
# 
#' # usage with rflow pipes
#' fn2 <- function(x, y) { x * y }
#' collected_pipe_result <- flowed_fn %>%
#'     flow_fn(2, fn = fn2) %>%
#'     collect()
#' 
#' @export
flow_fn <- function(..., 
                    fn = NULL,
                    fn_id = NULL,
                    flow_options = get_flow_options()
) {
    stopifnot(!is.null(fn))
    match_call <- match.call()
    use <- make_key(match_call$fn, fn, fn_id, flow_options, "R6Flow")
    eddy <- flow_options$eddy
    
    if (use$action == "get") {
        flow <- eddy$get_flow(use$fn_key)
    } else {
        flow <- R6Flow$new(
            fn = fn,
            fn_key = use$fn_key,
            fn_name = use$fn_name,
            fn_id = use$fn_id,
            flow_options = flow_options
        )
    }
    
    do.call(
        what = flow$rf_fn, 
        args = list(...),
        envir = parent.frame(n = 2)
    )
}


#' Implicit cache of a function and of the given call.
#' 
#' @param fn_call Function call to be processed.
#' @param fn_id Character or Integer. Optional id to uniquely identify 
#'   the function. By default, rflow functions reuse the \code{cache} if the 
#'   same function is given. The \code{fn_id} allows the user to suppress 
#'   console messages and  to explicitly indicate whether to reuse the old
#'   \code{cache} or create a new one.
#' @param flow_options List of options created using \code{get_flow_options}.
#' 
#' @return The flow object.
#' 
#' @examples
#' fn <- function(x, y) { x + y + 3 }
#' call_flow <- flow_call(fn(x = 1, y = 2))
#' collected_result <- call_flow %>% collect()
#' 
#' @export
flow_call <- function(fn_call, 
                      fn_id = NULL,
                      flow_options = get_flow_options()) {
    
    # fn_call will be replaced by its original call
    # this is to avoid triggering evaluation of fn_call
    fn_call <- parse_call()
    fn_name <- fn_call[[1L]]
    fn <- eval(fn_name)
    stopifnot(!is.null(fn))
    use <- make_key(fn_name, fn, fn_id, flow_options, "R6Flow")
    eddy <- flow_options$eddy
    
    if (use$action == "get") {
        flow <- eddy$get_flow(use$fn_key)
    } else {
        flow <- R6Flow$new(
            fn = fn,
            fn_key = use$fn_key,
            fn_name = use$fn_name,
            fn_id = use$fn_id,
            flow_options = flow_options
        )
    }
    
    # unlike make_flow_fn, we have a fn call to eval (in the parent.frame!)
    fn_call[[1L]] <- flow$rf_fn
    eval(fn_call, envir = parent.frame())
}


#' @importFrom dplyr collect
#' @name collect
#' @rdname collect.R6Flow
#' @export
NULL


#' Get the data from an \code{R6Flow} or an \code{Element} object.
#' 
#' @param x A flow object, e.g. as returned by \code{\link{flow_fn}}.
#' @param ... Name of the element of the output data to be selected. 
#'   If present, it must be named \code{name}, otherwise the first 
#'   item of the \code{...} list will be used.
#'   The default is \code{name = NULL}, which returns all the data.
#'   Ignored if \code{x} is an \code{Element} object.
#' 
#' @return Data associated with the output of the function.
#' 
#' @method collect R6Flow
#' 
#' @examples 
#' fn <- function(x, y) { x + y + 4 }
#' flowed_fn <- flow_fn(2,3, fn = fn) 
#' flow_result <- flowed_fn %>% collect()
#' 
#' @export
collect.R6Flow <- function(x, ...) {
    
    arg_lst <- list(...)
    arg_name <- if ("name" %in% names(arg_lst) || length(arg_lst) == 0L) {
        # NULL if no arguments present
        arg_lst$name
    } else {
        # there is at least one argument, not `name`; assume it is `name`
        arg_lst[[1L]]
    }
    x$collect(name = arg_name)
}


#' @rdname collect.R6Flow
#' 
#' @method collect Element
#' 
#' @examples 
#' fn <- function(x, y) { x + y + 5 }
#' flowed_fn <- flow_fn(2, 3, fn = fn)
#' flow_result <- flowed_fn %>%
#'     collect()
#' flow_element <- element(flowed_fn, "x")
#' collected_element_value <- collect(flow_element)
#' 
#' @export
collect.Element <- function(x, ...) {
    
    if (length(list(...)) > 0L) {
        rlang::warn("First arg is an rflow Element, the other args are ignored")
    }
    
    x$self$collect(name = x$elem_name)
}


#' @importFrom dplyr compute
#' @name compute
#' @rdname compute.R6Flow
#' @export
NULL


#' Trigger computation for an \code{R6Flow} or an \code{Element} object.
#' 
#' @details 
#'    Unlike \code{collect}, it does not trigger an error if it fails 
#'    to compute and it does not return the actual result of the computation.
#' 
#' @param x A flow object, e.g. as returned by \code{\link{flow_fn}}.
#' @param ... Any other arguments will be ignored.
#' 
#' @return Logical, whether the result is available to be collected.
#' 
#' @method compute R6Flow
#' @export
compute.R6Flow <- function(x, ...) {
    
    if (length(list(...)) > 0L) {
        rlang::warn("First arg is an rflow, the other args are ignored")
    }
    
    x$compute()
}


#' @rdname compute.R6Flow
#' 
#' @method compute Element
#' @export
compute.Element <- function(x, ...) {
    
    if (length(list(...)) > 0L) {
        rlang::warn("First arg is an rflow Element, the other args are ignored")
    }
    
    x$self$compute()
}


#' Extract an element from an \code{R6Flow} object.
#' 
#' @param flow A flow object, e.g. as returned by \code{\link{flow_fn}}.
#' @param name Element of the output data to be selected. The default is 
#'   \code{name = NULL}, which returns the element version of the \code{R6Flow} 
#'   input object.
#' 
#' @return An object with class \code{Element}.
#' 
#' @examples 
#' fn <- function(x, y) { x + y + 6 }
#' flowed_fn <- flow_fn(2, 3, fn = fn) 
#' flow_element <- element(flowed_fn, "x")
#' 
#' @export
element <- function(flow, name = NULL) {
    
    stopifnot(inherits(flow, "R6Flow"))
    
    flow$get_element(name = name)
}


#' @rdname element
#' 
#' @examples 
#' fn <- function(x, y) { x + y + 7 }
#' flowed_fn <- flow_fn(2, 3, fn = fn)
#' element_name <- flowed_fn["x"]$elem_name
#' 
#' @export
`[.R6Flow` <- function(flow, name) {
    
    if (missing(name)) {
        flow$get_element()
    } else {
        flow$get_element(name = name)
    }
}


#' Does the flow have a "current" state? 
#' 
#' If there is no current state, e.g. right after \code{\link{make_flow_fn}},
#'   the flow is "not flowing", it is preventing downstream flows
#'   from being computed.
#' 
#' @param flow A flow object, e.g. as returned by \code{\link{flow_fn}}.
#' 
#' @return A logical value, whether the current state is valid.
#' 
#' @examples 
#' fn <- function(x, y) { x + y + 8 }
#' flowed_fn <- flow_fn(1, 2, fn = fn) 
#' is_current_flow <- is_current(flowed_fn)
#' 
#' @export
is_current <- function(flow) {
    
    stopifnot(inherits(flow, "R6Flow"))
    
    flow$is_current
}


index_of_state <- function(flow, state) {
    
    stopifnot(inherits(flow, "R6Flow"))
    stopifnot(is_key(state) || flow$is_good_index(state))
    
    if (rlang::is_scalar_integerish(state)) {
        return(as.integer(state))
    }
    
    if (state == "current") {
        return(flow$state_index)
    } 
    
    index <- which(state %in% flow$state$in_hash)
    if (length(index) > 0L) {
        return(index)
    }
    
    index <- which(state %in% flow$state$out_hash)
    if (length(index) > 0L) {
        return(index)
    }
    
    # no valid index
    0L
}


#' Is the current state valid (stored in the cache)?
#' 
#' @param flow A flow object, e.g. as returned by \code{\link{flow_fn}}.
#' @param state A flow state. It can be either a valid state 
#'   \code{index} (integer) or a valid state: \code{"current"}, \code{"all"}, 
#'   \code{in_hash} or \code{out_hash} (string).
#' 
#' @return A logical value, whether the value can be obtained without
#'   triggering computation.
#' 
#' @examples 
#' fn <- function(x, y) { x + y + 9 }
#' flowed_fn <- flow_fn(2, 3, fn = fn) 
#' is_valid_flow <- is_valid(flowed_fn)
#'
#' @export
is_valid <- function(flow, state = "current") {
    
    index <- index_of_state(flow, state)
    flow$require_good_index(index)
    flow$is_valid_at_index(index)
}


#' Forgets the computation for the current state.
#' 
#' @param flow A flow object, e.g. as returned by \code{\link{flow_fn}}.
#' @param state A flow state. It can be either a valid state 
#'   \code{index} (integer) or a valid state: \code{"current"}, \code{"all"}, 
#'   \code{in_hash} or \code{out_hash} (string).
#' 
#' @return A logical value, whether the deletion was successful.
#' 
#' @examples 
#' fn <- function(x, y) { x + y + 10 }
#' flowed_fn <- flow_fn(2, 3, fn = fn)
#' forget(flowed_fn)
#' 
#' @export
forget <- function(flow, state = "current") {
    
    index <- index_of_state(flow, state)
    if (index == 0L) {
        # one reason state was not found
        if (state == "all") {
            flow$forget_all()
            flow$save()
        } else {
            rlang::abort(paste("Cannot forget; cannot find state:", state))
        }
    } else {
        flow$forget_state(index)
        flow$save()
    }
    
    flow
}


#' Is the object a flow object or a flow element?
#' 
#' @param x An object.
#' 
#' @return A logical value, whether \code{x} is a flow object.
#' 
#' @examples 
#' fn <- function(x, y) { x + y + 11 }
#' flow_function <- flow_fn(1, 2, fn = fn)
#' is_input_flow <- is_flow(flow_function)
#' 
#' @export
is_flow <- function(x) {
    
    inherits(x, "R6Flow") || inherits(x, "Element")
}


#' Is the function a flow function (as returned by \code{make_flow_fn})?
#' 
#' @param fn A function.
#' 
#' @return A logical value, whether \code{fn} is a flow function.
#' 
#' @examples 
#' fn <- function(x, y) { x + y + 12 }
#' flowed_function <- flow_fn(2, 3, fn = fn)
#' is_flow_function <- is_flow_fn(fn = flowed_function)
#' 
#' @export
is_flow_fn <- function(fn) {
    
    if (!is.function(fn)) return(FALSE)
    flow <- environment(fn)$self
    inherits(flow, "R6Flow")
}


#' Check if the function is NOT a flow function
#' (as returned by \code{make_flow_fn}). 
#' 
#' @param fn A function.
#' 
#' @return A logical value, whether \code{fn} is a not flow function.
is_not_flow_fn <- function(fn) {
    
    if (!is.function(fn)) return(FALSE)
    flow <- environment(fn)$self
    !inherits(flow, "R6Flow")
}
