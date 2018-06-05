# functions to create namespace data sinks (updated only on change)


# !diagnostics suppress=., self, private, super


# R6NsSink ----
R6NsSink <- R6::R6Class(
    classname = "R6NsSink",
    inherit = R6Flow
)


# calc_in_hash ----
R6NsSink$set("public", "calc_in_hash_ns_sink", function(
    rf_env = parent.frame()
) {
    # similar to calc_in_hash_default, but hash envir by address not content
    
    elem_args <- rf_env$elem_args
    data_hash_args <-
        elem_args %>% 
        discard_at(self$excluded_arg) %>%
        purrr::map_if(
            .p = ~ inherits(., "Element"),
            .f = function(x) {
                x$self$require_good_index()
                state <- x$self$get_state()
                list(
                    fn_key = state$fn_key,
                    in_hash = state$in_hash,
                    elem_name = x$elem_name
                )
            }
        ) %>%
        purrr::map_if(~ is.environment(.), ~ format(.)) %>%
        # if reactivevalues, use NULL (get its address outside shiny?)
        purrr::map_if(~ identical(class(.), "reactivevalues"), ~ NULL)
    
    in_hash <- self$eddy$digest(data_hash_args)
    
    in_hash
}, overwrite = TRUE)


# rf_fn ----
R6NsSink$set("public", "rf_fn_ns_sink", function(...) {
    # follow rf_fn_default, with some changes
    
    match_call <- match.call()
    supplied_args <- as.list(match_call)[-1]
    default_args <-
        as.list(formals()) %>%
        purrr::discard(~ identical(., quote(expr = ))) %>%      # nolint
        discard_at(names(supplied_args))
    eval_args <- c(
        lapply(supplied_args, eval, envir = parent.frame()),
        lapply(default_args, eval, envir = environment(self$fn))
    )
    elem_args <-
        eval_args %>%
        purrr::map_if(
            .p = ~ inherits(., "R6Flow"),
            .f = ~ .$get_element(name = NULL)
        )
    is_recursive_lgl <- elem_args %>%
        purrr::keep(~ inherits(., "Element")) %>%
        purrr::map_lgl(~ identical(.$self$fn_key, self$fn_key))
    if (any(is_recursive_lgl)) {
        rlang::abort("Recursive calls cannot be processed.")
    }
    in_hash <- self$calc_in_hash()
    
    # state has changed?
    found_state_idx <- self$which_state(in_hash)
    if (found_state_idx > 0L) {
        if (found_state_idx != self$state_index) {
            self$state_index <- found_state_idx
            changed <- TRUE
        } else {
            # same state but no cache ==> trigger "changed"
            changed <- !self$is_valid_at_index(found_state_idx)
        }
    } else {
        self$add_state(
            in_hash = in_hash, 
            out_hash = NA_character_,
            elem_args = NULL,
            make_current = TRUE
        )
        changed <- TRUE
    }
    
    if (changed) {
        # do not use compute, save to NS in one step
        data_args <-
            elem_args %>%
            purrr::map_if(
                .p = ~ inherits(., "Element"),
                .f = function(x) {
                    x$self$collect(x$elem_name)
                }
            )
        # not interested in the output itself, there is no split
        do.call(what = self$fn, args = data_args, envir = globalenv())
        # update the current state
        self$state$out_hash[self$state_index] <- "<dumped>"
        self$state$time_stamp[self$state_index] <- now_utc()
        self$save()
    }
    
    self
}, overwrite = TRUE)


# initialize ----
R6NsSink$set("public", "initialize", function(
        fn,
        fn_key,
        fn_name,
        fn_id,
        flow_options = get_flow_options()
) {
    super$initialize(fn, fn_key, fn_name, fn_id, flow_options)
    
    # after registering into eddy, remove itself if error
    tryCatch({
        # calc_in_hash
        self$calc_in_hash <- self$calc_in_hash_ns_sink
        self$rf_fn <- self$rf_fn_ns_sink
        formals(self$rf_fn) <- formals(args(fn))
    }, error = function(e) {
        self$eddy$remove_flow(fn_key)
        stop(e)
    })
    
    invisible(NULL)
}, overwrite = TRUE)


# forget_state ----
R6NsSink$set("public", "forget_state", function(index) {
    
    self$require_good_index(index)
    
    # overwrite to reflect no data storage
    self$state$out_hash[index] <- NA_character_
    self$state$time_stamp[index] <- now_utc()
    
    TRUE
}, overwrite = TRUE)


# get_element ----
R6NsSink$set("public", "get_element", function(name = NULL) {
    
    # overwrite to disable
    rlang::warn("`get_element` is not available for R6NsSink objects")
    
    invisible(NULL)
}, overwrite = TRUE)


# compute ----
R6NsSink$set("public", "compute", function() {

    # overwrite to disable
    rlang::warn("`compute` is not available for R6NsSink objects")
    
    invisible(NULL)
}, overwrite = TRUE)


# collect ----
R6NsSink$set("public", "collect", function(name = NULL) {
    
    # overwrite to disable
    rlang::warn("`collect` is not available for R6NsSink objects")
    
    invisible(NULL)
}, overwrite = TRUE)


# is_valid_at_index ----
R6NsSink$set("public", "is_valid_at_index", function(index = NULL) {
    
    if (is.null(index)) index <- self$state_index
    if (!self$is_good_index(index)) return(FALSE)
    
    out_hash <- self$state$out_hash[index]
    if (is.na(out_hash)) return(FALSE)
    
    # unlike R6Flow, there is no cache to check
    TRUE
}, overwrite = TRUE)


#' Assigns a value to a variable in a name space.
#' 
#' @param x Value to assign.
#' @param var_name The name (as string) of the variable.
#' @param ns The name space, either an \code{environment} or a 
#'   \code{Shiny::reactiveValues} object.
#' 
#' @return The initial value, \code{x}
to_ns <- function(x, var_name, ns) {
    
    ns[[var_name]] <- x
    
    invisible(NULL)
}


#' Write a value to a namespace only if the value has changed.
#' 
#' @param x Value to assign.
#' @param var_name The name (as string) of the variable.
#' @param ns The name space, either an \code{environment} or a 
#'   \code{Shiny::reactiveValues} object.
#' @param flow_options List of options created using \code{get_flow_options}.
#'   All options except \code{excluded_arg} and \code{eddy} are ignored.
#' 
#' @return The flow object
#' 
#' @export
flow_ns_sink <- function(x,
                         var_name,
                         ns,
                         flow_options = get_flow_options()) {
    
    require_keys(var_name)
    stopifnot(is.environment(ns) || identical(class(ns), "reactivevalues"))
    
    # excluded_arg: allow args to be excluded from identifying changes
    flow_options$eval_arg_fn <- NULL
    flow_options$split_bare_list <- FALSE
    flow_options$split_dataframe <- FALSE
    flow_options$split_fn <- NULL
    
    fn_name <- "to_ns"
    fn <- to_ns
    fn_id <- var_name  # it would be nice to include a ref to ns
    use <- make_key(fn_name, fn, fn_id, flow_options, "R6NsSink")
    eddy <- flow_options$eddy
    
    if (use$action == "get") {
        flow <- eddy$get_flow(use$fn_key)
    } else {
        flow <- R6NsSink$new(
            fn = fn,
            fn_key = use$fn_key,
            fn_name = use$fn_name,
            fn_id = use$fn_id,
            flow_options = flow_options
        )
    }
    
    do.call(
        what = flow$rf_fn, 
        args = list(x = x, var_name = var_name, ns = ns), 
        envir = parent.frame(n = 2)
    )
}
