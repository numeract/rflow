# wrappers around R6Eddy following standard R functionality


# !diagnostics suppress=., .EDDY_ENV


#' Get the default binding environment that keeps the eddies.
#' 
#' @return An environment.
#' 
#' @export
default_eddy_env <- function() {
    .EDDY_ENV
}


#' Create a new eddy. Does not affect the current eddy.
#' 
#' @param eddy_name Unique name for the eddy to allow retrieving later.
#' @param cache A cache object returned by one of the \code{cache} functions.
#' @param flow_options Options to store for future flow invocations. They
#'   do not affect the \code{eddy}, they are only stored for ease of access.
#' @param eddy_env The environment in which the eddy is put (bound).
#' 
#' @return An eddy object to be used for storing flows.
#' 
#' @family eddy functions
#' 
#' @export
new_eddy <- function(eddy_name,
                     cache = default_cache(),
                     flow_options = default_flow_options(),
                     eddy_env = default_eddy_env()) {
    # using the name `new_eddy`, not `add_eddy` to convey fresh/empty idea
    # new guarantees a clean eddy (for tests)
    
    stopifnot(rlang::is_string(eddy_name))
    stopifnot(eddy_name != ".CURRENT_NAME")
    stopifnot(inherits(cache, "R6Cache"))
    stopifnot(is.environment(eddy_env))
    if (base::exists(eddy_name, where = eddy_env, inherits = FALSE)) {
        rlang::abort(paste(
            "Cannot create a new eddy, name already present: ", eddy_name))
    }
    
    eddy <- R6Eddy$new(
        cache = cache,
        flow_options = flow_options
    )
    assign(eddy_name, eddy, envir = eddy_env)
    
    eddy
}


#' Retrieves an eddy. 
#' 
#' @param eddy_name Unique name for the eddy to allow retrieving later.
#' @param eddy_env The environment in which the eddy is put (bound).
#' 
#' @return An eddy object to be used for storing flows.
#' 
#' @family eddy functions
#' 
#' @export
get_eddy <- function(eddy_name,
                     eddy_env = default_eddy_env()) {
    
    stopifnot(rlang::is_string(eddy_name))
    stopifnot(eddy_name != ".CURRENT_NAME")
    stopifnot(is.environment(eddy_env))
    if (!base::exists(eddy_name, where = eddy_env, inherits = FALSE)) {
        rlang::abort(paste("Cannot find eddy with name: ", eddy_name))
    }
    
    eddy <- eddy_env[[eddy_name]]
    stopifnot(inherits(eddy, "R6Eddy"))
    
    eddy
}


#' Convenience function that creates a new eddy or re-uses it if present.
#' 
#' @param eddy_name Unique name for the eddy to allow retrieving later.
#' @param cache A cache object returned by one of the \code{cache} functions.
#' @param flow_options Options to store for future flow invocations. They
#'   do not affect the \code{eddy}, they are only stored for ease of access.
#' @param reuse_if_present Logical, whether or not to reuse an eddy with 
#' the same name, if already present, even if the \code{flow_options}
#'  are different.
#' @param set_current Logical, whether to make this eddy current.
#' @param eddy_env The environment in which the eddy is put (bound).
#' 
#' @return An eddy object to be used for storing flows.
#' 
#' @family eddy functions
#' 
#' @export
use_eddy <- function(eddy_name,
                     cache = default_cache(),
                     flow_options = default_flow_options(),
                     reuse_if_present = TRUE,
                     set_current = TRUE,
                     eddy_env = default_eddy_env()) {
    
    stopifnot(rlang::is_string(eddy_name))
    stopifnot(eddy_name != ".CURRENT_NAME")
    stopifnot(inherits(cache, "R6Cache"))
    stopifnot(
        rlang::is_true(reuse_if_present) || rlang::is_false(reuse_if_present))
    stopifnot(
        rlang::is_true(set_current) || rlang::is_false(set_current))
    stopifnot(is.environment(eddy_env))
    
    if (base::exists(eddy_name, where = eddy_env, inherits = FALSE)) {
        if (reuse_if_present) {
            rlang::inform(paste("Reusing eddy:", eddy_name))
            eddy <- eddy_env[[eddy_name]]
            stopifnot(inherits(eddy, "R6Eddy"))
        } else {
            rlang::abort(paste(
                "Cannot create a new eddy, name already present: ", eddy_name))
        }
    } else {
        eddy <- R6Eddy$new(
            cache = cache,
            flow_options = flow_options
        )
        assign(eddy_name, eddy, envir = eddy_env)
    }
    
    if (set_current) {
        eddy_env[[".CURRENT_NAME"]] <- eddy_name
    }
    
    eddy
}


#' Delete eddy and ALL its data from ALL cache layers (memory and/or disk).
#' 
#' @param eddy_name Unique name of the eddy to be deleted.
#' @param eddy_env The environment in which the eddy is put (bound).
#' 
#' @return Nothing (\code{NULL}).
#' 
#' @family eddy functions
#' 
#' @export
delete_eddy <- function(eddy_name,
                        eddy_env = default_eddy_env()) {
    
    stopifnot(rlang::is_string(eddy_name))
    stopifnot(eddy_name != ".CURRENT_NAME")
    stopifnot(is.environment(eddy_env))
    if (!base::exists(eddy_name, where = eddy_env, inherits = FALSE)) {
        rlang::abort(paste("Cannot find eddy with name: ", eddy_name))
    }
    
    eddy <- eddy_env[[eddy_name]]
    eddy$terminate()
    rm(list = eddy_name, envir = eddy_env, inherits = FALSE)
    
    # update the .CURRENT_NAME, if needed
    if (identical(eddy_env[[".CURRENT_NAME"]], eddy_name)) {
        eddy_env[[".CURRENT_NAME"]] <- NA_character_
    }
    
    invisible(NULL)
}


#' Set the current eddy to be used in future flow calls.
#' 
#' @param eddy_name Unique name for the eddy to to be set as current.
#' @param eddy_env The environment in which the eddy is put (bound).
#' 
#' @return An eddy object to be used for storing flows.
#' 
#' @family eddy functions
#' 
#' @export
set_current_eddy <- function(eddy_name,
                             eddy_env = default_eddy_env()) {
    
    stopifnot(rlang::is_string(eddy_name))
    stopifnot(eddy_name != ".CURRENT_NAME")
    stopifnot(is.environment(eddy_env))
    if (!base::exists(eddy_name, where = eddy_env, inherits = FALSE)) {
        rlang::abort(paste("Cannot find eddy with name: ", eddy_name))
    }
    
    eddy_env[[".CURRENT_NAME"]] <- eddy_name
    
    eddy <- eddy_env[[eddy_name]]
    invisible(eddy)
}    


#' Get the current eddy for a given (or default) environment.
#' 
#' If the current eddy was not previously set with \code{set_current_eddy},
#'   it creates a new eddy that uses \code{default_cache()}.
#' 
#' @param eddy_env The environment in which the eddy is put (bound).
#' 
#' @return An eddy object to be used for storing flows.
#' 
#' @family eddy functions
#' 
#' @export
get_current_eddy <- function(eddy_env = default_eddy_env()) {
    
    stopifnot(is.environment(eddy_env))
    
    eddy_name <- eddy_env[[".CURRENT_NAME"]]
    if (is.null(eddy_name) ||
        is.na(eddy_name) ||
        !base::exists(eddy_name, where = eddy_env, inherits = FALSE)
    ) {
        # no current name, create the default eddy
        eddy_name <- "default_eddy"
        eddy <- new_eddy(
            eddy_name,
            cache = default_cache(),
            eddy_env = eddy_env)
        eddy_env[[".CURRENT_NAME"]] <- eddy_name
    } else {
        eddy <- eddy_env[[eddy_name]]
        stopifnot(inherits(eddy, "R6Eddy"))
    }
    
    eddy
}


#' flow options used to initialize or update an eddy.
#' 
#' @details
#' If used in \code{set_flow_options}, these options will be stored in 
#'   \code{eddy} and retrieved by each flow subsequently executed. E.g. if not
#'   careful, it is possible to force all following flows to use a custom 
#'   \code{split_fn} function, which is not recommended.
#' 
#' @param excluded_arg A vector of argument names to be excluded when computing
#'   the input hash. Best used to exclude certain arguments that depend on 
#'   the running state, e.g. a Shiny session, a parallel cluster, etc. 
#'   Excluded arguments must have a default value to permit lazy computations.
#'   The default is not to exclude any arguments from the input hash.
#' @param eval_arg_fn Custom function to parse the input arguments and create
#'   a list of evaluated arguments to be hashed. This function should have the
#'   exact same arguments as the original function. Try to use 
#'   \code{excluded_arg} or flow source before creating a custom function. 
#'   Because each custom function is flow specific, it is not possible to set 
#'   this option at the eddy level using \code{set_flow_options}.
#' @param split_bare_list If the function output is a bare list 
#'   (\code{\link[rlang:bare-type-predicates]{rlang::is_bare_list}}), determines
#'   whether to calculate the hash of each list element and create
#'   corresponding flow elements.
#' @param split_dataframe If the function output is a data.frame or tibble,
#'   determines whether to calculate the hash of each column and create
#'   corresponding flow elements.
#' @param split_fn Custom function to generate a list of elements from the
#'   output of the flow-ed function. Useful only if the output is not a list
#'   but a flow elements are still desired. Consider returning a list
#'   as output before using this option.  If an \code{split_fn}
#'   is provided, \code{split_bare_list} and \code{split_dataframe} will be
#'   ignored.
#' @param eddy Eddy to apply / retrieve options to / from.
#' 
#' @name flow_options
NULL


parse_flow_options <- function(excluded_arg,
                               eval_arg_fn = NULL,
                               split_bare_list,
                               split_dataframe,
                               split_fn,
                               eddy = NULL
) {
    allow_null <- !is.null(eddy)
    stopifnot(
        (allow_null && is.null(excluded_arg)) || 
        is.character(excluded_arg))
    stopifnot(
        is.null(eval_arg_fn) || 
        is_not_flow_fn(eval_arg_fn))
    stopifnot(
        (allow_null && is.null(split_bare_list)) || 
        rlang::is_true(split_bare_list) || rlang::is_false(split_bare_list))
    stopifnot(
        (allow_null && is.null(split_dataframe)) || 
        rlang::is_true(split_dataframe) || rlang::is_false(split_dataframe))
    stopifnot(
        is.null(split_fn) || 
        is_not_flow_fn(split_fn))
    stopifnot(is.null(eddy) || inherits(eddy, "R6Eddy"))
    
    if (is.null(eddy)) {
        rfo <- list()
    } else {
        rfo <- eddy$flow_options
    }
    
    # recreating the list gets around NULL values
    list(
        excluded_arg = excluded_arg %||% rfo$excluded_arg,
        eval_arg_fn = eval_arg_fn,
        split_bare_list = split_bare_list %||% rfo$split_bare_list,
        split_dataframe = split_dataframe %||% rfo$split_dataframe,
        split_fn = split_fn %||% rfo$split_fn
    )
}


#' default flow options used to initialize or update an eddy.
#' 
#' @details  
#' Sets a list of default options.
#' @return For \code{default_flow_options}, a list of options.
#' 
#' @rdname flow_options
#' 
#' @export
default_flow_options <- function(excluded_arg = character(),
                                 split_bare_list = TRUE,
                                 split_dataframe = FALSE,
                                 split_fn = NULL
) {
    .args <- mget(names(formals()), sys.frame(sys.nframe()))
    .args["eval_arg_fn"] <- list(NULL)
    rfo <- do.call(parse_flow_options, .args)
    
    rfo
}


#' @details 
#' \code{set_flow_options} does not overwrite the current options when the 
#'   argument is \code{NULL}.
#' 
#' @return For \code{set_flow_options}, \code{NULL}.
#' 
#' @rdname flow_options
#' 
#' @export
set_flow_options <- function(excluded_arg = NULL,
                             split_bare_list = NULL,
                             split_dataframe = NULL,
                             split_fn = NULL,
                             eddy = get_current_eddy()
) {
    .args <- mget(names(formals()), sys.frame(sys.nframe()))
    .args["eval_arg_fn"] <- list(NULL)
    rfo <- do.call(parse_flow_options, .args)
    eddy$flow_options <- rfo
    
    invisible(NULL)
}


#' @return For \code{get_flow_options} and \code{gfo}, a list of options 
#' including the eddy. \code{gfo} is just an alias of \code{get_flow_options}.
#' 
#' @rdname flow_options
#' 
#' @export
get_flow_options <- function(excluded_arg = NULL,
                             eval_arg_fn = NULL,
                             split_bare_list = NULL,
                             split_dataframe = NULL,
                             split_fn = NULL,
                             eddy = get_current_eddy()
) {
    .args <- mget(names(formals()), sys.frame(sys.nframe()))
    rfo <- do.call(parse_flow_options, .args)
    rfo$eddy <- eddy
    
    rfo
}


#' @rdname flow_options
#' 
#' @export
gfo <- get_flow_options
