# row-wise caching of operations on data frame


# !diagnostics suppress=., self, private, super
ROW_HASH <- "..row_hash.."


# R6FlowDfr ----
R6FlowDfr <- R6::R6Class(
    classname = "R6FlowDfr",
    inherit = R6Flow,
    public = list(
        out_df = NULL,
        out_visible = NULL
    )
)


# compute ----
R6FlowDfr$set("public", "compute", function() {
    # follow R6Flow compute, with some changes
    
    if (self$is_valid) return(TRUE)
    if (!self$is_current) return(FALSE)
    state <- self$get_state()
    
    if (!base::exists(
        state$in_hash, where = self$state_env, inherits = FALSE)
    ) {
        return(FALSE)
    }
    elem_args <- self$state_env[[state$in_hash]]
    data_args <-
        elem_args %>%
        purrr::map_if(
            .p = ~ inherits(., "Element"),
            .f = function(x) {
                x$self$collect(x$elem_name)
            }
        )
    
    # df gets a new column, a hash for each row
    df <- data_args[[1L]]
    stopifnot(is.data.frame(df))
    stopifnot(ncol(df) > 0L)
    row_hash <- purrr::pmap_chr(df, ~ self$eddy$digest(list(...)))
    df[[ROW_HASH]] <- row_hash
    
    # rows that are not in cache
    first_time <- is.null(self$out_df)
    if (first_time) {
        changed_idx <- seq_nrow(df)
    } else {
        changed_idx <- which(!(row_hash %in% self$out_df[[ROW_HASH]]))
    }
    
    if (first_time || length(changed_idx) > 0L) {
        # first time might be an empty df, we still need to eval
        cdf <- df[changed_idx, , drop = FALSE]
        data_args[[1L]] <- cdf
        out_data <- withVisible(do.call(
            what = self$fn,
            args = data_args,
            envir = globalenv()
        ))
        out_df <- out_data$value
        stopifnot(is.data.frame(out_df))
        stopifnot(all(ROW_HASH %in% names(out_df)))
        stopifnot(all(out_df[[ROW_HASH]] %in% cdf[[ROW_HASH]]))
        if (first_time) {
            self$out_visible <- out_data$visible
        } else {
            stopifnot(identical(names(out_df), names(self$out_df)))
        }
        # save to row cache & add rows from cache
        self$out_df <- dplyr::bind_rows(self$out_df, tibble::as_tibble(out_df))
    }
    
    # reconstruct out_data from cache
    # there might be deleted rows returned by $fn
    row_idx <- na.omit(match(row_hash, self$out_df[[ROW_HASH]]))
    out_df <- self$out_df[row_idx, , drop = FALSE]
    # clean up
    out_df[[ROW_HASH]] <- NULL
    out_data <- list(
        value = out_df,
        visible = self$out_visible
    )
    out_hash <- self$eddy$digest(out_data)
    
    # update the current state
    update_OK <- self$update_state(
        index = self$state_index, 
        in_hash = state$in_hash, 
        out_hash = out_hash,
        out_data = out_data
    )
    if (!update_OK) return(FALSE)
    
    # split into elements by function
    split_using_fn <- !is.null(self$split_fn)
    split_bare_list <- 
        self$split_bare_list && rlang::is_bare_list(out_data$value)
    split_dataframe <- self$split_dataframe && is.data.frame(out_data$value)
    if (split_using_fn || split_bare_list || split_dataframe) {
        abort_split <- FALSE
        if (split_using_fn) {
            out_lst <- self$split_fn(out_data$value)
            if (!rlang::is_dictionaryish(out_lst)) {
                rlang::warn(paste(
                    "Cannot create flow elements,",
                    "`split_fn` must return a list with unique names."))
                abort_split <- TRUE
            }
        } else if (split_bare_list) {
            out_lst <- out_data$value
            if (!rlang::is_dictionaryish(out_lst)) {
                rlang::inform(paste(
                    "Cannot create flow elements,",
                    "the returned list must have unique names."))
                abort_split <- TRUE
            }
        } else {
            out_lst <- as.list(out_data$value)
            if (!rlang::is_dictionaryish(out_lst)) {
                rlang::inform(paste(
                    "Cannot create flow elements,",
                    "the returned data frame must have unique names."))
                abort_split <- TRUE
            }
        }
        if (!abort_split) {
            for (elem_name in names(out_lst)) {
                # reconstruct the withVisible list for each element
                vis_elem_lst <- list(
                    value = out_lst[[elem_name]],
                    visible = out_data$visible
                )
                elem_hash <- self$eddy$digest(vis_elem_lst)
                self$add_state_output(
                    out_hash, elem_name, elem_hash, vis_elem_lst)
            }
        }
    }
    
    self$save()
}, overwrite = TRUE)


#' Row-wise caching of operations on data frame.
#' 
#' @details 
#'   Function \code{fn} will receive only the rows changed; 
#'   it may drop some of the rows, but will not add any new rows.
#'   The function \code{fn} may return fewer or more columns or modify 
#'   existing columns as long it always returns the same columns (data types 
#'   and names). The data frame \code{df} passed to \code{fn} will have one 
#'   additional column \code{..row_hash..} which must be returned as is in 
#'   order to identify changes.
#' 
#' @param df A \code{data.frame} or \code{tibble}. Rownames are not supported.
#' @param ... Other named arguments to pass to \code{fn}.
#' @param fn The function to apply to the data frame. It must accept a data
#'   frame as the first argument.
#' @param fn_id Optional id to uniquely identify the function. By default,
#'   rflow functions reuse the cache if the same function is given. The id 
#'   allows the user to suppress console messages and to explicitly
#'   indicate whether to reuse the old cache or create a new one.
#' @param flow_options List of options created using \code{get_flow_options}.
#' 
#' @return The flow object.
#' 
#' @export
flow_dfr <- function(df, 
                     ..., 
                     fn = NULL,
                     fn_id = NULL,
                     flow_options = get_flow_options()
) {
    # same as make_flow_fn, but using R6FlowDfr (not DRY)
    
    if (is.data.frame(df)) {
        stopifnot(ncol(df) > 0L)
    } else {
        stopifnot(inherits(df, "R6Flow") || inherits(df, "Element"))
    }
    stopifnot(is_not_flow_fn(fn))
    if (any(grepl("\\.Primitive", format(fn)))) {
        rlang::abort("Primitive functions not supported.")
    }
    stopifnot(
        is.null(fn_id) || (!rlang::is_na(fn_id) && (
        rlang::is_string(fn_id) || rlang::is_scalar_integerish(fn_id)))
    )
    if (rlang::is_scalar_integerish(fn_id)) fn_id <- as.integer(fn_id)
    
    # when generated by get_flow_options(), flow_options are valid
    if (!is.null(flow_options$eval_arg_fn)) {
        fn_formals <- formals(args(fn))
        eval_arg_fn_formals <- formals(args(flow_options$eval_arg_fn))
        stopifnot(identical(fn_formals, eval_arg_fn_formals))
    }
    eddy <- flow_options$eddy
    
    # best place to capture the name of the function
    # fn_name (the binding) is irrelevant (it's the args and body that matter)
    # but it may be useful for debugging
    match_call <- match.call()
    if (is.symbol(match_call$fn)) {
        fn_name <- as.character(match_call$fn)
    } else {
        rlang::abort("Anonymous functions not supported.")
    }
    
    fn_key <- make_fn_key(fn, fn_id, flow_options, "R6FlowDfr")
    fn_names <-
        eddy$flow_lst %>%
        purrr::keep(~ rlang::inherits_all(., "R6FlowDfr")) %>%
        purrr::map_chr("fn_name")
    if (fn_name %in% fn_names) {
        if (eddy$has_flow(fn_key)) {
            # the R6FlowDfr obj exists ==> re-use it; message if no fn_id
            if (is.null(fn_id)) {
                rlang::inform(paste("Reusing cache for function:", fn_name))
            }
            flow <- eddy$get_flow(fn_key)
        } else {
            # obj does not exist but same fn_name ==> new; message if no fn_id
            if (is.null(fn_id)) {
                rlang::inform(paste(
                    "Function", fn_name, "exists with different body/options,",
                    "creating a new cache."))
                fn_ids <- eddy$flow_lst %>%
                    purrr::keep(~ .$fn_name == fn_name) %>%
                    purrr::keep(~ rlang::is_integerish(.$fn_id)) %>%
                    purrr::map_int("fn_id")
                if (length(fn_ids) > 0L) {
                    fn_id <- as.integer(max(fn_ids) + 1)
                } else {
                    fn_id <- 1L
                }
                fn_key <- make_fn_key(fn, fn_id, flow_options, "R6FlowDfr")
            }
            flow <- R6FlowDfr$new(
                fn = fn,
                fn_key = fn_key,
                fn_name = fn_name,
                fn_id = fn_id %||% 1L,
                flow_options = flow_options
            )
        }
    } else {
        if (eddy$has_flow(fn_key)) {
            rlang::inform(paste(
                "A cache for a function with the same signature but a", 
                "different name already exists, creating a new cache."))
            while (eddy$has_flow(fn_key)) {
                old_fn_id <- eddy$get_flow(fn_key)$fn_id
                if (is.numeric(old_fn_id)) {
                    fn_id <- as.integer(old_fn_id + 1)
                } else {
                    fn_id <- (fn_id %||% 1L) + 1L
                }
                fn_key <- make_fn_key(fn, fn_id, flow_options, "R6FlowDfr")
            }
        }
        flow <- R6FlowDfr$new(
            fn = fn,
            fn_key = fn_key,
            fn_name = fn_name,
            fn_id = fn_id %||% 1L,
            flow_options = flow_options
        )
    }
    
    do.call(
        what = flow$rf_fn, 
        args = c(list(df), list(...)),
        envir = parent.frame(n = 2)
    )
}
