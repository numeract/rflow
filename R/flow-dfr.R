# row-wise caching of operations on data frame


# !diagnostics suppress=., self, private, super


# R6FlowDfr ----
R6FlowDfr <- R6::R6Class(
    classname = "R6FlowDfr",
    inherit = R6Flow,
    public = list(
        out_df = NULL,
        out_visible = NULL
    )
)


# initialize ----
R6FlowDfr$set("public", "initialize", function(
    fn,
    fn_key,
    fn_name,
    fn_id,
    flow_options = get_flow_options()
) {
    super$initialize(fn, fn_key, fn_name, fn_id, flow_options)
    
    # after registering into eddy, remove itself if error
    tryCatch({
        if (self$eddy$cache$has_key(fn_key, .ROW_CACHE)) {
            flow_data <- self$eddy$cache$get_data(fn_key, .ROW_CACHE)
            self$out_df <- flow_data$out_df
            self$out_visible <- flow_data$out_visible
        } else {
            self$out_df <- NULL
            self$out_visible <- NULL
        }
    }, error = function(e) {
        self$eddy$remove_flow(fn_key)
        stop(e)
    })
    
    invisible(NULL)
}, overwrite = TRUE)


# forget_state ----
R6FlowDfr$set("public", "forget_state", function(index) {
    
    # in out_df one row can belong to many states, cannot separate them
    self$out_df <- NULL
    
    super$forget_state(index)
}, overwrite = TRUE)


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
    hdf <- df
    if (length(data_args) > 1L) {
        # extra column to capture change in not excluded dots
        hdf$dots_hash <- 
            data_args[-1L] %>%
            discard_at(self$excluded_arg) %>%
            self$eddy$digest()
    }
    row_hash <- purrr::pmap_chr(hdf, ~ self$eddy$digest(list(...)))
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
        out_df <- dplyr::ungroup(tibble::as_tibble(out_df))
        stopifnot(all(ROW_HASH %in% names(out_df)))
        # both groups and rows might be missing, but nothing new is allowed
        stopifnot(all(out_df[[ROW_HASH]] %in% cdf[[ROW_HASH]]))
        if (first_time) {
            self$out_visible <- out_data$visible
        } else {
            # check both name and col types (e.g. int vs num)
            old_header <- purrr::map_chr(self$out_df, ~ class(.)[1])
            new_header <- purrr::map_chr(out_df, ~ class(.)[1])
            stopifnot(identical(old_header, new_header))
        }
        # save to row cache & add rows from cache
        self$out_df <- dplyr::bind_rows(self$out_df, out_df)
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


# forget_all ----
R6FlowDfr$set("public", "forget_all", function() {
    
    self$out_df <- NULL
    self$out_visible <- NULL
    
    super$forget_all()
}, overwrite = TRUE)


# save ----
R6FlowDfr$set("public", "save", function() {
    
    super$save()
    
    flow_data <- list(
        out_df = self$out_df,
        out_visible = self$out_visible
    )
    
    # returns TRUE if cache for fn_key contains the key .ROW_CACHE
    save_ok <- self$eddy$add_data(self$fn_key, .ROW_CACHE, flow_data)
    if (!save_ok) {
        rlang::warn("flow cannot save row cache state")
    }
    save_ok
}, overwrite = TRUE)


#' Row-wise caching of operations on data frame.
#' 
#' @details 
#'   Function \code{fn} operates on a data frame received as argument.
#'   \code{fn} will receive only the rows changed; 
#'   it may drop some of the rows, but will not add any new rows.
#'   The function \code{fn} may return fewer or more columns or modify 
#'   existing columns as long it always returns a consistent schema
#'   (i.e., the same column data types and names) for all calls. 
#'   The data frame \code{df} passed to \code{fn} will include one 
#'   additional column \code{..row_hash..} that must be returned as is in 
#'   order to identify changes.
#'   
#'   Arguments \code{fn}, \code{fn_id} and \code{flow_options}, when provided,
#'   must be named. Argument \code{fn} must be always provided.
#' 
#' @param ... Named arguments to pass to \code{fn}. The first argument must be 
#'   a \code{data.frame} or \code{tibble}. Row names are not supported.
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
flow_dfr <- function(..., 
                     fn = NULL,
                     fn_id = NULL,
                     flow_options = get_flow_options()
) {
    dots <- list(...)
    stopifnot(length(dots) > 0L) 
    df <- dots[[1L]]
    if (is.data.frame(df)) {
        stopifnot(ncol(df) > 0L)
    } else {
        stopifnot(inherits(df, "R6Flow") || inherits(df, "Element"))
    }
    
    stopifnot(!is.null(fn))
    match_call <- match.call()
    use <- make_key(match_call$fn, fn, fn_id, flow_options, "R6FlowDfr")
    eddy <- flow_options$eddy
    
    if (use$action == "get") {
        flow <- eddy$get_flow(use$fn_key)
    } else {
        flow <- R6FlowDfr$new(
            fn = fn,
            fn_key = use$fn_key,
            fn_name = use$fn_name,
            fn_id = use$fn_id,
            flow_options = flow_options
        )
    }
    
    do.call(
        what = flow$rf_fn, 
        args = dots,
        envir = parent.frame(n = 2)
    )
}
