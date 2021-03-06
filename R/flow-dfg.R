# group-wise caching of operations on data frame


# !diagnostics suppress=., self, private, super


# R6FlowDfg ----
R6FlowDfg <- R6::R6Class(
    classname = "R6FlowDfg",
    inherit = R6FlowDfr,
    public = list(
        group_by = NULL
    )
)


# initialize ----
R6FlowDfg$set("public", "initialize", function(
    fn,
    fn_key,
    fn_name,
    fn_id,
    group_by,
    flow_options = get_flow_options()
) {
    super$initialize(fn, fn_key, fn_name, fn_id, flow_options)
    
    # after registering into eddy, remove itself if error
    tryCatch({
        self$group_by <- group_by
    }, error = function(e) {
        self$eddy$remove_flow(fn_key)
        stop(e)
    })
    
    invisible(NULL)
}, overwrite = TRUE)


# compute ----
R6FlowDfg$set("public", "compute", function() {
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
    
    # re-group (needed to create group hash)
    df <- data_args[[1L]]
    stopifnot(is.data.frame(df))
    stopifnot(ncol(df) > 0L)
    if (is.null(self$group_by)) {
        if (!dplyr::is_grouped_df(df)) {
            rlang::abort("Ungrouped data frame and NULL `group_by`.")
        }
        gdf <- df
    } else {
        gdf <- dplyr::group_by_at(df, .vars = self$group_by)
    }
    
    # df gets a new column, a hash for each row
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
    gdf[[ROW_HASH]] <- row_hash
    
    # df gets a new column, a hash for each group
    group_hash <- 
        gdf %>%
        tibble::rowid_to_column(., var = .ROW_ID) %>%
        dplyr::do(data.frame(
            row_id = .[[.ROW_ID]],
            # sort the row hashes to guarantee the same group hash
            group_hash = self$eddy$digest(sort(.[[ROW_HASH]])), 
            stringsAsFactors = FALSE
        )) %>%
        dplyr::ungroup() %>%
        dplyr::arrange_at(.vars = "row_id") %>%
        dplyr::pull(group_hash)
    df[[GROUP_HASH]] <- group_hash
    
    # rows that are not in cache
    first_time <- is.null(self$out_df)
    if (first_time) {
        changed_idx <- seq_nrow(df)
    } else {
        changed_idx <- which(!(group_hash %in% self$out_df[[GROUP_HASH]]))
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
        stopifnot(all(c(ROW_HASH, GROUP_HASH) %in% names(out_df)))
        # both groups and rows might be missing, but nothing new is allowed
        stopifnot(all(out_df[[GROUP_HASH]] %in% cdf[[GROUP_HASH]]))
        stopifnot(all(out_df[[ROW_HASH]] %in% cdf[[ROW_HASH]]))
        if (first_time) {
            self$out_visible <- out_data$visible
        } else {
            # check both name and col types (e.g. int vs num)
            old_header <- purrr::map_chr(self$out_df, ~ class(.)[1])
            new_header <- purrr::map_chr(out_df, ~ class(.)[1])
            stopifnot(identical(old_header, new_header))
            # update factor cols
            for (j in seq_ncol(out_df)) {
                f_new <- out_df[[j]]
                f_old <- self$out_df[[j]]
                if (is.factor(f_new) && 
                    !identical(levels(f_new), levels(f_old))
                ) {
                    f_lst <- forcats::fct_unify(
                        list(out_df[[j]], self$out_df[[j]]))
                    out_df[[j]] <- f_lst[[1]]
                    self$out_df[[j]] <- f_lst[[2]]
                }
            }
        }
        # save to row cache & add rows from cache
        self$out_df <- dplyr::bind_rows(self$out_df, out_df)
    }
    
    # reconstruct out_data from cache
    # first we match by group hash then by row hash (neither are unique)
    group_idx <- which(self$out_df[[GROUP_HASH]] %in% group_hash)
    match_df <- self$out_df[group_idx, , drop = FALSE]
    # there might be deleted rows returned by $fn
    row_idx <- na.omit(match(row_hash, match_df[[ROW_HASH]]))
    out_df <- match_df[row_idx, , drop = FALSE]
    # clean up
    out_df[[ROW_HASH]] <- NULL
    out_df[[GROUP_HASH]] <- NULL
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


#' Group-wise caching of operations on data frame
#' 
#' @details 
#'   Function \code{fn} will receive only the rows and groups changed; 
#'   it may drop some of the rows, but will not add any new rows.
#'   The function \code{fn} may return fewer or more columns or modify 
#'   existing columns as long it always returns a consistent schema
#'   (i.e., the same column data types and names) for all calls. 
#'   The data frame \code{df} passed to \code{fn} will include two
#'   additional columns: \code{..row_hash..} and \code{..group_hash..} that
#'   must be returned as is in order to identify changes.
#'   
#'   Arguments \code{fn}, \code{fn_id} and \code{flow_options}, when provided,
#'   must be named. Argument \code{fn} must be always provided.
#' 
#' @param ... Named arguments to pass to \code{fn}. The first argument must be 
#'   a \code{data.frame} or \code{tibble}. Row names are not supported.
#'   If no \code{group_by} values are provided, the data frame must be grouped.
#' @param fn The function to apply to the data frame. It must accept a data
#'   frame as the first argument. \code{fn} may also apply \code{group_by}
#'    operations if the data frame given as input is not already grouped.
#' @param fn_id Optional id to uniquely identify the function. By default,
#'   rflow functions reuse the cache if the same function is given. The id 
#'   allows the user to suppress console messages and to explicitly
#'   indicate whether to reuse the old cache or create a new one.
#' @param group_by A character vector of column names. If provided, groups
#'   already present will be ignored.
#' @param flow_options List of options created using \code{get_flow_options}.
#' 
#' @return The flow object.
#' 
#' @examples 
#' dfg_fn <- function(df) {
#'     df <- df %>%
#'         dplyr::mutate(Sepal.Length = Sepal.Length * 2)
#' }
#' 
#' dfg_fn2 <- function(df) {
#'     df <- df %>%
#'         dplyr::mutate(Petal.Length = Petal.Length * 3)
#' }
#' 
#' iris <- iris %>%
#'     dplyr::group_by(Species)
#' dfg_flow <- flow_dfg(iris, fn = dfg_fn)
#' collected_dfg <- dfg_flow %>% collect()
#' 
#' # when a change in group is made, the flow object updates its state
#' iris[1, "Species"] <- "virginica"
#' dfg_flow <- flow_dfg(iris, fn = dfg_fn)
#' collected_dfg <- dfg_flow %>% collect()
#' 
#' # the flow element can also become input for another flow_dfg function 
#' # in order to allow multiple, chained computations
#' collected_dfg2 <- dfg_flow %>%
#'    flow_dfg(fn = dfg_fn2, group_by = "Species") %>%
#'    collect()

#' @export
flow_dfg <- function(..., 
                     fn = NULL,
                     fn_id = NULL,
                     group_by = NULL,
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
    if (!is.null(group_by)) {
        stopifnot(!is.na(group_by) && is.character(group_by))
        if (is.data.frame(df)) {
            stopifnot(all(group_by %in% names(df)))
        }
    }
    # add `group_by` to flow_options to make the hash unique
    flow_options$group_by <- group_by
    
    stopifnot(!is.null(fn))
    match_call <- match.call()
    use <- make_key(match_call$fn, fn, fn_id, flow_options, "R6FlowDfg")
    eddy <- flow_options$eddy
    
    if (use$action == "get") {
        flow <- eddy$get_flow(use$fn_key)
    } else {
        flow <- R6FlowDfg$new(
            fn = fn,
            fn_key = use$fn_key,
            fn_name = use$fn_name,
            fn_id = use$fn_id,
            group_by = group_by,
            flow_options = flow_options
        )
    }
    
    do.call(
        what = flow$rf_fn, 
        args = dots,
        envir = parent.frame(n = 2)
    )
}
