# functions to indicate data sources (other than R variables)


# !diagnostics suppress=., self, private, super


# R6FileSource ----
R6FileSource <- R6::R6Class(
    classname = "R6FileSource",
    inherit = R6Flow
)


# calc_in_hash ----
R6FileSource$set("public", "calc_in_hash_file_source", function(
        rf_env = parent.frame()
) {
    file_path <- rf_env$elem_args[[1L]]
    in_hashes <- self$eddy$digest_each(objects = file_path, is_file_path = TRUE)
    in_hash <- self$eddy$digest(in_hashes)
    
    in_hash
}, overwrite = TRUE)


# initialize ----
R6FileSource$set("public", "initialize", function(
    fn,
    fn_key,
    fn_name,
    fn_id,
    flow_options = get_flow_options()
) {
    super$initialize(fn, fn_key, fn_name, fn_id, flow_options)
    
    # calc_in_hash
    self$calc_in_hash <- self$calc_in_hash_file_source
    
    invisible(NULL)
}, overwrite = TRUE)


#' Creates a flow object that watches one or more files.
#' 
#' @param file_path A (named) vector of file paths to be watched.
#' @param flow_options List of options created using \code{get_flow_options}.
#'   All options except \code{eddy} are ignored.
#' 
#' @return The flow object.
#' 
#' @export
flow_file_source <- function(file_path, 
                             flow_options = get_flow_options()) {
    
    # validate file_path 
    stopifnot(is.character(file_path))
    nms <- names(file_path)
    if (is.null(nms)) {
        names(file_path) <- file_path
    } else {
        nms[nms == ""] <- file_path[nms == ""]
        names(file_path) <- nms
    }
    lst <- as.list(file_path)
    if (!rlang::is_dictionaryish(lst)) {
        rlang::abort(paste(
            "Invalid/Duplicate names detected;", 
            "please provide a vector with unique names."))
    }
    
    flow_options$excluded_arg <- character()
    flow_options$split_bare_list <- TRUE
    flow_options$split_dataframe <- FALSE
    flow_options$split_fn <- as.list
    eddy <- flow_options$eddy
    
    fn_name <- file_path[1L]
    if (nchar(fn_name) > 20) {
        fn_name <- substr(fn_name, nchar(fn_name) - 17 + 1, nchar(fn_name))
        fn_name <- paste0("...", fn_name)
    }
    
    fn_key <- eddy$digest(file_path)
    
    fn_ids <- eddy$flow_lst %>%
        purrr::keep(~ .$fn_name == fn_name) %>%
        purrr::keep(~ rlang::is_integerish(.$fn_id)) %>%
        purrr::map_int("fn_id")
    if (length(fn_ids) > 0L) {
        fn_id <- as.integer(max(fn_ids) + 1)
    } else {
        fn_id <- 1L
    }
    
    if (eddy$has_flow(fn_key)) {
        flow <- eddy$get_flow(fn_key)
    } else {
        flow <- R6FileSource$new(
            fn = identity,
            fn_key = fn_key,
            fn_name = fn_name,
            fn_id = fn_id,
            flow_options = flow_options
        )
    }
    
    # eval the call
    res <- do.call(flow$rf_fn, list(x = file_path), envir = parent.frame(n = 2))
    
    res
}
