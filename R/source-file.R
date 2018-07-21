# functions to create file data sources (other than R variables)


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
    
    # after registering into eddy, remove itself if error
    tryCatch({
        # calc_in_hash
        self$calc_in_hash <- self$calc_in_hash_file_source
    }, error = function(e) {
        self$eddy$remove_flow(fn_key)
        stop(e)
    })
    
    invisible(NULL)
}, overwrite = TRUE)


#' Creates a flow object that watches one or more files
#' 
#' @details 
#'   This flow object does not throw an error if the \code{file_path} is missing, 
#'   but it changes its state. Hence, it can be used to trigger a downstream
#'   flow object if the file is now present, changed or missing.
#'
#' @param file_path A (named) vector of file paths to be watched or a
#'  \code{fs_path} object.
#' @param flow_options List of options created using \code{get_flow_options}.
#'   All options except \code{eddy} are ignored.
#' 
#' @return The flow object.
#' 
#' @examples 
#' # write for the first time content in file and create flow
#' file_temp <- tempfile(pattern = "example_source")
#' write.csv(head(mtcars), file_temp, row.names = FALSE)
#' rflow_source <- flow_file_source(file_temp)
#' 
#' # write other content in the same file
#' # now the flow object will update its state
#' write.csv(tail(mtcars), file_temp, row.names = FALSE)
#' rflow_source <- flow_file_source(file_temp)
#' unlink(file_temp)
#' 
#' @export
flow_file_source <- function(file_path, 
                             flow_options = get_flow_options()) {
    
    # validate file_path 
    stopifnot(is.character(file_path))
    stopifnot(length(file_path) >= 1L)
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
    flow_options$eval_arg_fn <- NULL
    flow_options$split_bare_list <- TRUE
    flow_options$split_dataframe <- FALSE
    flow_options$split_fn <- as.list
    eddy <- flow_options$eddy
    
    # fn_name based on file_path
    fn_name <- if (length(file_path) > 1L) {
        paste0(file_path[1L], " [+", length(file_path) - 1L, "]")
    } else {
        file_path
    }
    nchar_max <- 30L
    if (nchar(fn_name) > nchar_max) {
        nchar_side <- nchar_max %/% 2L - 2L
        n1 <- substr(fn_name, 1L, nchar_side)
        n2 <- substr(fn_name, nchar(fn_name) - nchar_side, nchar(fn_name))
        fn_name <- paste0(n1, "...", n2)
    }
    
    # fn_id is always the same for each path set, cannot be set by user
    fn_id <- 1L
    
    # cannot use make_key(), fn_key depends only on path string
    fn_key <- eddy$digest(file_path)
    
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
    do.call(
        what = flow$rf_fn, 
        args = list(x = file_path), 
        envir = parent.frame(n = 2)
    )
}
