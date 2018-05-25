# R6Eddy class and methods


# !diagnostics suppress=., self, private


# R6Eddy ----
R6Eddy <- R6::R6Class(
    classname = "R6Eddy",
    public = list(
        # data
        cache = NULL,
        rflow_lst = NULL,
        rflow_options = NULL,
        # attributes
        is_reactive = NULL,
        algo = NULL,
        # init
        initialize = function(cache,
                              is_reactive = FALSE,
                              algo = "xxhash64") {},
        # rflow
        has_cache = function(fn_key) {},
        has_rflow = function(fn_key) {},
        require_rflow = function(fn_key) {},
        get_rflow = function(fn_key) {},
        add_rflow = function(fn_key) {},
        remove_rflow = function(fn_key) {},
        forget_rflow = function(fn_key) {},
        delete_rflow = function(fn_key) {},
        # key / data
        list_keys = function(group) {},
        has_key = function(fn_key, key) {},
        get_data = function(fn_key, key) {},
        add_data = function(fn_key, key, value) {},
        delete_data = function(fn_key, key) {},
        # misc
        print = function() {},
        reset = function() {},
        terminate = function() {},
        digest = function(object, is_file_path =  FALSE) {},
        digest_each = function(objects, is_file_path =  FALSE) {}
    )
)


# initialize ----
R6Eddy$set("public", "initialize", function(
        cache,
        rflow_options = default_rflow_options(),
        is_reactive = FALSE,
        algo = "xxhash64"
) {
    stopifnot(inherits(cache, "R6Cache"))
    if (isTRUE(is_reactive))
        stop("reactive eddies not yet implemented")
    
    self$cache <- cache
    self$rflow_lst <- list()
    self$rflow_options <- rflow_options
    self$is_reactive <- FALSE
    self$algo <- algo
    
    invisible(NULL)
}, overwrite = TRUE)


# has_cache ----
R6Eddy$set("public", "has_cache", function(fn_key) {
    
    self$cache$has_group(fn_key)
}, overwrite = TRUE)


# has_rflow ----
R6Eddy$set("public", "has_rflow", function(fn_key) {
    
    fn_key %in% names(self$rflow_lst)
}, overwrite = TRUE)


# require_rflow ----
R6Eddy$set("public", "require_rflow", function(fn_key) {
    
    if (!self$has_rflow(fn_key)) {
        stop("rflow not found for fn_key: ", fn_key)
    }
    if (!self$cache$has_group(fn_key)) {
        stop("cache group not found for fn_key: ", fn_key)
    }
    
    invisible(NULL)
}, overwrite = TRUE)


# get_rflow ----
R6Eddy$set("public", "get_rflow", function(fn_key) {
    
    self$require_rflow(fn_key)
    
    self$rflow_lst[[fn_key]]
}, overwrite = TRUE)


# add_rflow ----
R6Eddy$set("public", "add_rflow", function(fn_key, rflow) {
    
    if (self$has_rflow(fn_key)) {
        # we cannot return the rflow already present since it may have
        # with different options ==> all eddy$*_rflow functions are strict
        stop("rflow already exists for key: ", fn_key)
    } else {
        self$rflow_lst[[fn_key]] <- rflow
        self$cache$add_group(fn_key)
        # TODO: reactive: update adjacency matrix
    }
    
    self$has_rflow(fn_key) && self$cache$has_group(fn_key)
}, overwrite = TRUE)


# remove_rflow ----
R6Eddy$set("public", "delete_rflow", function(fn_key) {
    
    self$require_rflow(fn_key)
    
    # do not delete the cache, just remove the R6Flow obj
    self$rflow_lst[[fn_key]] <- NULL
    # TODO: reactive: update adjacency matrix
    
    !self$has_rflow(fn_key)
}, overwrite = TRUE)


# forget_rflow ----
R6Eddy$set("public", "forget_rflow", function(fn_key) {
    
    self$require_rflow(fn_key)
    
    self$cache$forget_group(fn_key)
    # empty the cache without deleting the group, keep the R6Flow obj
    
    self$has_rflow(fn_key) && length(self$cache$list_keys(fn_key)) == 0L
}, overwrite = TRUE)


# delete_rflow ----
R6Eddy$set("public", "delete_rflow", function(fn_key) {
    
    self$require_rflow(fn_key)
    
    self$cache$delete_group(fn_key)
    self$rflow_lst[[fn_key]] <- NULL
    # TODO: reactive: update adjacency matrix
    
    !self$has_rflow(fn_key) && !self$cache$has_group(fn_key)
}, overwrite = TRUE)


# list_keys ----
R6Eddy$set("public", "list_keys", function(fn_key) {
    
    self$require_rflow(fn_key)
    
    self$cache$list_keys(fn_key)
}, overwrite = TRUE)


# has_key ----
R6Eddy$set("public", "has_key", function(fn_key, key) {
    
    self$require_rflow(fn_key)
    
    self$cache$has_key(fn_key, key)
}, overwrite = TRUE)


# get_data ----
R6Eddy$set("public", "get_data", function(fn_key, key) {
    
    self$require_rflow(fn_key)
    
    self$cache$get_data(fn_key, key)
}, overwrite = TRUE)


# add_data ----
R6Eddy$set("public", "add_data", function(fn_key, key, value) {
    
    self$require_rflow(fn_key)
    
    self$cache$add_data(fn_key, key, value)
    
    self$cache$has_key(fn_key, key)
}, overwrite = TRUE)


# delete_data ----
R6Eddy$set("public", "delete_data", function(fn_key, key) {
    
    self$require_rflow(fn_key)
    
    self$cache$delete_data(fn_key, key)
    
    !self$cache$has_key(fn_key, key)
}, overwrite = TRUE)


# print ----
# nocov start
R6Eddy$set("public", "print", function() {
    
    cache_df <- self$cache$summary()
    df <- tibble::tibble(
        fn_name = purrr::map_chr(self$rflow_lst, "fn_name"),
        fn_key = as.character(names(self$rflow_lst)),
        class = purrr::map_chr(self$rflow_lst, ~ class(.)[[1L]]),
        n_states = purrr::map_int(self$rflow_lst, ~ NROW(.$state))
    ) %>%
        dplyr::left_join(cache_df, by = "fn_key")
    
    rfo <- self$rflow_options
    excluded_arg <- paste(rfo$excluded_arg, collapse = ", ")
    source_file_arg <- paste(rfo$source_file_arg, collapse = ", ")
    eval_arg_fn <- format(args(rfo$eval_arg_fn))[[1]]
    split_fn <- format(args(rfo$split_fn))[[1]]
    
    emph_obj1 <- paste0("<", crayon::italic(class(self)[[1L]]), ">")
    emph_obj2 <- paste0("<", crayon::italic(class(self$cache)[[1L]]), ">")
    n_rflows <- crayon::bold(length(self$rflow_lst))
    cat(emph_obj1, "with cache", emph_obj2, "and", n_rflows, "rflow(s)\n",
        " - excluded_arg:", excluded_arg, "\n",
        " - source_file_arg:", source_file_arg, "\n",
        " - eval_arg_fn:", eval_arg_fn, "\n",
        " - split_bare_list:", rfo$split_bare_list, "\n",
        " - split_dataframe:", rfo$split_dataframe, "\n",
        " - split_fn:", split_fn, "\n"
    )
    print(df)
    
    invisible(self)
}, overwrite = TRUE)
# nocov end


# reset ----
R6Eddy$set("public", "reset", function() {
    # brings eddy in the same state as just after $new()
    
    self$cache$reset()
    self$rflow_lst <- list()
    # do not modify self$rflow_options
    
    invisible(self)
}, overwrite = TRUE)


# terminate ----
R6Eddy$set("public", "terminate", function() {
    # reset + delete its own data structures, e.g. folders
    # object cannot be used afterwards
    
    self$cache$terminate()
    
    self$cache <- NULL
    self$rflow_lst <- NULL
    self$rflow_options <- NULL
    
    invisible(NULL)
}, overwrite = TRUE)


# digest ----
R6Eddy$set("public", "digest", function(object, is_file_path = FALSE) {
    
    digest::digest(object, file = is_file_path, algo = self$algo)
}, overwrite = TRUE)


R6Eddy$set("public", "digest_each", function(objects, is_file_path = FALSE) {
    
    purrr::map_chr(
        .x = objects, 
        .f = ~ digest::digest(., file = is_file_path, algo = self$algo)
    )
}, overwrite = TRUE)
