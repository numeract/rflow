# R6Eddy class and methods


# !diagnostics suppress=., self, private


# R6Eddy ----
R6Eddy <- R6::R6Class(
    classname = "R6Eddy",
    public = list(
        # data
        cache = NULL,
        rflow = NULL,
        # attributes
        is_reactive = NULL,
        algo = NULL,
        
        initialize = function(cache,
                              is_reactive = FALSE,
                              algo = "xxhash64") {},
        
        digest = function(object, is_file_path =  FALSE) {},
        digest_each = function(objects, is_file_path =  FALSE) {},
        # rflow
        has_rflow = function(fn_key) {},
        require_rflow = function(fn_key) {},
        get_rflow = function(fn_key) {},
        add_rflow = function(fn_key) {},
        delete_rflow = function(fn_key) {},
        forget_rflow = function(fn_key) {},
        # key / data
        has_key = function(fn_key, key) {},
        get_data = function(fn_key, key) {},
        add_data = function(fn_key, key, value) {},
        delete_data = function(fn_key, key) {},
        # misc
        print = function() {},
        reset = function() {},
        terminate = function() {}
    )
)


# initialize ----
R6Eddy$set("public", "initialize", function(cache,
                                            is_reactive = FALSE,
                                            algo = "xxhash64") {
    stopifnot(inherits(cache, "R6Cache"))
    if (isTRUE(is_reactive))
        stop("reactive eddies not yet implemented")
    
    self$rflow <- list()
    self$is_reactive <- FALSE
    self$algo <- algo
    
    invisible(NULL)
}, overwrite = TRUE)


# digest ----
R6Eddy$set("public", "digest", function(object, is_file_path =  FALSE) {
    
    digest::digest(object, file = is_file_path, algo = self$algo)
}, overwrite = TRUE)


R6Eddy$set("public", "digest_each", function(objects, is_file_path =  FALSE) {
    
    purrr::map_chr(
        .x = objects, 
        .f = ~ digest::digest(., file = is_file_path, algo = self$algo))
}, overwrite = TRUE)


# has_rflow ----
R6Eddy$set("public", "has_rflow", function(fn_key) {
    
    fn_key %in% names(self$rflow)
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
        stop("rflow already exists for key: ", fn_key)
    } else {
        self$rflow_lst[[fn_key]] <- rflow
        self$cache$add_group(fn_key)
        # TODO: reactive: update adjacency matrix
    }
    
    self$cache$has_group(fn_key)
}, overwrite = TRUE)


# delete_rflow ----
R6Eddy$set("public", "delete_rflow", function(fn_key) {
    
    self$require_rflow(fn_key)
    
    self$cache$delete_group(fn_key)
    self$rflow_lst[[fn_key]] <- NULL
    # TODO: reactive: update adjacency matrix
    
    !self$cache$has_group(fn_key)
}, overwrite = TRUE)


# forget_rflow ----
R6Eddy$set("public", "forget_rflow", function(fn_key) {
    # empty the cache without deleting it
    
    self$require_rflow(fn_key)
    
    self$cache$forget_group(fn_key)
    self$rflow_lst[[fn_key]] <- list()
    
    length(self$cache$list_keys(fn_key)) == 0L
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
        fn = NA_character_,
        fn_key = names(self$rflow),
        type = NA_character_,
        n_states = NA_integer_
    ) %>%
        dplyr::left_join(cache_df, by = "fn_key")
    
    emph_obj <- paste0("<", crayon::italic("R6Eddy"), ">")
    cat(emph_obj, " with ", crayon::bold(length(self$rflow_lst)), ":\n")
    print(df)
    
    invisible(self)
}, overwrite = TRUE)
# nocov end


# reset ----
R6Eddy$set("public", "reset", function() {
    # brings eddy in the same state as just after $new()
    
    self$cache$reset()
    self$rflow <- list()
    
    invisible(NULL)
}, overwrite = TRUE)


# terminate ----
R6Eddy$set("public", "terminate", function() {
    # reset + delete its own data structures, e.g. folders
    # object cannot be used afterwards
    
    self$cache$terminate()
    self$cache <- NULL
    self$rf <- NULL
    
    invisible(NULL)
}, overwrite = TRUE)
