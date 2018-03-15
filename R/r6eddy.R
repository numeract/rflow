#' @include helper.R

# !diagnostics suppress=self, private


# R6Eddy ----
R6Eddy <- R6::R6Class(
    classname = 'R6Eddy',
    public = list(
        rflow_lst = list(),
        is_reactive = NULL,
        initialize = function() {},
        # rflow
        exists_rflow = function(fn_name) {},
        add_rflow = function(fn_name, rflow) {},
        delete_rflow = function(fn_name) {},
        # cache
        digest = function(...) {},
        find_key = function(key) {},
        has_key = function(key) {},
        get_data = function(key) {},
        put_data = function(key, value) {}
        # TODO: cache_reset()
        # TODO: flush_to_disk()
    ),
    private = list(
        cache_path = NULL,
        algo = NULL,
        cache = NULL
    )
)


# initialize ----
R6Eddy$set("public", "initialize", function(is_reactive = FALSE,
                                            cache_path = NULL,
                                            algo = "xxhash64") {
    if (isTRUE(is_reactive)) stop("reactive eddies not yet implemented")
    self$is_reactive <- FALSE
    
    if (!is.null(cache_path)) {
        if (!dir.exists(cache_path)) {
            dir.create(cache_path, showWarnings = FALSE)
        }
        private$cache_path <- cache_path
    }
    
    private$algo <- algo
    
    # memory cache always take place
    private$cache <- new.env(TRUE, emptyenv())
    
    invisible(NULL)
}, overwrite = TRUE)


# exists ----
R6Eddy$set("public", "exists_rflow", function(fn_name) {
    
    fn_name %in% names(self$rflow_lst)
}, overwrite = TRUE)


# add_rflow ----
R6Eddy$set("public", "add_rflow", function(fn_name, rflow) {
    
    if (self$exists_rflow(fn_name)) 
        stop("overwriting not yet implemented")
    self$exists_rflow[[fn_name]] <- rflow
    # TODO: update adjacency matrix
    
}, overwrite = TRUE)


# delete_rflow ----
R6Eddy$set("public", "delete_rflow", function(fn_name) {
    
    self$exists_rflow[[fn_name]] <- NULL
    # TODO: do not remove the disk cache, but we should remove memory cache?
    # TODO: update adjacency matrix
    
}, overwrite = TRUE)


# digest ----
R6Eddy$set("public", "digest", function(...) {
    
    digest::digest(..., algo = private$algo)
}, overwrite = TRUE)


# find_key ----
R6Eddy$set("public", "find_key", function(key) {
    
    # where is the key (in what cache), if anywhere
    # first check in memory cache
    key_in_mem <- exists(key, envir = private$cache, inherits = FALSE)
    if (key_in_mem) {
        "memory"
    } else {
        key_on_disk <- file.exists(file.path(private$path, key))
        if (key_on_disk) "disk" else "missing"
    }
}, overwrite = TRUE)


# has_key ----
R6Eddy$set("public", "has_key", function(key) {
    
    found <- self$find_key(key)
    found != "missing"
}, overwrite = TRUE)


# get_data ----
R6Eddy$set("public", "get_data", function(key) {
    
    found <- self$find_key(key)
    if (found == "memory") {
        get(key, envir = private$cache, inherits = FALSE)
    } else if (found == "disk") {
        readRDS(file = file.path(private$cache_path, key))
    } else {
        stop("key not found:", key)
    }
    
}, overwrite = TRUE)


# put_data ----
R6Eddy$set("public", "put_data", function(key, value) {
    
    # we always overwrite data in cache (but there should not be the case)
    # for now, put it in memory and on disk
    assign(key, value, envir = private$cache)
    saveRDS(value, file = file.path(private$path, key))
    
}, overwrite = TRUE)


# Env ----

# create a separate environment to keep eddies
get_default_env <- function() {
    
    # encl_env is package env if package is loaded, GlobalEnv if run outside 
    encl_env <- parent.env(environment())
    if (!base::exists('.EDDY_ENV', where = encl_env, inherits = FALSE)) {
        assign('.EDDY_ENV', new.env(parent = emptyenv()), envir = encl_env)
    }
    
    encl_env$.EDDY_ENV
}


get_default_eddy <- function(envir = get_default_env()) {
    
    if (!base::exists('.EDDY', envir = envir, inherits = FALSE)) {
        assign('.EDDY', R6Eddy$new(), envir = envir)
    }
    
    envir$.EDDY
}
