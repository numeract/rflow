#' @include helper.R

# !diagnostics suppress=self, private


# R6Eddy ----
R6Eddy <- R6::R6Class(
    classname = 'R6Eddy',
    public = list(
        rflow_lst = list(),
        is_reactive = NULL,
        initialize = function(is_reactive = FALSE, 
                              cache_path = NULL, 
                              algo = "xxhash64") {},
        # rflow
        find_rflow = function(fn_key) {},
        has_rflow = function(fn_key) {},
        get_rflow = function(fn_key) {},
        add_rflow = function(fn_key, rflow) {},
        delete_rflow = function(fn_key, remove_from) {},
        forget_rflow = function(fn_key) {},
        # cache
        digest = function(object, ...) {},
        find_key = function(key, fn_key) {},
        has_data = function(key, fn_key) {},
        get_data = function(key, fn_key) {},
        add_data = function(key, value, fn_key) {},
        delete_data = function(key, fn_key, from) {}
        # TODO: cache_reset() ?
        # TODO: flush/copy to cache level ?
    ),
    private = list(
        cache_path = NULL,
        cache_lst = list(),
        algo = NULL
    )
)


# Initialize ----
R6Eddy$set("public", "initialize", function(is_reactive = FALSE,
                                            cache_path = NULL,
                                            algo = "xxhash64") {
    if (isTRUE(is_reactive)) 
        stop("reactive eddies not yet implemented")
    self$is_reactive <- FALSE
    
    if (!is.null(cache_path)) {
        if (!dir.exists(cache_path)) {
            dir.create(cache_path, showWarnings = FALSE)
        }
        private$cache_path <- normalizePath(
            cache_path, winslash = '/', mustWork = TRUE)
    }
    
    private$algo <- algo
    
    invisible(NULL)
}, overwrite = TRUE)


# find_rflow ----
R6Eddy$set("public", "find_rflow", function(fn_key) {
    
    if (fn_key %in% names(self$rflow_lst)) {
        "memory"
    } else {
        if (!is.null(private$cache_path)) {
            if (dir.exists(file.path(private$cache_path, fn_key))) {
                "disk"
            } else {
                "missing"
            }
        } else {
            "missing"
        }
    }
}, overwrite = TRUE)


# has_rflow ----
R6Eddy$set("public", "has_rflow", function(fn_key) {
    
    fn_key %in% names(self$rflow_lst)
}, overwrite = TRUE)


# get_rflow ----
R6Eddy$set("public", "get_rflow", function(fn_key) {
    
    if (!self$has_rflow(fn_key)) {
        warning("rflow not found for key: ", fn_key)
        NULL
    } else {
        self$rflow_lst[[fn_key]]
    }
}, overwrite = TRUE)


# add_rflow ----
R6Eddy$set("public", "add_rflow", function(fn_key, rflow) {
    
    if (self$has_rflow(fn_key)) {
        stop("rflow already exists for key: ", fn_key)
        # FALSE
    } else {
        self$rflow_lst[[fn_key]] <- rflow
        # TODO: update adjacency matrix
        
        TRUE
    }
}, overwrite = TRUE)


# delete_rflow ----
R6Eddy$set("public", "delete_rflow", function(
    fn_key, 
    from = c("memory", "disk", "all")
) {
    from <- match.arg(from)
    
    if (!self$has_rflow(fn_key)) {
        warning("rflow not found for key: ", fn_key)
        # delete considered succesfull
        TRUE
    } else {
        self$rflow_lst[[fn_key]] <- NULL
        # TODO: remove from specified cache level and all lower levels
        # example: if remove from L2 = disk, also remove from memory
        # valid values for remove_from: TBD
        # TODO: reactive: update adjacency matrix
        
        TRUE
    }
}, overwrite = TRUE)


# forget_rflow ----
R6Eddy$set("public", "forget_rflow", function(fn_key) {
    
    if (!self$has_rflow(fn_key)) {
        warning("rflow not found for key: ", fn_key)
        # delete considered succesfull
        TRUE
    } else {
        # TODO: remove data from all cache levels, but keep fn_key in rflow_lst
        # for now, find keys based on disk fn_key
        fn_path <- file.path(private$cache_path, fn_key)
        res <- TRUE
        for (key in list.files(fn_path)) {
            res <- res && self$delete_data(key, fn_key, from = "all")
        }
        res
    }
}, overwrite = TRUE)


# digest ----
R6Eddy$set("public", "digest", function(object, ...) {
    
    digest::digest(object, ..., algo = private$algo)
}, overwrite = TRUE)


# find_key ----
R6Eddy$set("public", "find_key", function(key, fn_key) {
    
    # where is the key (in which cache), if anywhere
    
    # first, check in memory cache
    if (fn_key %in% names(private$cache_lst)) {
        cache_env <- private$cache_lst[[fn_key]]
        key_in_mem <- base::exists(key, where = cache_env, inherits = FALSE)
    } else {
        key_in_mem <- FALSE
    }
    
    if (key_in_mem) {
        "memory"
    } else {
        if (!is.null(private$cache_path)) {
            key_on_disk <- file.exists(
                file.path(private$cache_path, fn_key, key))
            if (key_on_disk) {
                "disk"
            } else {
                "missing"
            }
        } else {
            "missing"
        }
    }
}, overwrite = TRUE)


# has_data ----
R6Eddy$set("public", "has_data", function(key, fn_key) {
    
    found <- self$find_key(key, fn_key)
    
    found != "missing"
}, overwrite = TRUE)


# get_data ----
R6Eddy$set("public", "get_data", function(key, fn_key) {
    
    found <- self$find_key(key, fn_key)
    if (found == "memory") {
        cache_env <- private$cache_lst[[fn_key]]
        get(key, envir = cache_env, inherits = FALSE)
    } else if (found == "disk") {
        # private$cache_path is not null since key was found on disk
        value <- readRDS(file = file.path(private$cache_path, fn_key, key))
        # copy value (data) to memory too
        if (fn_key %in% names(private$cache_lst)) {
            cache_env <- private$cache_lst[[fn_key]]
        } else {
            cache_env <- new.env(hash = TRUE, parent = emptyenv())
            private$cache_lst[[fn_key]] <- cache_env
        }
        assign(key, value, envir = cache_env)
        value
    } else {
        # cannot return NA / NULL since that can be the result of the function
        # the user should check that the key exists
        stop("key not found: ", key)
    }
    
}, overwrite = TRUE)


# add_data ----
R6Eddy$set("public", "add_data", function(key, value, fn_key) {
    
    # we always overwrite data in cache (but there should not be the case)
    # for now, put it in memory and on disk
    
    # memory
    if (fn_key %in% names(private$cache_lst)) {
        cache_env <- private$cache_lst[[fn_key]]
    } else {
        cache_env <- new.env(hash = TRUE, parent = emptyenv())
        private$cache_lst[[fn_key]] <- cache_env
    }
    assign(key, value, envir = cache_env)
    
    # disk, if cache_path not NULL
    if (!is.null(private$cache_path)) {
        fn_path <- file.path(private$cache_path, fn_key)
        if (!dir.exists(fn_path)) {
            dir.create(fn_path, showWarnings = FALSE)
        }
        saveRDS(value, file = file.path(fn_path, key))
    }
    
    # check if data exists
    self$has_data(key, fn_key)
}, overwrite = TRUE)


# delete_data ----
R6Eddy$set("public", "delete_data", function(key, fn_key, from = "all") {
    
    if (from == "all") 
        from <- c("memory", "disk")
    
    # memory
    if (from == "memory" && fn_key %in% names(private$cache_lst)) {
        cache_env <- private$cache_lst[[fn_key]]
        if (base::exists(key, where = cache_env, inherits = FALSE)) {
            rm(key, envir = cache_env, inherits = FALSE)
        }
    }
    
    # disk
    if (from == "disk" && !is.null(private$cache_path)) {
        key_path <- file.path(private$cache_path, fn_key, key)
        if (file.exists(key_path)) {
            unlink(key_path)
        }
    }
    
    # check if data exists
    !self$has_data(key, fn_key)
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
    
    if (!base::exists('.EDDY', where = envir, inherits = FALSE)) {
        assign('.EDDY', R6Eddy$new(cache_path = "cache"), envir = envir)
    }
    
    envir$.EDDY
}
