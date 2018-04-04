# R6Eddy class and methods


# !diagnostics suppress=self, public


# R6Eddy ----
R6Eddy <- R6::R6Class(
    classname = 'R6Eddy',
    public = list(
        cache_path = NULL,
        is_reactive = NULL,
        algo = NULL,
        name = NULL,
        
        rflow_lst = list(),
        cache_lst = list(),
        # init
        initialize = function(is_reactive = FALSE, 
                              cache_path = NULL, 
                              algo = "xxhash64") {},
        reset = function() {},
        print = function() {},
        
        # rflow
        find_rflow = function(fn_key) {},
        has_rflow = function(fn_key) {},
        get_rflow = function(fn_key) {},
        add_rflow = function(fn_key, rflow) {},
        delete_rflow = function(fn_key, from) {},
        forget_rflow = function(fn_key) {},
        # cache
        digest = function(object, ...) {},
        find_key = function(key, fn_key) {},
        has_data = function(key, fn_key) {},
        get_data = function(key, fn_key) {},
        add_data = function(key, value, fn_key) {},
        delete_data = function(key, fn_key, from) {}
        
        # TODO: flush/copy to cache level ?
    )
)


# initialize ----
R6Eddy$set("public", "initialize", function(cache_path = NULL,
                                            name = NULL,
                                            is_reactive = FALSE,
                                            algo = "xxhash64") {
    if (isTRUE(is_reactive)) 
        stop("reactive eddies not yet implemented")
    self$is_reactive <- FALSE
    
    if (!is.null(cache_path)) {
        if (!dir.exists(cache_path)) {
            dir.create(cache_path, showWarnings = FALSE)
        }
        self$cache_path <- normalizePath(
            cache_path, winslash = '/', mustWork = TRUE)
    }
    
    self$algo <- algo
    self$name <- name
    
    invisible(NULL)
}, overwrite = TRUE)


# reset ----
R6Eddy$set("public", "reset", function() {
    # brings eddy in the same state as just after $new()
    self$rflow_lst <- list()
    self$cache_lst <- list()
    
    if (!is.null(self$cache_path)) {
        unlink(self$cache_path, recursive = TRUE)
        Sys.sleep(1)
        if (dir.exists(self$cache_path)) {
            stop("cache folder couldn't be deleted.")
        }
    }
    
    TRUE
}, overwrite = TRUE)


# print ----
R6Eddy$set("public", "print", function() { # nocov start
    
    no_rflows <- "no RFlows"
    cached_fn <- "NA"
    cache_path <- "NA"
    
    if (!is.null(self$cache_path)) {
        cache_path < paste0("\"", self$cache_path, "\"")
    }
    
    no_rflows <- paste0(length(self$rflow_lst), " rflow")
    
    cat(crayon::italic("R6Eddy"), " with ", crayon::bold(no_rflows), ":\n",
        "  - name: ", crayon::italic(self$name), "\n",
        "  - cache path: ", crayon::italic(cache_path), "\n", sep = "")
    
    rflow_names <- names(self$rflow_lst)
    cache_names <- names(self$cache_lst)
    
    file_names <- c()
    if (!is.null(self$cache_path)) {
        file_names <- list.files(self$cache_path)
    }
    
    fn_keys <- unique(c(rflow_names, cache_names, file_names))
    
    m <- matrix(nrow = length(fn_keys), ncol = 6)
    colnames(m) <-
        c("fn_name", "fn_key", "is_rflow", "n_states", "in_memory", "on_disk")
    
    for (i in seq_along(fn_keys)) {
        fn_key <- fn_keys[[i]]
        func <- NA
        is_rflow <- FALSE
        in_memory <- NA
        on_disk <- NA
        
        is_rflow <- inherits(self$rflow_lst[[fn_key]], "R6Flow")
        
        cache_env <- self$cache_lst[[fn_key]]
        if (!is.null(cache_env)) {
            in_memory <- length(ls(cache_env))
        }
        
        if (!is.null(self$cache_path)) {
            on_disk <- length(list.files(file.path(self$cache_path, fn_key)))
        }
        
        state <- NA
        if (is_rflow) {
            rflow <- self$rflow_lst[[fn_key]]
            state <- nrow(rflow$state)
            func <- rflow$fn_name
        }
        
        m[i, ] <- c(func,
                    fn_key,
                    is_rflow,
                    state,
                    in_memory,
                    on_disk)
    }
    
    print(as.data.frame(m))
    
    invisible(self)
}, overwrite = TRUE) # nocov end


# find_rflow ----
R6Eddy$set("public", "find_rflow", function(fn_key) {
    
    if (fn_key %in% names(self$rflow_lst)) {
        "memory"
    } else {
        if (!is.null(self$cache_path)) {
            if (dir.exists(file.path(self$cache_path, fn_key))) {
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
    if (from == "all") from <- c("memory", "disk")
    
    # TODO: remove from specified cache level and all lower levels
    # example: if remove from L2 = disk, also remove from memory
    # valid values for from: TBD
    
    if ("disk" %in% from) {
        fn_path <- file.path(self$cache_path, fn_key)
        res <- TRUE
        for (key in list.files(fn_path)) {
            res <- res && self$delete_data(key, fn_key, from = "all")
        }
        res
        # delete folder on disk
        unlink(fn_path, recursive = TRUE)
    }
    # delete cache envir from memory
    self$cache_lst[[fn_key]] <- NULL
    self$rflow_lst[[fn_key]] <- NULL
    
    # TODO: reactive: update adjacency matrix
    
    TRUE
}, overwrite = TRUE)


# forget_rflow ----
R6Eddy$set("public", "forget_rflow", function(fn_key) {
    
    if (!self$has_rflow(fn_key)) {
        warning("rflow not found for key: ", fn_key)
        # delete considered successful
        TRUE
    } else {
        # TODO: remove data from all cache levels, but keep fn_key in rflow_lst
        keys <- ls(self$cache_lst[[fn_key]])
        if (!is.null(self$cache_path)) {
            # for now, find keys based on disk fn_key
            fn_path <- file.path(self$cache_path, fn_key)
            keys <- c(keys, list.files(fn_path))
            keys <- unique(keys)
        }
        res <- TRUE
        for (key in keys) {
            res <- res && self$delete_data(key, fn_key, from = "all")
        }
        res
    }
}, overwrite = TRUE)


# digest ----
R6Eddy$set("public", "digest", function(object, ...) {
    
    digest::digest(object, ..., algo = self$algo)
}, overwrite = TRUE)


# find_key ----
R6Eddy$set("public", "find_key", function(key, fn_key) {
    
    # where is the key (in which cache), if anywhere
    
    # first, check in memory cache
    if (fn_key %in% names(self$cache_lst)) {
        cache_env <- self$cache_lst[[fn_key]]
        key_in_mem <- base::exists(key, where = cache_env, inherits = FALSE)
    } else {
        key_in_mem <- FALSE
    }
    
    if (key_in_mem) {
        "memory"
    } else {
        if (!is.null(self$cache_path)) {
            key_on_disk <- file.exists(
                file.path(self$cache_path, fn_key, paste0(key, ".rds")))
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
        cache_env <- self$cache_lst[[fn_key]]
        get(key, envir = cache_env, inherits = FALSE)
    } else if (found == "disk") {
        # self$cache_path is not null since key was found on disk
        value <- readRDS(
            file = file.path(self$cache_path, fn_key, paste0(key, ".rds")))
        # copy value (data) to memory too
        if (fn_key %in% names(self$cache_lst)) {
            cache_env <- self$cache_lst[[fn_key]]
        } else {
            cache_env <- new.env(hash = TRUE, parent = emptyenv())
            self$cache_lst[[fn_key]] <- cache_env
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
    if (fn_key %in% names(self$cache_lst)) {
        cache_env <- self$cache_lst[[fn_key]]
    } else {
        cache_env <- new.env(hash = TRUE, parent = emptyenv())
        self$cache_lst[[fn_key]] <- cache_env
    }
    assign(key, value, envir = cache_env)
    
    # disk, if cache_path not NULL
    if (!is.null(self$cache_path)) {
        fn_path <- file.path(self$cache_path, fn_key)
        if (!dir.exists(fn_path)) {
            dir.create(fn_path, showWarnings = FALSE) # nocov
        }
        saveRDS(value, file = file.path(fn_path, paste0(key, ".rds")))
    }
    
    # check if data exists
    self$has_data(key, fn_key)
}, overwrite = TRUE)


# delete_data ----
R6Eddy$set("public", "delete_data", function(key, fn_key, from = "all") {
    
    if (from == "all") 
        from <- c("memory", "disk")
    
    # memory
    if ("memory" %in% from && fn_key %in% names(self$cache_lst)) {
        cache_env <- self$cache_lst[[fn_key]]
        if (base::exists(key, where = cache_env, inherits = FALSE)) {
            rm(list = key, envir = cache_env, inherits = FALSE)
        }
    }
    
    # disk
    if ("disk" %in% from && !is.null(self$cache_path)) {
        key_path <- file.path(self$cache_path, fn_key, paste0(key, ".rds"))
        if (file.exists(key_path)) {
            unlink(key_path)
        }
    }
    
    # check if data exists
    !self$has_data(key, fn_key)
}, overwrite = TRUE)
