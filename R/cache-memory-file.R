# cache engine for caching to memory & file 


# !diagnostics suppress=., self, private



R6CacheMemoryFile <- R6::R6Class(
    classname = "R6CacheMemoryFile",
    inherit = R6Cache,
    public = list(
        cache_env = NULL,
        cache_dir = NULL
    )
)


# initialize ----
R6CacheMemoryFile$set("public", "initialize", function(cache_dir) {
    
    stopifnot(rlang::is_string(cache_dir))
    
    self$cache_env <- new.env(hash = TRUE, parent = emptyenv())
    
    # store the absolute, not relative path for cache
    cache_dir <- fs::path_abs(cache_dir)
    if (!fs::dir_exists(cache_dir)) {
        fs::dir_create(cache_dir)
    }
    self$cache_dir <- cache_dir
    
    invisible(NULL)
}, overwrite = TRUE)


# list_groups ----
R6CacheMemoryFile$set("public", "list_groups", function() {
    
    # error if cache_env not an environment
    in_memory <- as.character(ls.str(pos = self$cache_env, all.names = TRUE))
    # error if cache_dir does not exist
    on_disk <- as.character(fs::dir_ls(self$cache_dir, type = "directory"))
    
    # unique groups, in memory groups listed first
    groups <- c(in_memory, on_disk %if_not_in% in_memory)
    
    groups
}, overwrite = TRUE)


# has_group ----
R6CacheMemoryFile$set("public", "has_group", function(group) {
    
    group %in% self$list_groups()
}, overwrite = TRUE)


# add_group ----
R6CacheMemoryFile$set("public", "add_group", function(group) {
    
    stopifnot(fs::dir_exists(self$cache_dir))
    
    # add in memory, if missing
    if (!base::exists(group, where = self$cache_env, inherits = FALSE)) {
        base::assign(group, value = list(), pos = self$cache_env)
    }
    
    # add on disk, if missing
    group_dir <- fs::path(self$cache_dir, group)
    if (!fs::dir_exists(group_dir)) {
        fs::dir_create(group_dir)
    }
    
    self$has_group(group)
}, overwrite = TRUE)


# delete_group ----
R6CacheMemoryFile$set("public", "delete_group", function(group) {
    
    stopifnot(fs::dir_exists(self$cache_dir))
    
    # group may exist either in mem, or on disk, or on both, or may not exist
    if (base::exists(group, where = self$cache_env, inherits = FALSE)) {
        base::rm(list = group, pos = self$cache_env)
    }
    
    group_dir <- fs::path(self$cache_dir, group)
    if (fs::dir_exists(group_dir)) {
        fs::dir_delete(group_dir)
    }
    
    !self$has_group(group)
}, overwrite = TRUE)


# forget_group ----
R6CacheMemoryFile$set("public", "forget_group", function(group) {
    
    stopifnot(fs::dir_exists(self$cache_dir))
    
    # this also adds the group in memory, if missing
    base::assign(group, value = list(), pos = self$cache_env)
    
    # this also adds the group on disk, if missing
    group_dir <- fs::path(self$cache_dir, group)
    if (fs::dir_exists(group_dir)) {
        fs::dir_delete(group_dir)
    }
    fs::dir_create(group_dir)
    
    length(self$cache_env[[group]]) + length(fs::dir_ls(group_dir)) == 0L
}, overwrite = TRUE)


# list_keys ----
R6CacheMemoryFile$set("public", "list_keys", function(group) {
    
    # no error if group NOT present in memory
    kv_lst <- base::get0(
        group, envir = self$cache_env, inherits = FALSE, ifnotfound = list())
    in_memory <- as.character(names(kv_lst))
    
    # no error if group NOT present on disk
    group_dir <- fs::path(self$cache_dir, group)
    on_disk <- if (fs::dir_exists(group_dir)) {
        as.character(fs::dir_ls(group_dir, type = "file"))
    } else {
        character(0L)
    }
    
    # unique keys, in memory keys listed first
    keys <- c(in_memory, on_disk %if_not_in% in_memory)
    
    keys
}, overwrite = TRUE)


# has_key ----
R6CacheMemoryFile$set("public", "has_key", function(group, key) {
    
    key %in% self$list_keys(group)
}, overwrite = TRUE)


# get_data ----
R6CacheMemoryFile$set("public", "get_data", function(group, key) {
    
    stopifnot(fs::dir_exists(self$cache_dir))
    
    # error if group not present in memory
    kv_lst <- base::get(group, envir = self$cache_env, inherits = FALSE)
    if (key %in% names(kv_lst)) {
        # found it in memory
        value <- kv_lst[[key]]
        # assume it is also present on disk since it was written to both
    } else {
        # if not on disk (and not in memory) ==> error
        key_path <- fs::path(self$cache_dir, group, key)
        stopifnot(fs::file_exists(key_path))
        value <- readRDS(key_path)
        # copy data to memory cache
        if (is.null(value)) {
            kv_lst[key] <- list(NULL)
        } else {
            kv_lst[[key]] <- value
        }
        base::assign(group, value = kv_lst, pos = self$cache_env)
    }
    
    value
}, overwrite = TRUE)


# add_data ----
R6CacheMemoryFile$set("public", "add_data", function(group, key, value) {
    
    # add group only if not already present
    self$add_group(group)
    
    kv_lst <- base::get(group, envir = self$cache_env, inherits = FALSE)
    if (is.null(value)) {
        kv_lst[key] <- list(NULL)
    } else {
        kv_lst[[key]] <- value
    }
    base::assign(group, value = kv_lst, pos = self$cache_env)
    
    key_path <- fs::path(self$cache_dir, group, key)
    saveRDS(value, key_path)
    
    self$has_key(group, key)
}, overwrite = TRUE)


# delete_data ----
R6CacheMemoryFile$set("public", "delete_data", function(group, key) {
    
    # add group only if not already present
    self$add_group(group)
    
    kv_lst <- base::get(group, envir = self$cache_env, inherits = FALSE)
    kv_lst[[key]] <- NULL
    base::assign(group, value = kv_lst, pos = self$cache_env)
    
    key_path <- fs::path(self$cache_dir, group, key)
    if (fs::file_exists(key_path)) {
        fs::file_delete(key_path)
    }
    
    !self$has_key(group, key)
}, overwrite = TRUE)


# summary ----
R6CacheMemoryFile$set("public", "summary", function() {
    
    groups <- self$list_groups()
    
    in_memory <- 
        groups %>%
        purrr::map_int(~ length(base::get0(
            ., envir = self$cache_env, inherits = FALSE, ifnotfound = list())))
    
    on_disk <- 
        groups %>%
        purrr::map_int(function(group) {
            group_dir <- fs::path(self$cache_dir, group)
            if (fs::dir_exists(group_dir)) {
                length(fs::dir_ls(group_dir, type = "file"))
            } else {
                0L
            }
        })
    
    df <- tibble::tibble(
        fn_key = groups,
        in_memory = in_memory,
        on_disk = on_disk
    )
    
    df
}, overwrite = TRUE)


# reset ----
R6CacheMemoryFile$set("public", "reset", function() {
    # the instance is as if just initialized
    
    self$cache_env <- new.env(hash = TRUE, parent = emptyenv())
    fs::dir_delete(self$cache_dir)
    fs::dir_create(self$cache_dir)
    
    gc()
    invisible(NULL)
}, overwrite = TRUE)


# terminate ----
R6CacheMemoryFile$set("public", "terminate", function() {
    # reset + delete its own data structures, e.g. folders
    # object cannot be used afterwards
    
    self$cache_env <- NULL
    
    fs::dir_delete(self$cache_dir)
    self$cache_dir <- NULL
    
    gc()
    invisible(NULL)
}, overwrite = TRUE)
