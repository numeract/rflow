# cache engine for memory only


# !diagnostics suppress=., self, private



R6CacheMemory <- R6::R6Class(
    classname = "R6CacheMemory",
    inherit = R6Cache,
    public = list(
        cache_env = NULL
    )
)


# initialize ----
R6CacheMemory$set("public", "initialize", function() {
    
    self$cache_env <- new.env(hash = TRUE, parent = emptyenv())
    
    invisible(NULL)
}, overwrite = TRUE)


# list_groups ----
R6CacheMemory$set("public", "list_groups", function() {
    
    as.character(ls.str(pos = self$cache_env, all.names = TRUE))
}, overwrite = TRUE)


# has_group ----
R6CacheMemory$set("public", "has_group", function(group) {

    base::exists(group, where = self$cache_env, inherits = FALSE)
}, overwrite = TRUE)


# add_group ----
R6CacheMemory$set("public", "add_group", function(group) {
    
    if (!self$has_group(group)) {
        base::assign(group, value = list(), pos = self$cache_env)
    }
    
    self$has_group(group)
}, overwrite = TRUE)


# delete_group ----
R6CacheMemory$set("public", "delete_group", function(group) {
    
    if (!self$has_group(group)) {
        base::rm(list = group, pos = self$cache_env)
    }
    
    !self$has_group(group)
}, overwrite = TRUE)


# list_keys ----
R6CacheMemory$set("public", "list_keys", function(group) {
    
    kv_lst <- base::get0(
        group, envir = self$cache_env, inherits = FALSE, ifnotfound = list())
    
    as.character(names(kv_lst))
}, overwrite = TRUE)


# has_key ----
R6CacheMemory$set("public", "has_key", function(group, key) {
    
    key %in% self$list_keys(group)
}, overwrite = TRUE)


# get_data ----
R6CacheMemory$set("public", "get_data", function(group, key) {
    
    # error if group not present
    kv_lst <- base::get(group, envir = self$cache_env, inherits = FALSE)
    
    if (!(key %in% names(kv_lst))) {
        stop("key ", key, "not found for group ", group)
    }
    
    kv_lst[[key]]
}, overwrite = TRUE)


# add_data ----
R6CacheMemory$set("public", "add_data", function(group, key, value) {
    
    # add group only if not already present
    self$add_group(group)
    # error if group not present
    kv_lst <- base::get(group, envir = self$cache_env, inherits = FALSE)
    
    kv_lst[[key]] <- value
    base::assign(group, value = kv_lst, pos = self$cache_env)
    
    self$has_key(group, key)
}, overwrite = TRUE)


# delete_data ----
R6CacheMemory$set("public", "delete_data", function(group, key) {
    
    # add group only if not already present
    self$add_group(group)
    # error if group not present
    kv_lst <- base::get(group, envir = self$cache_env, inherits = FALSE)
    
    kv_lst[[key]] <- NULL
    base::assign(group, value = kv_lst, pos = self$cache_env)
    
    !self$has_key(group, key)
}, overwrite = TRUE)


# reset ----
R6CacheMemory$set("public", "reset", function() {
    
    self$cache_env <- new.env(hash = TRUE, parent = emptyenv())
    # old $cache_env is now unbound, force gc() to free up memory
    gc()
    
    invisible(NULL)
}, overwrite = TRUE)


# delete_all ----
R6CacheMemory$set("public", "delete_all", function() {
    
    # reset + delete its own data structures, e.g. folders
    # object cannot be used aftwerwords
    self$cache_env <- NULL
    gc()
    
    invisible(NULL)
}, overwrite = TRUE)


# sync ----
R6CacheMemory$set("public", "sync", function() {
    
    invisible(NULL)
}, overwrite = TRUE)
