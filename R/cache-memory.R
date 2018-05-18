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
    
    # error if cache_env not an environment
    as.character(ls.str(pos = self$cache_env, all.names = TRUE))
}, overwrite = TRUE)


# has_group ----
R6CacheMemory$set("public", "has_group", function(group) {
    
    require_keys(group)
    
    base::exists(group, where = self$cache_env, inherits = FALSE)
}, overwrite = TRUE)


# add_group ----
R6CacheMemory$set("public", "add_group", function(group) {
    
    require_keys(group)
    
    if (!base::exists(group, where = self$cache_env, inherits = FALSE)) {
        base::assign(group, value = list(), pos = self$cache_env)
    }
    
    self$has_group(group)
}, overwrite = TRUE)


# delete_group ----
R6CacheMemory$set("public", "delete_group", function(group) {
    
    require_keys(group)
    
    if (base::exists(group, where = self$cache_env, inherits = FALSE)) {
        base::rm(list = group, pos = self$cache_env)
    }
    
    !self$has_group(group)
}, overwrite = TRUE)


# forget_group ----
R6CacheMemory$set("public", "forget_group", function(group) {
    
    require_keys(group)
    
    # this also adds the group in memory, if missing
    base::assign(group, value = list(), pos = self$cache_env)
    
    length(self$cache_env[[group]]) == 0L
}, overwrite = TRUE)


# list_keys ----
R6CacheMemory$set("public", "list_keys", function(group) {
    
    require_keys(group)
    
    # no error if group NOT present in memory
    kv_lst <- base::get0(
        group, envir = self$cache_env, inherits = FALSE, ifnotfound = list())
    
    as.character(names(kv_lst))
}, overwrite = TRUE)


# has_key ----
R6CacheMemory$set("public", "has_key", function(group, key) {
    
    require_keys(group, key)
    
    key %in% self$list_keys(group)
}, overwrite = TRUE)


# get_data ----
R6CacheMemory$set("public", "get_data", function(group, key) {
    
    require_keys(group, key)
    
    # error if group not present in memory
    kv_lst <- base::get(group, envir = self$cache_env, inherits = FALSE)
    
    if (!(key %in% names(kv_lst))) {
        stop("key ", key, "not found for group ", group)
    }
    
    kv_lst[[key]]
}, overwrite = TRUE)


# add_data ----
R6CacheMemory$set("public", "add_data", function(group, key, value) {
    
    require_keys(group, key)
    
    # add group only if not already present
    self$add_group(group)
    
    kv_lst <- base::get(group, envir = self$cache_env, inherits = FALSE)
    if (is.null(value)) {
        kv_lst[key] <- list(NULL)
    } else {
        kv_lst[[key]] <- value
    }
    base::assign(group, value = kv_lst, pos = self$cache_env)
    
    self$has_key(group, key)
}, overwrite = TRUE)


# delete_data ----
R6CacheMemory$set("public", "delete_data", function(group, key) {
    
    require_keys(group, key)
    
    # add group only if not already present
    self$add_group(group)
    
    kv_lst <- base::get(group, envir = self$cache_env, inherits = FALSE)
    kv_lst[[key]] <- NULL
    base::assign(group, value = kv_lst, pos = self$cache_env)
    
    !self$has_key(group, key)
}, overwrite = TRUE)


# summary ----
R6CacheMemory$set("public", "summary", function() {
    
    groups <- self$list_groups()
    n_keys <- groups %>%
        purrr::map_int(~ length(self$list_keys(.)))
    df <- tibble::tibble(
        fn_key = groups,
        in_memory = n_keys
    )
    
    df
}, overwrite = TRUE)


# print ----
# nocov start
R6CacheMemory$set("public", "print", function() {
    
    df <- self$summary()
    
    emph_obj <- paste0("<", crayon::italic("R6CacheMemory"), ">")
    cat(emph_obj, " with ", crayon::bold(nrow(df)), " fn_keys:\n")
    print(df)
    
    invisible(self)
}, overwrite = TRUE)
# nocov end


# reset ----
R6CacheMemory$set("public", "reset", function() {
    # the instance is as if just initialized
    
    self$cache_env <- new.env(hash = TRUE, parent = emptyenv())
    
    # old $cache_env is now unbound, force gc() to free up memory
    gc()
    invisible(self)
}, overwrite = TRUE)


# terminate ----
R6CacheMemory$set("public", "terminate", function() {
    # reset + delete its own data structures, e.g. folders
    # object cannot be used afterwards
    
    self$cache_env <- NULL
    
    gc()
    invisible(NULL)
}, overwrite = TRUE)
