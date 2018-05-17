# cache engine for file only


# !diagnostics suppress=., self, private



R6CacheFile <- R6::R6Class(
    classname = "R6CacheFile",
    inherit = R6Cache,
    public = list(
        cache_dir = NULL
    )
)


# initialize ----
R6CacheMemory$set("public", "initialize", function(cache_dir) {
    
    stopifnot(rlang::is_string(cache_dir))
    
    # store the absolute, not relative path for cache
    cache_dir <- fs::path_abs(cache_dir)
    if (!fs::dir_exists(cache_dir)) {
        fs::dir_create(cache_dir)
    }
    
    self$cache_dir <- cache_dir
    
    invisible(NULL)
}, overwrite = TRUE)


# list_groups ----
R6CacheMemory$set("public", "list_groups", function() {
    
    stopifnot(fs::dir_exists(self$cache_dir))
    
    as.character(fs::dir_ls(self$cache_dir, type = "directory"))
}, overwrite = TRUE)


# has_group ----
R6CacheMemory$set("public", "has_group", function(group) {
    
    group %in% self$list_groups()
}, overwrite = TRUE)


# add_group ----
R6CacheMemory$set("public", "add_group", function(group) {
    
    stopifnot(fs::dir_exists(self$cache_dir))
    
    group_dir <- fs::path(self$cache_dir, group)
    if (!fs::dir_exists(group_dir)) {
        fs::dir_create(group_dir)
    }
    
    self$has_group(group)
}, overwrite = TRUE)


# delete_group ----
R6CacheMemory$set("public", "delete_group", function(group) {
    
    stopifnot(fs::dir_exists(self$cache_dir))
    
    group_dir <- fs::path(self$cache_dir, group)
    if (fs::dir_exists(group_dir)) {
        fs::dir_delete(group_dir)
    }
    
    !self$has_group(group)
}, overwrite = TRUE)


# forget_group ----
R6CacheMemory$set("public", "forget_group", function(group) {
    
    stopifnot(fs::dir_exists(self$cache_dir))
    
    group_dir <- fs::path(self$cache_dir, group)
    if (fs::dir_exists(group_dir)) {
        fs::dir_delete(group_dir)
    }
    fs::dir_create(group_dir)
    
    length(fs::dir_ls(group_dir)) == 0L
}, overwrite = TRUE)


# list_keys ----
R6CacheMemory$set("public", "list_keys", function(group) {
    
    stopifnot(fs::dir_exists(self$cache_dir))
    
    group_dir <- fs::path(self$cache_dir, group)
    stopifnot(fs::dir_exists(group_dir))
    
    as.character(fs::dir_ls(group_dir, type = "file"))
}, overwrite = TRUE)


# has_key ----
R6CacheMemory$set("public", "has_key", function(group, key) {
    
    key %in% self$list_keys(group)
}, overwrite = TRUE)


# get_data ----
R6CacheMemory$set("public", "get_data", function(group, key) {
    
    stopifnot(fs::dir_exists(self$cache_dir))
    
    key_path <- fs::path(self$cache_dir, group, key)
    stopifnot(fs::file_exists(key_path))
    
    readRDS(key_path)
}, overwrite = TRUE)


# add_data ----
R6CacheMemory$set("public", "add_data", function(group, key, value) {
    
    # add group only if not already present
    self$add_group(group)
    
    key_path <- fs::path(self$cache_dir, group, key)
    saveRDS(value, key_path)
    
    self$has_key(group, key)
}, overwrite = TRUE)


# delete_data ----
R6CacheMemory$set("public", "delete_data", function(group, key) {
    
    # add group only if not already present
    self$add_group(group)
    
    key_path <- fs::path(self$cache_dir, group, key)
    if (fs::file_exists(key_path)) {
        fs::file_delete(key_path)
    }
    
    !self$has_key(group, key)
}, overwrite = TRUE)


# summary ----
R6CacheMemory$set("public", "summary", function() {
    
    groups <- self$list_groups()
    n_keys <- groups %>%
        purrr::map_int(~ length(self$list_keys(.)))
    df <- tibble::tibble(
        fn_key = groups,
        on_disk = n_keys
    )
    
    df
}, overwrite = TRUE)


# print ----
# nocov start
R6CacheMemory$set("public", "print", function() {
    
    df <- self$summary()
    
    emph_obj <- paste0("<", crayon::italic("R6CacheFile"), ">")
    cat(emph_obj, " with ", crayon::bold(nrow(df)), " fn_keys:\n")
    print(df)
    
    invisible(self)
}, overwrite = TRUE)
# nocov end


# reset ----
R6CacheMemory$set("public", "reset", function() {
    
    fs::dir_delete(self$cache_dir)
    fs::dir_create(self$cache_dir)
    
    invisible(NULL)
}, overwrite = TRUE)


# terminate ----
R6CacheMemory$set("public", "terminate", function() {
    # reset + delete its own data structures, e.g. folders
    # object cannot be used afterwards
    
    fs::dir_delete(self$cache_dir)
    self$cache_dir <- NULL
    
    invisible(NULL)
}, overwrite = TRUE)
