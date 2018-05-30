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
R6CacheFile$set("public", "initialize", function(cache_dir) {
    
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
R6CacheFile$set("public", "list_groups", function() {
    
    # error if cache_dir does not exist
    as.character(
        fs::path_file(fs::dir_ls(self$cache_dir, type = "directory")))
    
}, overwrite = TRUE)


# has_group ----
R6CacheFile$set("public", "has_group", function(group) {
    
    require_keys(group)
    
    group_dir <- fs::path(self$cache_dir, group)
    fs::dir_exists(group_dir)
}, overwrite = TRUE)


# add_group ----
R6CacheFile$set("public", "add_group", function(group) {
    
    require_keys(group)
    
    stopifnot(fs::dir_exists(self$cache_dir))
    
    group_dir <- fs::path(self$cache_dir, group)
    if (!fs::dir_exists(group_dir)) {
        fs::dir_create(group_dir)
    }
    
    self$has_group(group)
}, overwrite = TRUE)


# forget_group ----
R6CacheFile$set("public", "forget_group", function(group) {
    
    require_keys(group)
    
    stopifnot(fs::dir_exists(self$cache_dir))
    
    # this also adds the group on disk, if missing
    group_dir <- fs::path(self$cache_dir, group)
    if (fs::dir_exists(group_dir)) {
        unlink(group_dir, recursive = TRUE, force = DIR_DELETE_FORCE)
        Sys.sleep(DIR_DELETE_WAIT)
    }
    fs::dir_create(group_dir)
    
    self$has_group(group) && length(fs::dir_ls(group_dir)) == 0L
}, overwrite = TRUE)


# delete_group ----
R6CacheFile$set("public", "delete_group", function(group) {
    
    require_keys(group)
    
    stopifnot(fs::dir_exists(self$cache_dir))
    
    group_dir <- fs::path(self$cache_dir, group)
    if (fs::dir_exists(group_dir)) {
        unlink(group_dir, recursive = TRUE, force = DIR_DELETE_FORCE)
        # Sys.sleep(DIR_DELETE_WAIT)
    }
    
    !self$has_group(group)
}, overwrite = TRUE)


# list_keys ----
R6CacheFile$set("public", "list_keys", function(group) {
    
    require_keys(group)
    
    stopifnot(fs::dir_exists(self$cache_dir))
    
    # no error if group NOT present on disk
    group_dir <- fs::path(self$cache_dir, group)
    if (fs::dir_exists(group_dir)) {
        as.character(fs::path_file(fs::dir_ls(group_dir, type = "file")))
    } else {
        character(0L)
    }
}, overwrite = TRUE)


# has_key ----
R6CacheFile$set("public", "has_key", function(group, key) {
    
    require_keys(group, key)
    
    key %in% self$list_keys(group)
}, overwrite = TRUE)


# get_data ----
R6CacheFile$set("public", "get_data", function(group, key) {
    
    require_keys(group, key)
    
    stopifnot(fs::dir_exists(self$cache_dir))
    
    key_path <- fs::path(self$cache_dir, group, key)
    stopifnot(fs::file_exists(key_path))
    
    readRDS(key_path)
}, overwrite = TRUE)


# add_data ----
R6CacheFile$set("public", "add_data", function(group, key, value) {
    
    require_keys(group, key)
    
    # add group only if not already present
    self$add_group(group)
    
    key_path <- fs::path(self$cache_dir, group, key)
    saveRDS(value, key_path)
    
    self$has_key(group, key)
}, overwrite = TRUE)


# delete_data ----
R6CacheFile$set("public", "delete_data", function(group, key) {
    
    require_keys(group, key)
    
    # add group only if not already present
    self$add_group(group)
    
    key_path <- fs::path(self$cache_dir, group, key)
    if (fs::file_exists(key_path)) {
        fs::file_delete(key_path)
    }
    
    !self$has_key(group, key)
}, overwrite = TRUE)


# summary ----
R6CacheFile$set("public", "summary", function() {
    
    groups <- self$list_groups()
    n_keys <- groups %>%
        purrr::map_int(~ length(self$list_keys(.)))
    df <- tibble::tibble(
        fn_key = groups,
        on_disk = n_keys
    )
    
    df
}, overwrite = TRUE)


# reset ----
R6CacheFile$set("public", "reset", function() {
    # the instance is as if just initialized
    
    if (fs::dir_exists(self$cache_dir)) {
        unlink(self$cache_dir, recursive = TRUE, force = DIR_DELETE_FORCE)
        Sys.sleep(DIR_DELETE_WAIT)
    }
    fs::dir_create(self$cache_dir)
    
    invisible(self)
}, overwrite = TRUE)


# terminate ----
R6CacheFile$set("public", "terminate", function() {
    # reset + delete its own data structures, e.g. folders
    # object cannot be used afterwards
    
    if (fs::dir_exists(self$cache_dir)) {
        unlink(self$cache_dir, recursive = TRUE, force = DIR_DELETE_FORCE)
    }
    self$cache_dir <- NULL
    
    invisible(NULL)
}, overwrite = TRUE)
