# cache functions, some accessible to the user


# !diagnostics suppress=.,


#' Get the memory only cache engine.
#' 
#' All cache data will be stored only in memory.
#' 
#' @return A cache object that inherits from \code{R6Cache}.
#' 
#' @family cache functions
#' 
#' @export
cache_memory <- function() {
    
    R6CacheMemory$new()
}


#' Get the file only cache engine.
#' 
#' All cache data will be stored only on the local disk.
#' 
#' @return A cache object that inherits from \code{R6Cache}.
#' 
#' @family cache functions
#' 
#' @export
cache_file <- function(cache_dir) {
    
    R6CacheFile$new(cache_dir)
}


#' Get the memory-file cache engine.
#' 
#' The cache data will be stored both in memory (for quick access) and on
#'   disk (for persistence).
#' 
#' @return A cache object that inherits from \code{R6Cache}.
#' 
#' @family cache functions
#' 
#' @export
cache_memory_file <- function(cache_dir) {
    
    R6CacheFile$new(cache_dir)
}


#' Get the default cache engine.
#' 
#' Currently, the default cache engine is \code{cache_memory()}, but this 
#'   might change in the future.
#' 
#' @return A cache object that inherits from \code{R6Cache}.
#' 
#' @family cache functions
#' 
#' @export
default_cache <- function() {
    
    cache_memory()
}
