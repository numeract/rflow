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
#' @examples 
#' cache_in_memory <- cache_memory()
#' 
#' @export
cache_memory <- function() {
    
    R6CacheMemory$new()
}


#' Get the file only cache engine.
#' 
#' All cache data will be stored only on the local disk.
#' 
#' @param cache_dir Directory where to store the cache files.
#' 
#' @return A cache object that inherits from \code{R6Cache}.
#' 
#' @family cache functions
#' 
#' @examples 
#' \dontrun{
#' cache_in_file <- cache_file("path_to_cache_diretory")
#' }
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
#' @param cache_dir Directory where to store the cache files.
#' 
#' @return A cache object that inherits from \code{R6Cache}.
#' 
#' @family cache functions
#' 
#' @examples
#' \dontrun{
#' cache_fmem <- cache_memory_file("path_to_cache_diretory")
#' }
#'  
#'  
#' @export
cache_memory_file <- function(cache_dir) {
    
    R6CacheMemoryFile$new(cache_dir)
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
#' @examples
#' \dontrun{
#' current_cache <- default_cache()
#' }
#' 
#' @export
default_cache <- function() {
    
    cache_memory()
}
