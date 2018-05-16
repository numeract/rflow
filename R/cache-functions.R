# cache functions, some accesible to the user


# !diagnostics suppress=.,


#' Get the memory cache engine.
#' 
#' @return A cache object that inherits from \code{R6Cache}.
#' 
#' @family cache functions
#' 
#' @export
cache_memory <- function() {
    
    R6CacheMemory$new()
}


#' Get the default cache engine (currenlty \code{cache_memory()}).
#' 
#' @return A cache object that inherits from \code{R6Cache}.
#' 
#' @family cache functions
#' 
#' @export
default_cache <- function() {
    
    cache_memory()
}
