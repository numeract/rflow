# wrappers around R6Eddy following standard R functionality


# @include r6eddy.R


# create a separate environment to keep eddies
.EDDY_ENV <- new.env(parent = emptyenv())


#' @export
get_default_env <- function() {
    .EDDY_ENV
}


#' @export
get_default_eddy <- function(cache_path = NULL,
                             envir = get_default_env()) {
    
    if (!base::exists('.EDDY', where = envir, inherits = FALSE)) {
        assign('.EDDY', R6Eddy$new(cache_path = cache_path), envir = envir)
    }
    
    envir$.EDDY
}
