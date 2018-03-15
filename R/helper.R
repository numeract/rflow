#' @include package.R 


keep_if_in <- function(x, y) {
    
    x[x %in% y]
}


keep_if_not_in <- function(x, y) {
    
    x[!(x %in% y)]
}


keep_at <- function(.x, .at) {
    
    .p <- if (is.character(.at)) {
        names(.x) %in% .at
    } else if (is.numeric(.at)) {
        seq_along(.x) %in% as.integer(.at)
    } else {
        stop("`.at` must be character (names) or a numeric (positions)")
    }
    
    purrr::keep(.x, .p)
}


discard_at <- function(.x, .at) {
    
    .p <- if (is.character(.at)) {
        names(.x) %in% .at
    } else if (is.numeric(.at)) {
        seq_along(.x) %in% as.integer(.at)
    } else {
        stop("`.at` must be character (names) or a numeric (positions)")
    }
    
    purrr::discard(.x, .p)
}
