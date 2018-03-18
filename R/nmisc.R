# functions from Nmisc


keep_if_in <- function(x, y) {
    
    x[x %in% y]
}
`%if_in%` <- keep_if_in


keep_if_not_in <- function(x, y) {
    
    x[!(x %in% y)]
}
`%if_not_in%` <- keep_if_not_in


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


now_utc <- function(length = 1L) {
    
    len <- as.integer(length[1L])
    stopifnot(base::length(len) == 1L || len >= 0L)
    
    if (len == 0L) {
        as.POSIXct(character(), tz = 'UTC')
    } else {
        now <- Sys.time()
        attr(now, "tzone") <- 'UTC'
        rep(now, len)
    }
}
