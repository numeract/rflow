# helper functions not present in nmisc.R


# !diagnostics suppress=.,


is_key <- function(x) {
    
    if (typeof(x) != "character") return(FALSE)
    if (length(x) != 1L) return(FALSE)
    if (x %in% c(NA_character_, "")) return(FALSE)
    
    TRUE
}


require_keys <- function(...) {
    
    key_lgl <- purrr::map_lgl(list(...), ~ is_key(.))
    
    if (sum(!key_lgl) > 0L) {
        # stop("key arguments must be valid strings", call. = FALSE)
        rlang::abort("key arguments must be valid strings")
    }
}
