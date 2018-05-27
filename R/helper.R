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
        rlang::abort("key arguments must be valid strings")
    }
}


parse_call <- function(pos = 2L) {
    
    # the full call into the caller of this function
    parent_call <- match.call(
        definition = sys.function(-1L),
        call = sys.call(-1L),
        expand.dots = TRUE,
        envir = parent.frame(3L)
    )
    
    token <- parent_call[[pos]]
    if (is.symbol(token)) {
        if (as.character(token) == "." && pos == 2L) {
            # assume . from %>% ==> hack
            unmatched_fn_call <- parent_call[[3L]]
            parent <- parent.frame()
            unmatched_fn_call[[2L]] <- parent[["fn_call"]]
            parent[["fn_id"]] <- parent[["flow_options"]]
            parent[["flow_options"]] <- get_flow_options()
        } else {
            rlang::abort("The first argument must be a function call.")
        }
    } else if (is.language(token)) {
        # un-matched argument of the parent call
        # this is the as.is call to function (and its arguments) to be flow-ed
        unmatched_fn_call <- token
    } else {
        rlang::abort("Unrecognized argument type, expected a function call.")
    }
    
    fn <- eval(unmatched_fn_call[[1L]])
    if (!is.function(fn)) {
        format_fn_call <- paste(format(unmatched_fn_call), collapse = " ")
        rlang::abort(paste("Not a function call:", format_fn_call))
    }
    if (any(grepl("\\.Primitive", format(fn)))) {
        rlang::abort("Primitive functions not supported.")
    }
    
    # match.call for the function (and its arguments) to be flow-ed
    fn_call <- match.call(
        definition = fn,
        call = unmatched_fn_call,
        expand.dots = TRUE,
        envir = parent.frame(3L)
    )
    
    fn_call
}
