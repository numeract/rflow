#' @include r6eddy.R

# !diagnostics suppress=self, private


# R6Flow ----
R6Flow <- R6::R6Class(
    classname = 'R6Flow',
    public = list(
        fn = function() {},             # original function
        fn_name = character(),          # function name
        rf_fn = function() {},          # rflow (cached) function
        
        eddy = NULL,                    # link to R6Eddy obj were data is stored
        initialize = function() {}
    ),
    active = list(
        is_valid = function() {
            # TODO: read state from tbl
            TRUE
        }
    ),
    lock_objects = TRUE
)


# Initialize ----
R6Flow$set("public", "initialize", function(fn,
                                            fn_name,
                                            eddy) {
    
    if (eddy$exists_rflow(fn_name)) {
        stop("overwriting / re-flowing function not yet implemented")
        # TODO: common case: files were resourced and fn body changed
    }
    
    # init self$
    self$fn <- fn
    self$fn_name <- fn_name
    self$eddy <- eddy
    # rf_fn and fn have the same arguments
    formals(self$rf_fn) <- formals(args(fn))
    # the enclosing env of rn_fn is not changed to preserve access to self$
    # all args of this initialize function are transfered to new R6 obj
    
    # register itself in eddy
    eddy$add_rflow(fn_name, self)
    
    invisible(NULL)
}, overwrite = TRUE)


# rf_fn ----
R6Flow$set("public", "rf_fn", function(...) {
    # when called, the formals already match the original fn
    mc <- match.call()
    
    # R6Flow arguments are treated specially, identify them first
    rflow_args <- as.list(formals()) %>%
        purrr::keep(~ inherits(., 'R6Flow'))
    # follow memoise logic to separate supplied and default arguments
    # https://cran.r-project.org/doc/manuals/r-release/R-lang.html
    #     #Argument-evaluation
    # supplied arguments
    supplied_args <- as.list(mc)[-1] %>%
        discard_at(names(rflow_args))
    # default arguments that have not been supplied
    default_args <- as.list(formals()) %>%
        purrr::discard(~ identical(., quote(expr = ))) %>%
        discard_at(names(rflow_args)) %>%
        discard_at(names(supplied_args))
    
    # R6Flow args are eval for hashing by getting their hash (faster)
    # and for data by getting thier data
    if (self$eddy$is_reactive) {
        # TODO: is_reactive case
        stop("reactive eddies not yet implemented")
    } else {
        # non-reactive case, all rflows args must to be valid
        valid_rflow_args <- rflow_args %>%
            purrr::map_lgl(~ .$is_valid)
        if (any(!valid_rflow_args)) {
            stop("invalid input rflows")
            # TODO: better error message, which rflows?
        }
        # TODO: implement collect_hash as public function
        rflow_hash <- rflow_args %>%
            purrr::map(~ .$collect_hash(what = NULL))
    }
    
    # non-rflow / static args use the data for hashing
    # supplied args eval in the evaluation frame of the calling function
    # default args eval in the evaluation frame of the original function
    static_data <- c(
        lapply(supplied_args, eval, envir = parent.frame()),
        lapply(default_args, eval, envir = environment(self$fn))
    )
    
    in_hash <- self$eddy$digest(rflow_hash, static_data)
    # TODO: maybe cache body_hash? how can we tell if original fn body changed?
    body_hash <- self$eddy$digest(as.character(body(self$fn)))
    
    # check self if there is an out_hash associated with in_hash & body_hash
    # TODO: implement private get_state (also check cache exits in eddy)
    out_hash <- private$get_out_hash(in_hash, body_hash)
    if (!is.na(out_hash)) {
        out_data <- self$eddy$get_data(out_hash)
    } else {
        # not in cache, eval the function
        # replace the first arg to reconstruct the original fn match.call
        mc[[1L]] <- self$fn
        # we also need to replace R6Flow args with their data
        # avoid purrr to guarantee no unexpected effects since we have a call
        for (nm in names(rflow_args)) {
            rflow_obj <- rflow_args[[nm]] 
            mc[[nm]] <- rflow_obj$collect(what = NULL)
        }
        # need to preserve (and cache) the visibility of the return
        # eval envir must be the parent.frame of this funct, not of withVisible
        out_data <- withVisible(eval(mc, envir = parent.frame()))
        
        # we store the out_hash to avoid (re)hashing for rflow objects
        out_hash <- self$eddy$digest(out_data)
        # TODO: implement private add_state
        private$add_state(in_hash, body_hash, out_hash, make_current = TRUE)
        # store in cache
        self$eddy$put_data(out_hash, out_data)
    }
    
    # return the R6Flow obj instead of its data, use $collect() to get the data
    self
}, overwrite = TRUE)

