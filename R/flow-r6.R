# R6Flow class and methods


# !diagnostics suppress=self, public
.STATE_KEY = "_state"


# R6Flow ----
R6Flow <- R6::R6Class(
    classname = "R6Flow",
    public = list(
        # original fn to be rflow-ed 
        fn = NULL,
        # hash of fn's arguments (as defined) and its body (excludes comments)
        fn_key = character(),
        # original function name, mostly for debug purposes
        fn_name = character(),
        # from rflow_options
        excluded_arg = character(),
        eval_arg_fn = NULL,
        split_bare_list = TRUE,
        split_dataframe = FALSE,
        split_fn = NULL,
        # link to R6Eddy obj were data is stored
        eddy = NULL,
        
        # internal states
        state = NULL,
        state_index = 0L,   # 0 ==> NA
        # data frame to store elements of fn output
        output_state = NULL,
        
        # functions with the same arguments as fn
        # (functions declared as obj to bypass locking of R6 methods)
        calc_in_hash = NULL,
        rf_fn = NULL,
        exists_cache = NULL,
        delete_cache = NULL,
        
        initialize = function(fn,
                              fn_key,
                              fn_name,
                              rflow_options = get_rflow_options()) {},
        # state
        which_state = function(in_hash) {},
        get_state = function(index = NULL, required = FALSE) {},
        add_state = function(in_hash, 
                             out_hash, 
                             make_current = TRUE) {},
        update_state = function(index, 
                                in_hash, 
                                out_hash, 
                                make_current = TRUE) {},
        delete_state = function(index) {},
        add_output_state = function(out_hash, elem_name, elem_hash) {},
        # elements
        get_out_hash = function(name = NULL) {},
        get_element = function(name = NULL) {},
        # eval & collect
        evaluate = function() {},
        collect = function(name = NULL) {},
        
        # misc
        save = function() {},
        print = function() {},
        forget = function() {},
        is_valid_at_index = function(index = NULL) {},
        require_valid_at_index = function(index = NULL) {},
        is_evaluated_at_index = function(index = NULL) {},
        require_evaluated_at_index = function(index = NULL) {}
    ),
    active = list(
        is_valid = function() {},
        is_evaluated = function() {}
    )
)


# calc_in_hash ----
R6Flow$set("public", "calc_in_hash_default", function(rf_env = parent.frame()) {
    
    rflow_hash <- NULL
    if (length(rf_env$rflow_args) > 0L && !self$eddy$is_reactive) {
        # non-reactive case, all rflow args must to be valid & evaluated
        invalid_rflow_args <- !(
            purrr::map_lgl(rf_env$rflow_args, "is_valid") &
            purrr::map_lgl(rf_env$rflow_args, "is_evaluated")
        )
        if (any(invalid_rflow_args)) {
            invalid_names <- names(rf_env$rflow_args)[invalid_rflow_args]
            invalid_names <- paste(invalid_names, collapse = ", ")
            stop("Invalid/Unevaluated rflow args: ", invalid_names)
        }
        rflow_hash <- purrr::map(rf_env$rflow_args, "elem_hash")
    }
    if (length(rf_env$rflow_args) > 0L && self$eddy$is_reactive) {
        stop("reactive eddies not yet implemented")
    }
    
    # non-rflow / static args use their data for hashing
    static_data <- 
        rf_env$eval_args %>% 
        discard_at(names(rf_env$rflow_args))
    
    in_hash <- self$eddy$digest(c(rflow_hash, static_data))
    
    in_hash
}, overwrite = TRUE)


R6Flow$set("public", "calc_in_hash_custom", function(rf_env = parent.frame()) {
    
    # we already checked that fn and eval_arg_fn have the same formals
    # ignore excluded_arg
    match_call <- rf_env$match_call
    match_call[[1L]] <- self$eval_arg_fn
    excluded_arg <- self$excluded_arg %if_in% names(match_call)
    for (nm in excluded_arg) {
        match_call[[nm]] <- NULL
    }
    # called from $rf_fn, need to go back two frames when evaluating
    res <- eval(match_call, envir = parent.frame(n = 2))
    in_hash <- self$eddy$digest(res)
    
    in_hash
}, overwrite = TRUE)


# rf_fn ----
R6Flow$set("public", "rf_fn_default", function(...) {
    # when called, the formals already match the original fn
    match_call <- match.call()
    
    # follow memoise logic to separate supplied and default arguments
    # we are still at symbolic stage, have not evaluated them yet
    # https://cran.r-project.org/doc/manuals/r-release/R-lang.html
    #     #Argument-evaluation
    
    # supplied arguments
    supplied_args <- as.list(match_call)[-1]
    if (any(names(supplied_args) %in% self$excluded_arg)) {
        rlang::abort("Excluded arguments must not be supplied values")
    }
    
    # default arguments that have not been supplied
    # excluded arguments have defaults, drop them from hash / eval / saving
    default_args <-
        as.list(formals()) %>%
        purrr::discard(~ identical(., quote(expr = ))) %>%      # nolint
        discard_at(names(supplied_args)) %>% 
        discard_at(self$excluded_arg)
    # supplied args eval in the evaluation frame of the calling function
    # default args eval in the evaluation frame of the original function
    eval_args <- c(
        lapply(supplied_args, eval, envir = parent.frame()),
        lapply(default_args, eval, envir = environment(self$fn))
    )
    
    # Element/R6Flow args are treated differently
    # for consistency, transform R6Flow into an Element
    rflow_args <-
        eval_args %>%
        purrr::keep(~ inherits(., c("Element", "R6Flow"))) %>%
        purrr::map_if(
            .p = ~ inherits(., "R6Flow"),
            .f = ~ .$get_element(name = NULL)
        )
    
    in_hash <- self$calc_in_hash()
    
    # check if there is a state associated with in_hash
    found_state_idx <- self$which_state(in_hash)
    if (found_state_idx > 0L) {
        if (found_state_idx != self$state_index) {
            # no need to calc or get the data, just update the state index
            self$state_index <- found_state_idx
        }
        # if found_state_idx == self$state_index no processing is needed
    } else {
        # not in cache, prepare for lazy eval:
        # save enough args into cache to reconstruct the call later
        # TODO: reactivity: what if collect cannot provide data now?
        
        # we need to replace R6Flow args with their data
        for (nm in names(rflow_args)) {
            rflow_elem <- rflow_args[[nm]]
            eval_args[[nm]] <- rflow_elem$self$collect(rflow_elem$elem_name)
        }
        
        # store eval_args in cache
        self$eddy$add_data(self$fn_key, in_hash, eval_args)
        # adding a new state makes the new state current
        self$add_state(
            in_hash = in_hash, 
            out_hash = NA_character_, 
            make_current = TRUE
        )
        if (!self$save()) {
            rlang::abort("rflow cannot save its own state, aborting.")
        }
    }
    
    # return the R6Flow obj instead of its data, use $collect() to get data
    # we could have returned a structure similar to $element(), but
    # - $collect() would require $self$collect(), or
    # - adding a new collect function preserves its encl envir, takes memory
    self
}, overwrite = TRUE)


# initialize ----
R6Flow$set("public", "initialize", function(
        fn,
        fn_key,
        fn_name,
        rflow_options = get_rflow_options()
) {
    stopifnot(is.function(fn))
    require_keys(fn_key, fn_name)
    
    # init self$
    self$fn <- fn
    self$fn_key <- fn_key
    self$fn_name <- fn_name
    self$excluded_arg <- rflow_options$excluded_arg
    self$eval_arg_fn <- rflow_options$eval_arg_fn
    self$split_bare_list <- rflow_options$split_bare_list
    self$split_dataframe <- rflow_options$split_dataframe
    self$split_fn <- rflow_options$split_fn
    self$eddy <- rflow_options$eddy
    
    # new states / load states from cache
    if (self$eddy$has_rflow(fn_key)) {
        stop("rflow object with key ", fn_key, " already present in eddy")
    } else if (self$eddy$has_cache(fn_key)) {
        # data in cache but not loaded
        if (self$eddy$has_key(fn_key, .STATE_KEY)) {
            rflow_data <- self$eddy$get_data(fn_key, .STATE_KEY)
        } else {
            # cache present but no _state file ==> start with no states
            rflow_data <- NULL
        }
    } else {
        # no data in cache ==> start with no states
        rflow_data <- NULL
    }
    if (is.null(rflow_data)) {
        # state
        self$state <- tibble::data_frame(
            in_hash = character(),
            out_hash = character(),
            fn_key = character(),
            time_stamp = now_utc(0L)
        )
        self$state_index <- 0L
        # output state
        self$output_state <- tibble::data_frame(
            out_hash = character(),
            elem_name = character(),
            elem_hash = character()
        )
    } else {
        self$state <- rflow_data$state
        self$state_index <- 0L
        self$output_state <- rflow_data$output_state
    }
    
    # register itself in eddy (error if fn_key already present)
    self$eddy$add_rflow(fn_key, self)
    
    # calc_in_hash
    if (!is.null(self$eval_arg_fn)) {
        self$calc_in_hash <- self$calc_in_hash_custom
    } else {
        self$calc_in_hash <- self$calc_in_hash_default
    }
    
    # rf_fn: its enclosing envir is not changed to preserve access to self$
    self$rf_fn <- self$rf_fn_default
    formals(self$rf_fn) <- formals(args(fn))
    
    # # exists_cache
    # self$exists_cache <- self$exists_cache_default
    # formals(self$exists_cache) <- formals(args(fn))
    # 
    # # delete_cache
    # self$delete_cache <- self$delete_cache_default
    # formals(self$delete_cache) <- formals(args(fn))
    
    invisible(NULL)
}, overwrite = TRUE)


# which_state ----
R6Flow$set("public", "which_state", function(in_hash) {
    
    # since we just looking for the index, we do not check if the
    # eddy contains the cached values
    found_state_idx <- which(
        self$state$in_hash == in_hash &
        self$state$fn_key == self$fn_key
    )
    len <- length(found_state_idx)
    # there should not be duplicate states for the same fn_key and in_hash
    stopifnot(len <= 1L)
    
    if (len == 0L) {
        0L
    } else {
        found_state_idx
    }
}, overwrite = TRUE)


# get_state ----
R6Flow$set("public", "get_state", function(index = NULL) {
    
    if (is.null(index)) index <- self$state_index
    if (self$is_valid_at_index(index)) {
        self$state[index, , drop = FALSE]
    } else {
        # to preserve type, return a zero-row df if index not valid
        self$state[0L, , drop = FALSE]
    }
}, overwrite = TRUE)


# add_state ----
R6Flow$set("public", "add_state", function(in_hash,
                                           out_hash,
                                           make_current = TRUE) {
    
    self$state <-
        self$state %>%
        tibble::add_row(
            in_hash = in_hash,
            out_hash = out_hash,
            fn_key = self$fn_key,
            time_stamp = now_utc()
        )
    if (make_current) self$state_index <- nrow(self$state)
    
    # return TRUE if state can be found
    self$which_state(in_hash) == nrow(self$state)
}, overwrite = TRUE)


# update_state ----
R6Flow$set("public", "update_state", function(index,
                                              in_hash,
                                              out_hash,
                                              make_current = TRUE) {
    self$require_valid_at_index(index)
    
    self$state[index, ] <- list(
        in_hash = in_hash,
        out_hash = out_hash,
        fn_key = self$fn_key,
        time_stamp = now_utc()
    )
    if (make_current) self$state_index <- index
    
    # return TRUE if state can be found
    self$which_state(in_hash) == index
}, overwrite = TRUE)


# delete_state ----
R6Flow$set("public", "delete_state", function(index) {
    
    self$require_valid_at_index(index)
    
    in_hash <- self$state$in_hash[index]
    self$state <- self$state[-index, , drop = FALSE]
    if (self$state_index == index) {
        self$state_index <- 0L
    } else if (self$state_index > index) {
        self$state_index <- self$state_index - 1L
    }
    
    # return TRUE if state cannot be found
    self$which_state(in_hash) == 0L
}, overwrite = TRUE)


# add_output_state ----
R6Flow$set("public", "add_output_state", function(out_hash,
                                                  elem_name,
                                                  elem_hash) {
    
    output_state <- self$output_state
    found_state_idx <- which(
        output_state$out_hash == out_hash &
        output_state$elem_name == elem_name
    )
    len <- length(found_state_idx)
    stopifnot(len <= 1L)
    if (len == 1L) {
        output_state <- output_state[-found_state_idx, , drop = FALSE]
    }
    
    self$output_state <-
        output_state %>%
        tibble::add_row(
            out_hash = out_hash,
            elem_name = elem_name,
            elem_hash = elem_hash
        )
    
}, overwrite = TRUE)


# get_out_hash ----
R6Flow$set("public", "get_out_hash", function(name = NULL) {
    # invalid state OK; not yet evaluated OK
    
    if (!self$is_valid) {
        # invalid state, cannot talk about hashes
        return(NULL)
    }
    if (!self$is_evaluated) {
        # valid, but not yet evaluated
        return(NA_character_)
    }
    
    state <- self$get_state()
    if (is.null(name)) {
        # valid & evaluated - no element
        out_hash <- state$out_hash
    } else {
        # valid & evaluated - element requested
        found_state_idx <- which(
            self$output_state$out_hash == state$out_hash &
            self$output_state$elem_name == name
        )
        if (length(found_state_idx) != 1L) {
            rlang::abort(paste("Cannot find output element:", name))
        }
        out_hash <- self$output_state$elem_hash[found_state_idx]
    }
    
    out_hash
}, overwrite = TRUE)


# get_element ----
R6Flow$set("public", "get_element", function(name = NULL) {
    # invalid state OK; not yet evaluated OK
    
    elem_hash <- self$get_out_hash(name = name)
    if (is.null(elem_hash)) {
        # invalid state, cannot talk about hashes
        is_valid <- FALSE
        is_evaluated <- FALSE
    } else if (is.na(elem_hash)) {
        # valid, but not yet evaluated
        is_valid <- TRUE
        is_evaluated <- FALSE
    } else {
        is_valid <- TRUE
        is_evaluated <- TRUE
    }
    
    # class does not inherit R6Flow since it has a different structure
    rflow_elem <- list(
        self = self,
        is_valid = is_valid,
        is_evaluated = is_evaluated,
        elem_name = name,
        elem_hash = elem_hash
    )
    class(rflow_elem) <- c("Element", "list")
    
    rflow_elem
}, overwrite = TRUE)


# evaluate ----
R6Flow$set("public", "evaluate", function() {
    # we evalaute even if already evaluated
    # return TRUE/FALSE not an actual value since there might be elements
    
    if (!self$is_valid) {
        return(FALSE)
    }
    state <- self$get_state()
    
    if (!self$eddy$has_key(self$fn_key, state$in_hash)) {
        # cannot find input args ==> cannot evaluate
        return(FALSE)
    }
    eval_args <- self$eddy$get_data(self$fn_key, state$in_hash)
    
    # eval in .GlobalEnv to avoid name collisions
    out_data <- withVisible(do.call(
        what = self$fn, args = eval_args, envir = globalenv()))
    # we store the out_hash to avoid (re)hashing for rflow objects
    out_hash <- self$eddy$digest(out_data)
    
    # store out_data in cache
    eddy_add_OK <- self$eddy$add_data(self$fn_key, out_hash, out_data)
    if (!eddy_add_OK) {
        return(FALSE)
    }
    
    # update the current state
    update_OK <- self$update_state(
        index = self$state_index, 
        in_hash = state$in_hash, 
        out_hash = out_hash
    )
    if (!update_OK) {
        return(FALSE)
    }
    
    # split into elements by function
    split_using_fn <- !is.null(self$split_fn)
    split_bare_list <- 
        self$split_bare_list && rlang::is_bare_list(out_data$value)
    split_dataframe <- self$split_dataframe && is.data.frame(out_data$value)
    if (split_using_fn || split_bare_list || split_dataframe) {
        if (split_using_fn) {
            out_lst <- self$split_fn(out_data$value)
            if (!is.list(out_lst) || !rlang::is_named(out_lst)) {
                rlang::abort("split_fn() must return a named list")
            }
        } else if (split_bare_list) {
            out_lst <- out_data$value
        } else {
            out_lst <- as.list(out_data$value)
        }
        for (elem_name in names(out_lst)) {
            # reconstruct the withVisible list for each element
            vis_elem_lst <- list(
                value = out_lst[[elem_name]],
                visible = out_data$visible
            )
            elem_hash <- self$eddy$digest(vis_elem_lst)
            self$eddy$add_data(self$fn_key, elem_hash, vis_elem_lst)
            self$add_output_state(out_hash, elem_name, elem_hash)
        }
        
    }
    
    if (!self$save()) {
        rlang::abort("rflow cannot save its own state, aborting.")
    }
    
    TRUE
}, overwrite = TRUE)


# collect ----
R6Flow$set("public", "collect", function(name = NULL) {
    
    # require valid state since cannot return NULL (NULL can be a valid result)
    self$require_valid_at_index()
    
    # if not yet evaluated ==> trigger evaluation
    if (!self$is_evaluated) {
        if (!self$evaluate()) {
            rlang::abort("Cannot evaluate the current state.")
        }
    }
    
    out_hash <- self$get_out_hash(name = name)
    if (!self$eddy$has_key(self$fn_key, out_hash)) {
        rlang::abort(paste("Cached output is missing for out_hash", out_hash))
    }
    vis_out_lst <- self$eddy$get_data(self$fn_key, out_hash)
    
    # preserve the output visibility of the original result
    if (vis_out_lst$visible) {
        vis_out_lst$value
    } else {
        invisible(vis_out_lst$value)
    }
}, overwrite = TRUE)


# save ----
R6Flow$set("public", "save", function() {
    
    rflow_data <- list(
        fn_key = self$fn_key,
        fn_name = self$fn_name,
        state = self$state,
        state_index = self$state_index,
        output_state = self$output_state
    )
    
    # returns TRUE if cache for fn_key contains the key .STATE_KEY
    self$eddy$add_data(self$fn_key, .STATE_KEY, rflow_data)
}, overwrite = TRUE)


# print ----
# nocov start
R6Flow$set("public", "print", function() {
    
    emph_obj <- paste0("<", crayon::italic(class(self)[[1L]]), ">")
    cat(emph_obj, "for function", crayon::bold(self$fn_name), "\n",
        " - number of states:", nrow(self$state), "\n",
        " - current state index:", self$state_index, "\n",
        " - is_valid:", self$is_valid, "\n",
        " - is_evalauted:", self$is_evaluated, "\n"
    )
    print(self$state)
    
    invisible(self)
}, overwrite = TRUE)
# nocov end


# forget ----
R6Flow$set("public", "forget", function() {
    
    # clear states
    self$state <- self$state[0L, , drop = FALSE]
    self$state_index <- 0L
    self$output_state <- self$output_state[0L, , drop = FALSE]
    
    # clear cache
    self$eddy$forget_rflow(self$fn_key)
    
    invisible(self)
}, overwrite = TRUE)


# is_valid_at_index ----
R6Flow$set("public", "is_valid_at_index", function(index = NULL) {
    
    if (is.null(index)) index <- self$state_index
    
    !is.na(index) && 
            rlang::is_scalar_integerish(index) &&
            index >= 1L && 
            index <= nrow(self$state)
}, overwrite = TRUE)


# require_valid_at_index ----
R6Flow$set("public", "require_valid_at_index", function(index = NULL) {
    
    if (is.null(index)) index <- self$state_index
    if (!self$is_valid_at_index(index)) {
        if (identical(index, self$state_index)) {
            rlang::abort(paste("Invalid current state, index =", index))
        } else {
            rlang::abort(paste("Invalid state, index =", index))
        }
    }
}, overwrite = TRUE)


# is_evaluated_at_index ----
R6Flow$set("public", "is_evaluated_at_index", function(index = NULL) {
    
    if (is.null(index)) index <- self$state_index
    self$is_valid_at_index(index) && !is.na(self$state$out_hash[index])
}, overwrite = TRUE)


# require_evaluated_at_index ----
R6Flow$set("public", "require_evaluated_at_index", function(index = NULL) {
    
    if (is.null(index)) index <- self$state_index
    if (!self$is_evaluated_at_index(index)) {
        if (identical(index, self$state_index)) {
            rlang::abort(paste("Unevaluated current state, index =", index))
        } else {
            rlang::abort(paste("Unevaluated state, index =", index))
        }
    }
}, overwrite = TRUE)


# is_valid ----
R6Flow$set("active", "is_valid", function() {
    # so far, we look only athe index, but this might change
    
    self$is_valid_at_index(self$state_index)
}, overwrite = TRUE)


# is_evaluated ----
R6Flow$set("active", "is_evaluated", function() {
    
    index <- self$state_index
    self$is_valid_at_index(index) && !is.na(self$state$out_hash[index])
}, overwrite = TRUE)



# print.Element ----
# nocov start
print.Element <- function(x, ...) {
    
    if (length(list(...)) > 0L) warning("all other arguments ignored")
    
    emph_obj1 <- paste0("<", crayon::italic("Element"), ">")
    emph_obj2 <- paste0("<", crayon::italic(class(x$self)[[1L]]), ">")
    fn_name <- crayon::bold(x$self$fn_name)
    cat(emph_obj1, "of", emph_obj2, "for function", fn_name, "\n",
        " - elem_name:", x$elem_name %||% "<full result>", "\n",
        " - elem_hash:", x$elem_hash, "\n",
        " - is_valid:", x$self$is_valid, "\n",
        " - is_evalauted:", x$self$is_evaluated, "\n"
    )
    
    invisible(x)
}
# nocov end
