# R6Flow class and methods


# !diagnostics suppress=self, public
.STATE_KEY = "_state"


# R6Flow ----
R6Flow <- R6::R6Class(
    classname = "R6Flow",
    public = list(
        # original fn to be flow-ed 
        fn = NULL,
        # hash of fn's arguments (as defined) and its body (excludes comments)
        fn_key = character(),
        # original function name, mostly for debug purposes
        fn_name = character(),
        # from flow_options
        excluded_arg = character(),
        eval_arg_fn = NULL,
        split_bare_list = TRUE,
        split_dataframe = FALSE,
        split_fn = NULL,
        # link to R6Eddy obj were data is stored
        eddy = NULL,
        # internal states
        state = NULL,
        state_index = 0L,   # 0 ==> missing (do not use NA)
        # data frame to store elements of fn output
        state_output = NULL,
        # an local cache used for lazy calc
        state_env = NULL,
        # functions with the same arguments as fn
        # (functions declared as obj to bypass locking of R6 methods)
        calc_in_hash = NULL,
        rf_fn = NULL,
        # init
        initialize = function(fn,
                              fn_key,
                              fn_name,
                              flow_options = get_flow_options()) {},
        # state
        which_state = function(in_hash) {},
        get_state = function(index = NULL) {},
        add_state = function(in_hash,
                             out_hash,
                             elem_args,
                             make_current = TRUE) {},
        update_state = function(index,
                                in_hash,
                                out_hash,
                                out_data,
                                make_current = TRUE) {},
        forget_state = function(index) {},
        delete_state = function(index) {},
        add_state_output = function(out_hash,
                                    elem_name,
                                    elem_hash,
                                    elem_data) {},
        delete_state_output = function(out_hash) {},
        # elements
        get_out_hash = function(name = NULL) {},
        get_element = function(name = NULL) {},
        # eval & collect
        compute = function() {},
        collect = function(name = NULL) {},
        # misc
        check_all = function() {},
        forget_all = function() {},
        save = function() {},
        print = function() {},
        is_good_index = function(index = NULL) {},
        require_good_index = function(index = NULL) {},
        is_valid_at_index = function(index = NULL) {},
        require_valid_at_index = function(index = NULL) {}
    ),
    active = list(
        is_current = function() {},
        is_valid = function() {}
    )
)


# calc_in_hash ----
R6Flow$set("public", "calc_in_hash_default", function(rf_env = parent.frame()) {
    # goal: replace Element obj with their $elem_hash, then hash the list
    
    elem_args <- rf_env$elem_args
    data_hash_args <-
        elem_args %>% 
        discard_at(self$excluded_arg) %>%
        purrr::map_if(
            .p = ~ inherits(., "Element"),
            .f = function(x) {
                x$self$require_good_index()
                state <- x$self$get_state()
                # we must return some data even if elem is not yet computed
                # uniquely identify fn, its input state, and which elem
                list(
                    fn_key = state$fn_key,
                    in_hash = state$in_hash,
                    elem_name = x$elem_name
                )
            }
        )
    in_hash <- self$eddy$digest(data_hash_args)
    
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
    # we are still at symbolic stage, have not yet evaluated them
    # https://cran.r-project.org/doc/manuals/r-release/R-lang.html
    #     #Argument-evaluation
    
    # supplied arguments, in the order received, might not be named
    supplied_args <- as.list(match_call)[-1]
    
    # default arguments that have not been supplied
    # excluded arguments have defaults, drop them from hash / eval / saving
    default_args <-
        as.list(formals()) %>%
        purrr::discard(~ identical(., quote(expr = ))) %>%      # nolint
        discard_at(names(supplied_args))
    # supplied args eval in the evaluation frame of the calling function
    # default args eval in the evaluation frame of the original function
    eval_args <- c(
        lapply(supplied_args, eval, envir = parent.frame()),
        lapply(default_args, eval, envir = environment(self$fn))
    )
    
    # for consistency, transform any R6Flow into its Element
    elem_args <-
        eval_args %>%
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
        # not in cache, prepare for lazy eval: save args to be called later
        self$add_state(
            in_hash = in_hash, 
            out_hash = NA_character_,
            elem_args = elem_args,
            make_current = TRUE
        )
        self$save()
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
        flow_options = get_flow_options()
) {
    stopifnot(is.function(fn))
    require_keys(fn_key, fn_name)
    
    # register itself in eddy (error if fn_key already present)
    self$eddy <- flow_options$eddy
    if (!self$eddy$add_flow(fn_key, self)) {
        rlang::abort(paste("Failed to register flow:", fn_key))
    }
    
    # init self$
    self$fn <- fn
    self$fn_key <- fn_key
    self$fn_name <- fn_name
    self$excluded_arg <- flow_options$excluded_arg
    self$eval_arg_fn <- flow_options$eval_arg_fn
    self$split_bare_list <- flow_options$split_bare_list
    self$split_dataframe <- flow_options$split_dataframe
    self$split_fn <- flow_options$split_fn
    
    # 'group' in cache; does it have state data?
    if (self$eddy$has_key(fn_key, .STATE_KEY)) {
        flow_data <- self$eddy$get_data(fn_key, .STATE_KEY)
        self$state <- flow_data$state
        self$state_output <- flow_data$state_output
    } else {
        # state
        self$state <- tibble::data_frame(
            fn_key = character(),
            in_hash = character(),
            out_hash = character(),
            time_stamp = now_utc(0L)
        )
        # output state
        self$state_output <- tibble::data_frame(
            out_hash = character(),
            elem_name = character(),
            elem_hash = character()
        )
    }
    self$state_index <- 0L
    self$check_all()
    self$state_env <- new.env(hash = TRUE, parent = emptyenv())
    
    # calc_in_hash
    if (!is.null(self$eval_arg_fn)) {
        self$calc_in_hash <- self$calc_in_hash_custom
    } else {
        self$calc_in_hash <- self$calc_in_hash_default
    }
    
    # rf_fn: its enclosing envir is not changed to preserve access to self$
    self$rf_fn <- self$rf_fn_default
    formals(self$rf_fn) <- formals(args(fn))
    
    invisible(NULL)
}, overwrite = TRUE)


# which_state ----
R6Flow$set("public", "which_state", function(in_hash) {
    
    # since we just looking for the index, we do not check if the
    # eddy contains the cached values
    found_state_idx <- which(
        self$state$fn_key == self$fn_key &
        self$state$in_hash == in_hash
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
    if (self$is_good_index(index)) {
        state <- self$state[index, , drop = FALSE]
    } else {
        # to preserve type, return a zero-row df if index not valid
        state <- self$state[0L, , drop = FALSE]
    }
    
    state
}, overwrite = TRUE)


# add_state ----
R6Flow$set("public", "add_state", function(in_hash,
                                           out_hash,
                                           elem_args,
                                           make_current = TRUE) {
    require_keys(in_hash)
    
    self$state <-
        self$state %>%
        tibble::add_row(
            fn_key = self$fn_key,
            in_hash = in_hash,
            out_hash = out_hash,
            time_stamp = now_utc()
        )
    self$state_env[[in_hash]] <- elem_args
    if (make_current) self$state_index <- nrow(self$state)
    
    # return TRUE if state can be found
    self$which_state(in_hash) == nrow(self$state)
}, overwrite = TRUE)


# update_state ----
R6Flow$set("public", "update_state", function(index,
                                              in_hash,
                                              out_hash,
                                              out_data,
                                              make_current = TRUE) {
    self$require_good_index(index)
    require_keys(in_hash, out_hash)
    
    # store out_data in cache
    eddy_add_OK <- self$eddy$add_data(self$fn_key, out_hash, out_data)
    if (!eddy_add_OK) return(FALSE)
    
    self$state[index, ] <- list(
        fn_key = self$fn_key,
        in_hash = in_hash,
        out_hash = out_hash,
        time_stamp = now_utc()
    )
    if (!is.na(out_hash) && 
        base::exists(in_hash, where = self$state_env, inherits = FALSE)
    ) {
        # the main reason to update is to add out_hash ==> args no longer needed
        base::rm(list = in_hash, pos = self$state_env)
    }
    if (make_current) self$state_index <- index
    
    # return TRUE if state can be found
    self$which_state(in_hash) == index
}, overwrite = TRUE)


# forget_state ----
R6Flow$set("public", "forget_state", function(index) {
    
    self$require_good_index(index)
    old_state <- self$state[index, , drop = FALSE]
    
    # update state
    self$state[index, ] <- list(
        fn_key = self$fn_key,
        in_hash = old_state$in_hash,
        out_hash = NA_character_,
        time_stamp = now_utc()
    )
    # delete data from cache
    # only if not present somewhere else (same output for the same input)
    delete_key <- old_state$out_hash %if_not_in% self$out_hash
    if (is_key(delete_key)) {
        self$eddy$delete_data(self$fn_key, delete_key)
    }
    # delete data from state_output
    self$delete_state_output(old_state$out_hash)
    
    # return TRUE if state can be found
    self$which_state(old_state$in_hash) == index
}, overwrite = TRUE)


# delete_state ----
R6Flow$set("public", "delete_state", function(index) {
    
    self$require_good_index(index)
    old_state <- self$state[index, , drop = FALSE]
    
    self$forget_state(index)
    # delete state
    self$state <- self$state[-index, , drop = FALSE]
    # update index
    self$state_index <- self$which_state(old_state$in_hash)
    
    # return TRUE if state cannot be found
    self$which_state(old_state$in_hash) == 0L
}, overwrite = TRUE)


# add_state_output ----
R6Flow$set("public", "add_state_output", function(out_hash,
                                                  elem_name,
                                                  elem_hash,
                                                  elem_data) {
    require_keys(out_hash, elem_name, elem_hash)
    
    # store elem data in cache
    self$eddy$add_data(self$fn_key, elem_hash, elem_data)
    
    state_output <- self$state_output
    found_state_idx <- which(
        state_output$out_hash == out_hash &
        state_output$elem_name == elem_name
    )
    len <- length(found_state_idx)
    stopifnot(len <= 1L)
    if (len == 1L) {
        state_output <- state_output[-found_state_idx, , drop = FALSE]
    }
    
    self$state_output <-
        state_output %>%
        tibble::add_row(
            out_hash = out_hash,
            elem_name = elem_name,
            elem_hash = elem_hash
        )
    
    invisible(NULL)
}, overwrite = TRUE)


# delete_state_output ----
R6Flow$set("public", "delete_state_output", function(out_hash) {
    
    if (length(out_hash) == 0L) return(invisible(NULL))
    
    keep_lgl <- self$state_output$out_hash != out_hash
    old_state_output <- self$state_output[!keep_lgl, , drop = FALSE]
    
    # delete form state_output
    self$state_output <- self$state_output[keep_lgl, , drop = FALSE]
    # delete data from cache
    # only if not present somewhere else (same output for the same input)
    delete_keys <- 
        old_state_output$elem_hash %if_not_in% self$state_output$elem_hash
    deleted_keys <- 
        delete_keys %>%
        rlang::set_names() %>%
        purrr::map_lgl(~ self$eddy$delete_data(fn_key, .))
    if (any(!deleted_keys)) {
        txt <- paste(names(deleted_keys[!deleted_keys]), collapse = ", ")
        rlang::warn(paste("flow", self$fn_key, "- cannot delete keys:", txt))
    }
    
    invisible(NULL)
}, overwrite = TRUE)


# get_out_hash ----
R6Flow$set("public", "get_out_hash", function(name = NULL) {
    # invalid state OK; not yet computed OK
    
    if (!self$is_current) {
        # invalid state, cannot talk about hashes
        return(NULL)
    }
    if (!self$is_valid) {
        # valid, but not yet computed
        return(NA_character_)
    }
    
    state <- self$get_state()
    if (is.null(name)) {
        # valid & computed - no element
        out_hash <- state$out_hash
    } else {
        # valid & computed - element requested
        found_state_idx <- which(
            self$state_output$out_hash == state$out_hash &
            self$state_output$elem_name == name
        )
        if (length(found_state_idx) != 1L) {
            rlang::abort(paste("Cannot find output element:", name))
        }
        out_hash <- self$state_output$elem_hash[found_state_idx]
    }
    
    out_hash
}, overwrite = TRUE)


# get_element ----
R6Flow$set("public", "get_element", function(name = NULL) {
    # invalid state OK; not yet computed OK
    
    elem_hash <- self$get_out_hash(name = name)
    if (is.null(elem_hash)) {
        # invalid state, cannot talk about hashes
        is_current <- FALSE
        is_valid <- FALSE
    } else if (is.na(elem_hash)) {
        # valid, but not yet computed
        is_current <- TRUE
        is_valid <- FALSE
    } else {
        is_current <- TRUE
        is_valid <- TRUE
    }
    
    # class does not inherit R6Flow since it has a different structure
    flow_elem <- list(
        self = self,
        is_current = is_current,
        is_valid = is_valid,
        elem_name = name,
        elem_hash = elem_hash
    )
    class(flow_elem) <- c("Element", "list")
    
    flow_elem
}, overwrite = TRUE)


# compute ----
R6Flow$set("public", "compute", function() {
    # do not compute if already computed
    # return TRUE/FALSE not an actual value since there might be elements
    
    if (self$is_valid) return(TRUE)
    if (!self$is_current) return(FALSE)
    state <- self$get_state()
    
    if (!base::exists(
        state$in_hash, where = self$state_env, inherits = FALSE)
    ) {
        # cannot find input args ==> cannot compute
        return(FALSE)
    }
    elem_args <- self$state_env[[state$in_hash]]
    
    # need to collect output of Elements (if any)
    eval_args <-
        elem_args %>%
        purrr::map_if(
            .p = ~ inherits(., "Element"),
            .f = function(x) {
                x$self$collect(x$elem_name)
            }
        )
    
    # eval in .GlobalEnv to avoid name collisions
    out_data <- withVisible(do.call(
        what = self$fn, args = eval_args, envir = globalenv()))
    # we store the out_hash to avoid (re)hashing for flow objects
    out_hash <- self$eddy$digest(out_data)
    
    # update the current state
    update_OK <- self$update_state(
        index = self$state_index, 
        in_hash = state$in_hash, 
        out_hash = out_hash,
        out_data = out_data
    )
    if (!update_OK) return(FALSE)
    
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
            self$add_state_output(out_hash, elem_name, elem_hash, vis_elem_lst)
        }
    }
    
    self$save()
}, overwrite = TRUE)


# collect ----
R6Flow$set("public", "collect", function(name = NULL) {
    
    # require valid state since cannot return NULL (NULL can be a valid result)
    self$require_good_index()
    
    # if not yet computed ==> trigger compute
    if (!self$compute()) {
        rlang::abort("Cannot compute the current state.")
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


# check_all ----
R6Flow$set("public", "check_all", function() {
    
    # save current index / in_hash
    if (self$is_current) {
        in_hash <- self$state$in_hash[self$state_index]
    } else {
        in_hash <- NA_character_
    }
    
    keys <- self$eddy$list_keys(self$fn_key)
    changed <- FALSE
    
    # state: forget states missing from cache
    changed_lgl <- !(self$state$out_hash %in% keys)
    changed <- changed || any(changed_lgl)
    self$state$out_hash[changed_lgl] <- NA_character_
    
    # output state: forget implies deleting rows
    keep_rows_lgl <- (self$state_output$elem_hash %in% keys) &
        (self$state_output$out_hash %in% self$state$out_hash)
    changed <- changed || any(!keep_rows_lgl)
    self$state_output <- self$state_output[keep_rows_lgl, , drop = FALSE]
    
    # delete cache of missing states
    delete_keys <- keys %if_not_in% c(
        self$state$out_hash, self$state_output$elem_hash)
    changed <- changed || (length(delete_keys) > 0L)
    deleted_keys <- 
        delete_keys %>%
        rlang::set_names() %>%
        purrr::map_lgl(~ self$eddy$delete_data(fn_key, .))
    if (any(!deleted_keys)) {
        txt <- paste(names(deleted_keys[!deleted_keys]), collapse = ", ")
        rlang::warn(paste("flow", self$fn_key, "- cannot delete keys:", txt))
    }
    
    if (changed) {
        # update index
        self$state_index <- self$which_state(in_hash)
        self$save()
    }
    
    changed
}, overwrite = TRUE)


# forget_all ----
R6Flow$set("public", "forget_all", function() {
    
    # clear all states
    self$state <- self$state[0L, , drop = FALSE]
    self$state_index <- 0L
    self$state_output <- self$state_output[0L, , drop = FALSE]
    
    # clear cache
    self$eddy$forget_flow(self$fn_key)
    
    self$save()
}, overwrite = TRUE)


# save ----
R6Flow$set("public", "save", function() {
    
    flow_data <- list(
        fn_key = self$fn_key,
        fn_name = self$fn_name,
        state = self$state,
        state_index = self$state_index,
        state_output = self$state_output
    )
    
    # returns TRUE if cache for fn_key contains the key .STATE_KEY
    save_ok <- self$eddy$add_data(self$fn_key, .STATE_KEY, flow_data)
    if (!save_ok) {
        rlang::warn("flow cannot save its own state")
    }
    save_ok
}, overwrite = TRUE)


# print ----
# nocov start
R6Flow$set("public", "print", function() {
    
    emph_obj <- paste0("<", crayon::italic(class(self)[[1L]]), ">")
    cat(emph_obj, "for function", crayon::bold(self$fn_name), "\n",
        " - number of states:", nrow(self$state), "\n",
        " - current state index:", self$state_index, "\n",
        " - is_current:", self$is_current, "\n",
        " - is_valid:", self$is_valid, "\n"
    )
    print(self$state)
    
    invisible(self)
}, overwrite = TRUE)


print.Element <- function(x, ...) {
    
    if (length(list(...)) > 0L) warning("all other arguments ignored")
    
    emph_obj1 <- paste0("<", crayon::italic("Element"), ">")
    emph_obj2 <- paste0("<", crayon::italic(class(x$self)[[1L]]), ">")
    fn_name <- crayon::bold(x$self$fn_name)
    cat(emph_obj1, "of", emph_obj2, "for function", fn_name, "\n",
        " - elem_name:", x$elem_name %||% "<full result>", "\n",
        " - elem_hash:", x$elem_hash, "\n",
        " - is_current:", x$self$is_current, "\n",
        " - is_valid:", x$self$is_valid, "\n"
    )
    
    invisible(x)
}
# nocov end


# is_good_index ----
R6Flow$set("public", "is_good_index", function(index = NULL) {
    
    if (is.null(index)) index <- self$state_index
    
    !is.na(index) && 
            rlang::is_scalar_integerish(index) &&
            index >= 1L && 
            index <= nrow(self$state)
}, overwrite = TRUE)


# require_good_index ----
R6Flow$set("public", "require_good_index", function(index = NULL) {
    
    if (is.null(index)) index <- self$state_index
    if (!self$is_good_index(index)) {
        if (identical(index, self$state_index)) {
            rlang::abort(paste("Invalid current state, index =", index))
        } else {
            rlang::abort(paste("Invalid state, index =", index))
        }
    }
}, overwrite = TRUE)


# is_valid_at_index ----
R6Flow$set("public", "is_valid_at_index", function(index = NULL) {
    
    if (is.null(index)) index <- self$state_index
    self$is_good_index(index) && !is.na(self$state$out_hash[index])
}, overwrite = TRUE)


# require_valid_at_index ----
R6Flow$set("public", "require_valid_at_index", function(index = NULL) {
    
    if (is.null(index)) index <- self$state_index
    if (!self$is_valid_at_index(index)) {
        if (identical(index, self$state_index)) {
            rlang::abort(paste("Not-computed current state, index =", index))
        } else {
            rlang::abort(paste("Not-computed state, index =", index))
        }
    }
}, overwrite = TRUE)


# is_current ----
R6Flow$set("active", "is_current", function() {
    # so far, we look only at the index, but this might change
    
    self$is_good_index(self$state_index)
}, overwrite = TRUE)


# is_valid ----
R6Flow$set("active", "is_valid", function() {
    
    index <- self$state_index
    self$is_good_index(index) && !is.na(self$state$out_hash[index])
}, overwrite = TRUE)
