# R6Flow class and methods


# !diagnostics suppress=self, public


# R6Flow ----
R6Flow <- R6::R6Class(
    classname = "R6Flow",
    public = list(
        # original fn (declared as obj to bypass locking of R6 methods)
        fn = NULL,
        # hash of fn's defined arguments and body
        fn_key = character(),
        # original function name, mostly for debug
        fn_name = character(),
        # position or name of the source argument to watched
        fn_source_arg = NULL,
        # callback to create a custom input hash, e.g. a subset of rflow output
        hash_input_fn = NULL,
        # callback to split the output into a list to hash its elem separately
        split_output_fn = NULL,
        # link to R6Eddy obj were data is stored
        eddy = NULL,
        
        # input hash function (declared as obj to bypass locking of R6 methods)
        calc_in_hash = NULL,
        # cached function (declared as obj to bypass locking of R6 methods)
        rf_fn = NULL,
        
        initialize = function(fn,
                              fn_key = NULL,
                              fn_name = "missing",
                              hash_input_fn = NULL,
                              split_output_fn = NULL,
                              eddy = get_default_eddy()) {},
        save = function() {},
        print = function() {},
        get_element = function(name = NULL) {},
        collect_data = function(name = NULL) {},
        collect_hash = function(name = NULL) {},
        # internal states
        state = NULL,
        state_index = NA_integer_,
        # data frame to store elements of fn output
        output_state = NULL,
        
        # data frame to store hashes and current state
        find_state_index = function(in_hash) {},
        get_state = function(index = NULL) {},
        check_state = function(index = NULL) {},
        add_state = function(in_hash, 
                             out_hash, 
                             make_current = TRUE) {},
        update_state = function(index, 
                                in_hash, 
                                out_hash, 
                                make_current = TRUE) {},
        get_out_hash = function(in_hash, fn_key) {},
        add_output_state = function(out_hash, elem_name, elem_hash) {}
    ),
    active = list(
        is_valid = function() {}
    ),
    lock_objects = TRUE
)


# calc_in_hash ----
R6Flow$set("public", "calc_in_hash_default", function(rf_env = parent.frame()) {
    
    rflow_hash <- NULL
    if (length(rf_env$rflow_args) > 0L && !self$eddy$is_reactive) {
        # non-reactive case, all rflow args must to be valid
        invalid_rflow_args <- !purrr::map_lgl(rf_env$rflow_args, "is_valid")
        if (any(invalid_rflow_args)) {
            invalid_names <- names(rf_env$rflow_args)[invalid_rflow_args]
            invalid_names <- paste(invalid_names, collapse = ", ")
            stop("Invalid input rflow args: ", invalid_names)
        }
        rflow_hash <- purrr::map(rf_env$rflow_args, "elem_hash")
    }
    if (length(rf_env$rflow_args) > 0L && self$eddy$is_reactive) {
        stop("reactive eddies not yet implemented")
    }
    
    # non-rflow / static args use their data for hashing
    static_data <- rf_env$eval_args %>%
        discard_at(names(rf_env$rflow_args))
    
    in_hash <- self$eddy$digest(c(rflow_hash, static_data))
    
    in_hash
}, overwrite = TRUE)


R6Flow$set("public", "calc_in_hash_custom", function(rf_env = parent.frame()) {
    
    # we already checked that fn and hash_input_fn have the same formals
    match_call <- rf_env$match_call
    match_call[[1L]] <- self$hash_input_fn
    res <- eval(match_call, envir = parent.frame(n = 2))
    in_hash <- self$eddy$digest(res)
    
    in_hash
}, overwrite = TRUE)


R6Flow$set("public", "calc_in_hash_source", function(rf_env = parent.frame()) {
    
    file_path <- rf_env$eval_args[[self$fn_source_arg]]
    stopifnot(rlang::is_scalar_character(file_path) || !is.na(file_path))
    
    if (!file.exists(file_path)) {
        in_hash <- self$eddy$digest(object = NULL)
    } else {
        in_hash <- self$eddy$digest(object = file_path, file = TRUE)
    }
    
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
    # default arguments that have not been supplied
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
    
    # R6FlowElement/R6Flow args are treated different
    # for consistency, transform R6Flow into a R6FlowElement
    rflow_args <-
        eval_args %>%
        purrr::keep(~ inherits(., c("R6FlowElement", "R6Flow"))) %>%
        purrr::map_if(
            .p = ~ inherits(., "R6Flow"),
            .f = ~ .$get_element(name = NULL)
        )
    
    in_hash <- self$calc_in_hash()
    
    # check if there is a state associated with in_hash
    found_state_idx <- self$find_state_index(in_hash)
    if (found_state_idx > 0L) {
        if (found_state_idx != self$state_index) {
            # no need to get the data, but need to update the state index
            self$state_index <- found_state_idx
            # TODO: here just the index is changed, not a good idea to save all
            self$save()
        }
        # if found_state_idx == self$state_index no processing is needed
    } else {
        # not in cache, eval the function
        # replace the first arg to reconstruct the original fn match.call
        match_call[[1L]] <- self$fn
        # we also need to replace R6Flow args with their data
        # avoid purrr to guarantee no unexpected effects since we have a call
        for (nm in names(rflow_args)) {
            rflow_elem <- rflow_args[[nm]]
            match_call[[nm]] <- rflow_elem$self$collect_data(
                name = rflow_elem$name)
        }
        
        # need to preserve (and cache) the visibility of the return
        # eval envir must be the parent.frame of this func, not of withVisible
        out_data <- withVisible(eval(match_call, envir = parent.frame()))
        # we store the out_hash to avoid (re)hashing for rflow objects
        out_hash <- self$eddy$digest(out_data)
        
        # adding a new state makes the new state current
        self$add_state(in_hash, out_hash)
        # store out_data in cache
        self$eddy$add_data(out_hash, out_data, self$fn_key)
        
        # split the out_data and store its elements
        if (!is.null(self$split_output_fn)) {
            vis_out_lst <- withVisible(self$split_output_fn(out_data$value))
            out_lst <- vis_out_lst$value
            if (!is.list(out_lst))
                stop("split_output_fn() must return a named list")
            out_nms <- names(out_lst) %if_not_in% ""
            if (length(out_nms) != length(out_lst))
                stop("split_output_fn() must provide a name for each element")
            for (elem_name in out_nms) {
                # reconstruct the withVisible list for each element
                elem_data <- list(
                    value = out_lst[[elem_name]],
                    visible = vis_out_lst$visible
                )
                elem_hash <- self$eddy$digest(elem_data)
                self$add_output_state(out_hash, elem_name, elem_hash)
                self$eddy$add_data(elem_hash, elem_data, self$fn_key)
            }
        }
        
        self$save()
    }
    
    # return the R6Flow obj instead of its data, use $collect_data() to get data
    # we could have returned a structure similar to $element(), but
    # - $collect_data() would require $self$collect_data(), or
    # - adding a new collect function preserves its encl envir, takes memory
    self
}, overwrite = TRUE)


R6Flow$set("public", "rf_fn_sink", function(...) {
    # follow rf_fn_default, with some changes
    
    match_call <- match.call()
    
    supplied_args <- as.list(match_call)[-1]
    default_args <-
        as.list(formals()) %>%
        purrr::discard(~ identical(., quote(expr = ))) %>%      # nolint
        discard_at(names(supplied_args))
    eval_args <- c(
        lapply(supplied_args, eval, envir = parent.frame()),
        lapply(default_args, eval, envir = environment(self$fn))
    )
    
    rflow_args <-
        eval_args %>%
        purrr::keep(~ inherits(., c("R6FlowElement", "R6Flow"))) %>%
        purrr::map_if(
            .p = ~ inherits(., "R6Flow"),
            .f = ~ .$get_element(name = NULL)
        )
    
    in_hash <- self$calc_in_hash()
    
    found_state_idx <- self$find_state_index(in_hash)
    if (found_state_idx == 0L || found_state_idx != self$state_index) {
        # either a new state or state changed => call fn for its side effects
        
        match_call[[1L]] <- self$fn
        for (nm in names(rflow_args)) {
            rflow_elem <- rflow_args[[nm]]
            match_call[[nm]] <- rflow_elem$self$collect_data(
                name = rflow_elem$name)
        }
        out_data <- withVisible(eval(match_call, envir = parent.frame()))
        out_hash <- self$eddy$digest(out_data)
        
        if (found_state_idx == 0L) {
            # adding a new state makes the new state current
            self$add_state(in_hash, out_hash)
        } else {
            # update state & make it current
            self$update_state(found_state_idx, in_hash, out_hash)
        }
        # store out_data in cache
        self$eddy$add_data(out_hash, out_data, self$fn_key)
        
        # skip split_output_fn, not defined for sinks
        
        self$save()
    }
    
    # return itself to allow pipe after
    self
}, overwrite = TRUE)


# initialize ----
R6Flow$set("public", "initialize", function(fn,
                                            fn_key = NULL,
                                            fn_name = "missing",
                                            fn_source_arg = NULL,
                                            hash_input_fn = NULL,
                                            split_output_fn = NULL,
                                            eddy = get_default_eddy()) {
    
    if (is.null(fn_key)) fn_key <- make_fn_key(fn, eddy)
    
    found <- eddy$find_rflow(fn_key)
    if (found == "memory")
        stop("rflow object with key ", fn_key, " already present in eddy")
    if (found == "disk") {
        # load previous state from disk (special key = fn_key)
        # for now, it still needs fn, fn_name, hash_input_fn, split_output_fn
        rflow_data <- eddy$get_data(fn_key, fn_key, bring_closer = FALSE)
    } else {
        rflow_data <- NULL
    }
    
    # init self$
    self$fn <- fn
    self$fn_key <- fn_key
    self$fn_name <- fn_name
    self$fn_source_arg <- fn_source_arg
    self$hash_input_fn <- hash_input_fn
    self$split_output_fn <- split_output_fn
    self$eddy <- eddy
    
    # set calc_in_hash
    if (!is.null(fn_source_arg)) {
        self$calc_in_hash <- self$calc_in_hash_source
    } else if (!is.null(hash_input_fn)) {
        self$calc_in_hash <- self$calc_in_hash_custom
    } else {
        self$calc_in_hash <- self$calc_in_hash_default
    }
    
    # the enclosing envir of rn_fn is not changed to preserve access to self$
    self$rf_fn <- self$rf_fn_default
    # rf_fn and fn have the same arguments
    formals(self$rf_fn) <- formals(args(fn))
    
    if (is.null(rflow_data)) {
        # state
        self$state <- tibble::data_frame(
            in_hash = character(),
            out_hash = character(),
            fn_key = character(),
            time_stamp = now_utc(0L)
        )
        self$state_index <- NA_integer_
        # output state
        self$output_state <- tibble::data_frame(
            out_hash = character(),
            elem_name = character(),
            elem_hash = character()
        )
    } else {
        self$state <- rflow_data$state
        self$state_index <- rflow_data$state_index
        self$output_state <- rflow_data$output_state
    }
    
    # register itself in eddy
    eddy$add_rflow(fn_key, self)
    
    invisible(NULL)
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
    self$eddy$add_data(self$fn_key, rflow_data, self$fn_key)
}, overwrite = TRUE)


# print ----
# nocov start
R6Flow$set("public", "print", function() {
    
    name <- ifelse(
        is.null(self$fn_name),
        "an anonymous function", crayon::bold(self$fn_name))
    
    emph_R6Flow <- paste0("<", crayon::italic("R6Flow"), ">")
    cat(emph_R6Flow, " describing ", name, ": \n",
        "  - number of states: ", nrow(self$state), "\n",
        "  - current state: ", self$state_index, "\n",
        "  - is_valid: ", self$is_valid, "\n",
        sep = "")
    
    print(as.data.frame(self$state))
    
    invisible(self)
}, overwrite = TRUE)
# nocov end


# get_element ----
R6Flow$set("public", "get_element", function(name = NULL) {
    
    state <- self$get_state()
    if (is.null(state) || nrow(state) == 0L) {
        is_valid <- FALSE
        elem_hash <- NULL
    } else if (is.null(name)) {
        is_valid <- TRUE
        elem_hash <- state$out_hash
    } else {
        is_valid <- TRUE
        found_state_idx <- which(
            self$output_state$out_hash == state$out_hash &
            self$output_state$elem_name == name
        )
        if (length(found_state_idx) != 1L) {
            stop("Cannot find output element: ", name)
        }
        elem_hash <- self$output_state$elem_hash[found_state_idx]
    }
    
    # class does not inherit R6Flow since it has a different structure
    structure(list(
        self = self,
        is_valid = is_valid,
        elem_name = name,
        elem_hash = elem_hash
    ), class = "R6FlowElement")
}, overwrite = TRUE)


# collect_data ----
R6Flow$set("public", "collect_data", function(name = NULL) {
    
    state <- self$get_state()
    if (is.null(state) || nrow(state) == 0L) {
        vis_out_lst <- list(
            value = NULL,
            visible = TRUE
        )
    } else if (is.null(name)) {
        vis_out_lst <- self$eddy$get_data(state$out_hash, self$fn_key)
    } else {
        found_state_idx <- which(
            self$output_state$out_hash == state$out_hash &
            self$output_state$elem_name == name
        )
        if (length(found_state_idx) != 1L)
            stop("Cannot find output element: ", name)
        elem_hash <- self$output_state$elem_hash[found_state_idx]
        vis_out_lst <- self$eddy$get_data(elem_hash, self$fn_key)
    }
    
    # preserve the output visibility of the original fn
    if (vis_out_lst$visible) {
        vis_out_lst$value
    } else {
        invisible(vis_out_lst$value)
    }
}, overwrite = TRUE)


# collect_hash ----
R6Flow$set("public", "collect_hash", function(name = NULL) {
    
    state <- self$get_state()
    if (is.null(state) || nrow(state) == 0L) {
        NA_character_
    } else if (is.null(name)) {
        state$out_hash
    } else {
        found_state_idx <- which(
            self$output_state$out_hash == state$out_hash &
            self$output_state$elem_name == name
        )
        if (length(found_state_idx) != 1L)
            stop("Cannot find output element: ", name)
        self$output_state$elem_hash[found_state_idx]
    }
}, overwrite = TRUE)


# find_state_index ----
R6Flow$set("public", "find_state_index", function(in_hash) {
    
    # since we just looking for the index, we do not check if the
    # eddy contains the cache
    found_state_idx <- which(
        self$state$in_hash == in_hash &
        self$state$fn_key == self$fn_key
    )
    len <- length(found_state_idx)
    stopifnot(len <= 1L)
    
    if (len == 0L) 0L else found_state_idx
}, overwrite = TRUE)


# get_state ----
R6Flow$set("public", "get_state", function(index = NULL) {
    
    if (is.null(index)) index <- self$state_index
    
    if (is.na(index) || index < 1L || index > nrow(self$state)) {
        # returns a zero row df if index not valid
        self$state[0L, , drop = FALSE]
    } else {
        self$state[index, , drop = FALSE]
    }
    
}, overwrite = TRUE)


# check_state ----
R6Flow$set("public", "check_state", function(index = NULL) {
    
    state <- self$get_state(index)
    changed <- if (is.null(state) || nrow(state) == 0L) {
        if (is.null(index)) {
            # zero rows for current state (index = NULL) --> is_invalid <- FALSE
            if (is.na(self$state_index)) {
                TRUE        # OK
            } else {
                # side effect: invalid state if cannot find row
                self$state_index <- NA_integer_
                FALSE       # had to make a change
            }
        } else {
            # special request (invalid index provided)
            TRUE            # OK
        }
    } else {
        # we have an out_hash, does it exist in eddy?
        in_eddy <- self$eddy$has_data(state$out_hash, self$fn_key)
        if (!in_eddy) {
            # need to remove this state and maybe update the index
            if (is.null(index)) {
                # using state_index --> remove row and mark as invalid
                self$state <- self$state[
                    -self$state_index, , drop = FALSE]
                self$state_index <- NA_integer_
            } else {
                self$state <- self$state[-index, , drop = FALSE]
                if (index <= self$state_index) {
                    state_index <- self$state_index - 1L
                    if (state_index < 1L) state_index <- NA_integer_
                    self$state_index <- state_index
                }
            }
        }
        in_eddy             # if not in eddy --> had to make a change
    }
    if (changed) self$save()
    
    changed
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
    
}, overwrite = TRUE)


# update_state ----
R6Flow$set("public", "update_state", function(index,
                                              in_hash,
                                              out_hash,
                                              make_current = TRUE) {
    
    if (!rlang::is_scalar_integerish(index) || 
       is.na(index) || index < 1L || index > nrow(self$state)) {
        stop("update_state> not a valid index")
    }
    
    self$state[index, ] <- list(
        in_hash = in_hash,
        out_hash = out_hash,
        fn_key = self$fn_key,
        time_stamp = now_utc()
    )
    if (make_current) self$state_index <- index
    
}, overwrite = TRUE)


# get_out_hash ----
R6Flow$set("public", "get_out_hash", function(in_hash) {
    
    index <- self$find_state_index(in_hash)
    if (index == 0L) {
        NA_character_
    } else {
        # we have an index, does its put_hash exist in eddy?
        if (self$check_state(index)) {
            self$state$out_hash[index]
        } else {
            NA_character_
        }
    }
    
}, overwrite = TRUE)


# add_output_state ----
R6Flow$set("public", "add_output_state", function(out_hash,
                                                  elem_name,
                                                  elem_hash) {
    
    found_state_idx <- which(
        self$output_state$out_hash == out_hash &
        self$output_state$elem_name == elem_name
    )
    len <- length(found_state_idx)
    stopifnot(len <= 1L)
    output_state <- self$output_state
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


# is_valid ----
R6Flow$set("active", "is_valid", function() {
    
    !is.na(self$state_index)
}, overwrite = TRUE)
