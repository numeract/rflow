#' @include r6eddy.R

# !diagnostics suppress=self, private


# R6Flow ----
R6Flow <- R6::R6Class(
    classname = 'R6Flow',
    public = list(
        # original fb (declare as obj to avoid locking of R6 methods)
        fn = NULL,
        # hash of fn's defined arguments and body
        fn_key = character(),
        # original function name, mostly for debug
        fn_name = character(),
        # rflow (cached) function
        rf_fn = function(...) {},
        # callback to create a custom input hash, e.g. a subset of rflow output
        hash_input_fn = NULL,
        # callback to split the output into a list to hash its elem separately
        split_output_fn = NULL,
        # link to R6Eddy obj were data is stored
        eddy = NULL,
        
        initialize = function(fn,
                              fn_key = NULL,
                              fn_name = 'missing',
                              hash_input_fn = NULL,
                              split_output_fn = NULL,
                              eddy = get_default_eddy()) {},
        save = function() {},
        element = function(what = NULL) {},
        collect = function(what = NULL) {},
        collect_hash = function(what = NULL) {},
        # internal states
        state = NULL,
        state_index = NA_integer_,
        # data frame to store elements of fn output
        output_state = NULL
    ),
    private = list(
        # data frame to store hashes and current state
        find_state_index = function(in_hash) {},
        get_state = function(index = NULL) {},
        check_state = function(index = NULL) {},
        add_state = function(in_hash,
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


# rf_fn ----
R6Flow$set("public", "rf_fn", function(...) {
    # when called, the formals already match the original fn
    mc <- match.call()
    
    # R6Flow arguments are treated specially, identify them first
    # for consitency, transform R6Flow into a R6FlowElement
    rflow_args <- 
        as.list(formals()) %>%
        purrr::keep(~ inherits(., c("R6FlowElement", "R6Flow"))) %>%
        purrr::map_if(
            .p = ~ inherits(., "R6Flow"), 
            .f = ~ .$element(what = NULL)
        )
    rflow_args_names <- names(rflow_args)
    
    if (is.null(self$hash_input_fn)) {
        # follow memoise logic to separate supplied and default arguments
        # https://cran.r-project.org/doc/manuals/r-release/R-lang.html
        #     #Argument-evaluation
        # supplied arguments
        supplied_args <- 
            as.list(mc)[-1] %>%
            discard_at(rflow_args_names)
        # default arguments that have not been supplied
        default_args <- 
            as.list(formals()) %>%
            purrr::discard(~ identical(., quote(expr = ))) %>%
            discard_at(rflow_args_names) %>%
            discard_at(names(supplied_args))
        
        # R6FlowElement args are eval for hashing by getting their hash (faster)
        # and for data by getting their data
        rflow_hash <- NULL
        if (length(rflow_args) > 0L && !self$eddy$is_reactive) {
            # non-reactive case, all rflow args must to be valid
            valid_rflow_args <- purrr::map_lgl(rflow_args, "is_valid")
            if (any(!valid_rflow_args)) {
                invalid_names <- names(rflow_args)[!valid_rflow_args]
                invalid_names <- paste(invalid_names, collapse = ", ")
                stop("Invalid input rflow args: ", invalid_names)
            }
            rflow_hash <- purrr::map(rflow_args, "elem_hash")
        }
        if (length(rflow_args) > 0L && self$eddy$is_reactive) {
            stop("reactive eddies not yet implemented")
        } 
        
        # non-rflow / static args use the data for hashing
        # supplied args eval in the evaluation frame of the calling function
        # default args eval in the evaluation frame of the original function
        static_data <- c(
            lapply(supplied_args, eval, envir = parent.frame()),
            lapply(default_args, eval, envir = environment(self$fn))
        )
        
        in_hash <- self$eddy$digest(c(rflow_hash, static_data))
    } else {
        # we already checked that fn and hash_input_fn have the same formals
        mc[[1L]] <- self$hash_input_fn
        res <- eval(mc, envir = parent.frame())
        in_hash <- self$eddy$digest(res)
    }
    
    # check self if there is an out_hash associated with in_hash and fn_key
    out_hash <- private$get_out_hash(in_hash)
    if (!is.na(out_hash)) {
        out_data <- self$eddy$get_data(out_hash, self$fn_key)
    } else {
        # not in cache, eval the function
        # replace the first arg to reconstruct the original fn match.call
        mc[[1L]] <- self$fn
        # we also need to replace R6Flow args with their data
        # avoid purrr to guarantee no unexpected effects since we have a call
        for (nm in names(rflow_args)) {
            rflow_elem <- rflow_args[[nm]]
            mc[[nm]] <- rflow_elem$self$collect(what = rflow_elem$what)
        }
        # need to preserve (and cache) the visibility of the return
        # eval envir must be the parent.frame of this func, not of withVisible
        out_data <- withVisible(eval(mc, envir = parent.frame()))
        
        # we store the out_hash to avoid (re)hashing for rflow objects
        out_hash <- self$eddy$digest(out_data)
        private$add_state(in_hash, out_hash)
        # store out_data in cache
        self$eddy$add_data(out_hash, out_data, self$fn_key)
        
        # split the out_data and store its elements
        if (!is.null(self$split_output_fn)) {
            vis_out_lst <- withVisible(self$split_output_fn(out_data))
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
                private$add_output_state(out_hash, elem_name, elem_hash)
                self$eddy$add_data(elem_hash, elem_data, self$fn_key)
            }
        }
        self$save()
    }
    
    # return the R6Flow obj instead of its data, use $collect() to get the data
    # we could have returned a structure similar to $element(), but 
    # - $collect() would require $self$collect(), or
    # - adding a new collect function preserves its encl envir, takes memory
    self
}, overwrite = TRUE)


# Initialize ----
R6Flow$set("public", "initialize", function(fn,
                                            fn_key = NULL,
                                            fn_name = 'missing',
                                            hash_input_fn = NULL,
                                            split_output_fn = NULL,
                                            eddy = get_default_eddy()) {
    
    if (is.null(fn_key)) fn_key <- make_fn_key(fn, eddy)
    
    found <- eddy$find_rflow(fn_key)
    if (found == 'memory')
        stop("rflow object with key ", fn_key, " already present in eddy")
    if (found == 'disk') {
        # load previous state from disk (special key = fn_key)
        # for now, it still needs fn, fn_name, hash_input_fn, split_output_fn
        rflow_data <- eddy$get_data(fn_key, fn_key)
    } else {
        rflow_data <- NULL
    }
    
    # init self$
    self$fn <- fn
    self$fn_key <- fn_key
    self$fn_name <- fn_name
    self$hash_input_fn <- hash_input_fn
    self$split_output_fn <- split_output_fn
    self$eddy <- eddy
    
    # R6 locks methods / functions found in public list
    unlockBinding('rf_fn', self)
    # rf_fn and fn have the same arguments
    formals(self$rf_fn) <- formals(args(fn))
    # the enclosing env of rn_fn is not changed to preserve access to self$
    # all args of this initialize function are transfered to new R6 obj
    lockBinding('rf_fn', self)
    
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


# element ----
R6Flow$set("public", "element", function(what = NULL) {
    
    state <- private$get_state()
    if (nrow(state) == 0L) {
        is_valid <- FALSE
        elem_hash <- NULL
    } else if (is.null(what)) {
        is_valid <- TRUE
        elem_hash <- state$out_hash
    } else {
        is_valid <- TRUE
        found_state_idx <- which(
            self$output_state$out_hash == state$out_hash && 
            self$output_state$elem_name == what
        )
        if (length(found_state_idx) != 1L) 
            stop("Cannot find output element: ", what)
        elem_hash <- self$output_state$elem_hash[found_state_idx]
    }
    
    structure(list(
        self = self,
        is_valid = is_valid,
        elem_name = what,
        elem_hash = elem_hash
    ), class = "R6FlowElement")
}, overwrite = TRUE)


# collect ----
R6Flow$set("public", "collect", function(what = NULL) {
    
    state <- private$get_state()
    if (nrow(state) == 0L) {
        vis_out_lst <- list(
            value = NULL,
            visible = TRUE
        )
    } else if (is.null(what)) {
        vis_out_lst <- self$eddy$get_data(state$out_hash, self$fn_key)
    } else {
        found_state_idx <- which(
            self$output_state$out_hash == state$out_hash && 
            self$output_state$elem_name == what
        )
        if (length(found_state_idx) != 1L) 
            stop("Cannot find output element: ", what)
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
R6Flow$set("public", "collect_hash", function(what = NULL) {
    
    state <- private$get_state()
    if (nrow(state) == 0L) {
        NA_character_
    } else if (is.null(what)) {
        state$out_hash
    } else {
        found_state_idx <- which(
            self$output_state$out_hash == state$out_hash && 
            self$output_state$elem_name == what
        )
        if (length(found_state_idx) != 1L) 
            stop("Cannot find output element: ", what)
        self$output_state$elem_hash[found_state_idx]
    }
}, overwrite = TRUE)


# find_state_index ----
R6Flow$set("private", "find_state_index", function(in_hash) {
    
    # since we just looking for the index, we do not check if the 
    # eddy contains the cache
    found_state_idx <- which(
        self$state$in_hash == in_hash && 
        self$state$fn_key == self$fn_key
    )
    len <- length(found_state_idx)
    stopifnot(len <= 1L)
    
    if (len == 0L) 0L else found_state_idx
}, overwrite = TRUE)


# get_state ----
R6Flow$set("private", "get_state", function(index = NULL) {
    
    if (is.null(index)) index <- self$state_index
    
    if (is.na(index) || index < 1L || index > nrow(self$state)) {
        # returns a zero row df if index not valid
        self$state[0L, , drop = FALSE]
    } else {
        self$state[index, , drop = FALSE]
    }
    
}, overwrite = TRUE)


# check_state ----
R6Flow$set("private", "check_state", function(index = NULL) {
    
    state <- private$get_state(index)
    changed <- if (nrow(state) == 0L) {
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
R6Flow$set("private", "add_state", function(in_hash, 
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


# get_out_hash ----
R6Flow$set("private", "get_out_hash", function(in_hash) {
    
    index <- private$find_state_index(in_hash)
    if (index == 0L) {
        NA_character_
    } else {
        # we have an index, does its put_hash exist in eddy?
        if (private$check_state(index)) {
            self$state$out_hash[index]
        } else {
            NA_character_
        }
    }
    
}, overwrite = TRUE)


# add_output_state ----
R6Flow$set("private", "add_output_state", function(out_hash, 
                                                   elem_name, 
                                                   elem_hash) {
    
    found_state_idx <- which(
        self$output_state$out_hash == out_hash && 
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
