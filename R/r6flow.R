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
                              fn_name = 'no_name',
                              hash_input_fn = NULL,
                              split_output_fn = NULL,
                              eddy = get_default_eddy()) {},
        element = function(what = NULL) {},
        collect = function(what = NULL) {},
        collect_hash = function(what = NULL) {}
    ),
    private = list(
        # data frame to store hashes and current state
        state = NULL,
        state_index = NA_integer_,
        find_state_index = function(in_hash) {},
        get_state = function(index = NULL) {},
        check_state = function(index = NULL) {},
        add_state = function(in_hash,
                             out_hash,
                             make_current = TRUE) {},
        get_out_hash = function(in_hash, fn_key) {},
        
        # data frame to store elements of fn output
        output_state = NULL,
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
        self$eddy$put_data(out_hash, out_data, self$fn_key)
        
        # split the out_data and store its elements
        if (!is.null(self$split_output_fn)) {
            out_lst <- self$split_output_fn(out_data)
            if (!is.list(out_lst)) 
                stop("split_output_fn() must return a named list")
            out_nms <- names(out_lst) %if_not_in% ""
            if (length(out_nms) != length(out_lst))
                stop("split_output_fn() must provide a name for each element")
            for (elem_name in out_nms) {
                elem_data <- out_lst[[elem_name]]
                elem_hash <- self$eddy$digest(elem_data)
                private$add_output_state(out_hash, elem_name, elem_hash)
                self$eddy$put_data(elem_hash, elem_data, self$fn_key)
            }
        }
    }
    
    # return the R6Flow obj instead of its data, use $collect() to get the data
    # we could have returned a structure similar to $element(), but 
    # - $collect() would require $self$collect(), or
    # - adding a new collect function preserves its encl envir, takes memory
    self
}, overwrite = TRUE)


# Initialize ----
R6Flow$set("public", "initialize", function(fn,
                                            fn_name,
                                            hash_input_fn = NULL,
                                            split_output_fn = NULL,
                                            eddy = get_default_eddy()) {
    
    # unique key = hash of fn's defined arguments and body
    fn_formals <- formals(args(fn))
    arg_chr <- paste(
        paste(names(fn_formals), as.character(fn_formals), sep = '='),
        collapse = ', '
    )
    body_chr <- as.character(body(fn))
    fn_key <- eddy$digest(c(arg_chr, body_chr))
    
    if (eddy$exists_rflow(fn_key)) {
        stop("overwriting / re-flowing function not yet implemented")
        # TODO: common case: files were resourced and fn body changed
        # TODO: load its former state from eddy
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
    formals(self$rf_fn) <- fn_formals
    # the enclosing env of rn_fn is not changed to preserve access to self$
    # all args of this initialize function are transfered to new R6 obj
    lockBinding('rf_fn', self)
    
    # state
    private$state <- tibble::data_frame(
        in_hash = character(),
        out_hash = character(),
        fn_key = character(),
        time_stamp = now_utc(0L)
    )
    private$state_index <- NA_integer_
    
    # output state
    private$output_state <- tibble::data_frame(
        out_hash = character(),
        elem_name = character(),
        elem_hash = character()
    )
    
    self$eddy <- eddy
    # register itself in eddy
    eddy$add_rflow(fn_key, self)
    
    invisible(NULL)
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
            private$output_state$out_hash == state$out_hash && 
            private$output_state$elem_name == what
        )
        if (length(found_state_idx) != 1L) 
            stop("Cannot find output element: ", what)
        elem_hash <- private$output_state$elem_hash[found_state_idx]
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
        NULL
    } else if (is.null(what)) {
        self$eddy$get_data(state$out_hash, self$fn_key)
    } else {
        found_state_idx <- which(
            private$output_state$out_hash == state$out_hash && 
            private$output_state$elem_name == what
        )
        if (length(found_state_idx) != 1L) 
            stop("Cannot find output element: ", what)
        elem_hash <- private$output_state$elem_hash[found_state_idx]
        self$eddy$get_data(elem_hash, self$fn_key)
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
            private$output_state$out_hash == state$out_hash && 
                private$output_state$elem_name == what
        )
        if (length(found_state_idx) != 1L) 
            stop("Cannot find output element: ", what)
        private$output_state$elem_hash[found_state_idx]
    }
}, overwrite = TRUE)


# find_state_index ----
R6Flow$set("private", "find_state_index", function(in_hash) {
    
    # since we just looking for the index, we do not check if the 
    # eddy contains the cache
    found_state_idx <- which(
        private$state$in_hash == in_hash && 
        private$state$fn_key == self$fn_key
    )
    len <- length(found_state_idx)
    stopifnot(len <= 1L)
    
    if (len == 0L) 0L else found_state_idx
}, overwrite = TRUE)


# get_state ----
R6Flow$set("private", "get_state", function(index = NULL) {
    
    if (is.null(index)) index <- private$state_index
    
    if (is.na(index) || index < 1L || index > nrow(private$state)) {
        # returns a zero row df if index not valid
        private$state[0L, , drop = FALSE]
    } else {
        private$state[index, , drop = FALSE]
    }
    
}, overwrite = TRUE)


# check_state ----
R6Flow$set("private", "check_state", function(index = NULL) {
    
    state <- private$get_state(index)
    if (nrow(state) == 0L) {
        if (is.null(index)) {
            # zero rows for current state (index = NULL) --> is_invalid <- FALSE
            if (is.na(private$state_index)) {
                TRUE        # OK
            } else {
                # side effect: invalid state if cannot find row
                private$state_index <- NA_integer_
                FALSE       # had to make a change
            }
        } else {
            # special request (invalid index provided)
            TRUE            # OK
        }
    } else {
        # we have an out_hash, does it exist in eddy?
        in_eddy <- self$eddy$has_key(state$out_hash)
        if (!in_eddy) {
            # need to remove this state and maybe update the index
            if (is.null(index)) {
                # using state_index --> remove row and mark as invalid
                private$state <- private$state[
                    -private$state_index, , drop = FALSE]
                private$state_index <- NA_integer_
            } else {
                private$state <- private$state[-index, , drop = FALSE]
                if (index <= private$state_index) {
                    state_index <- private$state_index - 1L
                    if (state_index < 1L) state_index <- NA_integer_
                    private$state_index <- state_index
                }
            }
        }
        in_eddy             # if not in eddy --> had to make a change
    }
}, overwrite = TRUE)


# add_state ----
R6Flow$set("private", "add_state", function(in_hash, 
                                            out_hash, 
                                            make_current = TRUE) {
    
    private$state <- 
        private$state %>%
        tibble::add_row(
            in_hash = in_hash,
            out_hash = out_hash,
            fn_key = self$fn_key,
            time_stamp = now_utc()
        )
    if (make_current) private$state_index <- nrow(private$state)
    
}, overwrite = TRUE)


# get_out_hash ----
R6Flow$set("private", "get_out_hash", function(in_hash) {
    
    index <- private$find_state_index(in_hash)
    if (index == 0L) {
        NA_character_
    } else {
        # we have an index, does its put_hash exist in eddy?
        if (private$check_state(index)) {
            private$state$out_hash[index]
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
        private$output_state$out_hash == out_hash && 
        private$output_state$elem_name == elem_name
    )
    len <- length(found_state_idx)
    stopifnot(len <= 1L)
    output_state <- private$output_state
    if (len == 1L) {
        output_state <- output_state[-found_state_idx, , drop = FALSE]
    } 
    
    private$output_state <- 
        output_state %>%
        tibble::add_row(
            out_hash = out_hash,
            elem_name = elem_name,
            elem_hash = elem_hash
        )
    
}, overwrite = TRUE)


# is_valid ----
R6Flow$set("active", "is_valid", function() {
    
    !is.na(private$state_index)
}, overwrite = TRUE)

