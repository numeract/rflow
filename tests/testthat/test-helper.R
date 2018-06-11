# Tests for helper --------------------------------------------
context("Test helper functions")

# if (digest::digest(Sys.info()[-c(2, 3)]) %in% c(
#     "2e85e2a3018ecf3b2e5fc03bfb20fd39"
# )) {
#     skip("cache-memory-file functions")
# }


setup({
    test_fn <- function(x, y) { x + y }
    make_key_wrap <- function(fn, fn_id, flow_options, class_name) {
        match_call <- match.call()
        make_key(match_call$fn, fn, fn_id, flow_options, class_name)
    }
    test_fn3 <- function(x) {x}
    assign("test_fn", test_fn, envir = .GlobalEnv)
    assign("test_fn3", test_fn3, envir = .GlobalEnv)
    assign("make_key_wrap", make_key_wrap, envir = .GlobalEnv)
})


test_that("is_key() works", {
    
    expect_true(is_key("a_key"))
})


test_that("is_key() stops with NA and NULL inputs", {
    
    expect_false(is_key(NULL))
    expect_false(is_key(NA))
})


test_that("is_key() stops with non character input", {
    
    expect_false(is_key(TRUE))
    expect_false(is_key(list()))
    expect_false(is_key(list(a = "a")))
    expect_false(is_key(data.frame()))
})


test_that("is_key() stops with vectors", {
    
    expect_false(is_key(c("a_key", "b_key")))
    expect_false(is_key(c(NA, "b_key")))
    expect_false(is_key(c("d", "b_key", NULL)))
})


test_that("require_keys() works", {
    
    expect_silent(require_keys())
    expect_silent(require_keys("key1"))
    expect_silent(require_keys("a_key", "b_key"))
    expect_silent(require_keys("a_key", "b_key", "c_key"))
    
 
})


test_that("require_keys() stops with NA and NULL inputs", {
    
    expect_error(require_keys(NULL))
    expect_error(require_keys(NULL, "key"))
    expect_error(require_keys("key", NULL))
    expect_error(require_keys(NA))
    expect_error(require_keys(NA, "key"))
    expect_error(require_keys(NA, NA))
   
})

test_that("require_keys() stops with non character input", {
    
    expect_error(require_keys(1, 2, "test"))
    expect_error(require_keys(TRUE))
    expect_error(require_keys(list()))
    expect_error(require_keys(data.frame()))
})


test_that("require_keys() stops with vectors", {
    
    expect_error(require_keys(c("a_key", "b_key"), "a_key"))
    expect_error(require_keys(c(NA, "b_key")))
    expect_error(require_keys(c("d", "b_key", NULL)))
})


# make_key tests ---------------------------------------------------------------
test_that("make_key() works", {
    fn <- test_fn
    fn_name <- "test_fn"
    fn_id <- "id1"
    flow_options <- get_flow_options()
    class_name <- "R6Flow"
   
    test_key <- make_key(
        fn_name, fn, fn_id, flow_options, class_name)
    
    expect_equal(test_key$action, "new")
    expect_equal(test_key$fn_name, fn_name)
    expect_equal(test_key$fn_id, fn_id)
})


test_that("make_key() works with symbols as fn_name", {
    fn <- test_fn
    fn_name <- as.symbol("test_fn")
    fn_id <- "id1"
    flow_options <- get_flow_options()
    class_name <- "R6Flow"
    
    expect_silent(test_key <- make_key(
        fn_name, fn, fn_id, flow_options, class_name))
    
    expect_equal(test_key$action, "new")
    expect_equal(test_key$fn_name, "test_fn")
    expect_equal(test_key$fn_id, fn_id)
})


test_that("make_key() stops with anonymus function", {
    fn_id <- "id1"
    flow_options <- get_flow_options()
    class_name <- "R6Flow"
    
    expect_error(
        make_key_wrap(function(x){x}, fn_id, flow_options, class_name))
})


test_that("make_key() stops with unrecognized fn_name data type", {
    fn_name <- TRUE
    fn_id <- "id1"
    flow_options <- get_flow_options()
    class_name <- "R6Flow"
    
    expect_error(
        make_key(fn_name, test_fn, fn_id, flow_options, class_name))
})


test_that("make_key() stops with primitive function", {
    fn_id <- "id1"
    flow_options <- get_flow_options()
    class_name <- "R6Flow"
    
    expect_error(
        make_key_wrap(sum, fn_id, flow_options, class_name))
})


test_that("make_key() works with integerish id", {
    fn <- test_fn
    flow_options <- get_flow_options()
    class_name <- "R6Flow"

    expect_silent(test_key <- make_key_wrap(
        fn, 10.0, flow_options,  class_name))
    
    expect_equal(test_key$fn_id, 10)
})


test_that("make_key() works with default id", {
    fn <- test_fn
    flow_options <- get_flow_options()
    class_name <- "R6Flow"
    
    expect_message(test_key <- make_key_wrap(
        fn, fn_id = NULL, flow_options,  class_name))
    expect_equal(test_key$fn_id, 1)
    expect_equal(test_key$action, "new")
})


test_that("make_key() works with custom options", {
    fn <- test_fn
    flow_options <- get_flow_options(split_dataframe = TRUE)
    class_name <- "R6Flow"
    
    test_key <- make_key_wrap(
        fn, fn_id = "id1", flow_options,  class_name)
    expect_equal(test_key$fn_id, "id1")
    expect_equal(test_key$action, "new")
})


test_that("make_key() works with options, eval_arg_fn different arguments", {
    fn <- test_fn
    flow_options <- get_flow_options(eval_arg_fn = test_fn3)
    class_name <- "R6Flow"
    
    expect_error(test_key <- make_key_wrap(
        fn, fn_id = "id1", flow_options,  class_name))
})


test_that("make_key() stops with non valid id", {
    fn <- test_fn
    flow_options <- get_flow_options()
    class_name <- "R6Flow"
    
    expect_error(make_key_wrap(
        fn, -Inf, flow_options,  class_name))
    
    expect_error(make_key_wrap(
        fn, Inf, flow_options,  class_name))
    
    expect_error(make_key_wrap(
        fn, 0, flow_options,  class_name))
    
    expect_error(make_key_wrap(
        fn, -5, flow_options,  class_name))
})


test_that("make_key() works with multiple flows", {
    fn <- test_fn
    fn_name <- "test_fn"
    fn_id <- "id1"
    flow_options <- get_flow_options()
    class_name <- "R6Flow"
    
    test_make_flow_fn <- make_flow_fn(fn, fn_id = "id1") 
    rflow_test <- test_make_flow_fn(2, 3)
    
    
    test_make_flow_fn2 <- make_flow_fn(fn, fn_id = "id2") 
    rflow_test2 <- test_make_flow_fn(3, 3)
    
    
    expect_error(test_key <- make_key_wrap(
        fn, fn_id = NULL, flow_options, class_name))
})


test_that("make_key() works with one flow already existent", {
    get_current_eddy()$reset()
    
    fn <- test_fn
    fn_name <- "test_fn"
    fn_id <- "id1"
    flow_options <- get_flow_options()
    class_name <- "R6Flow"
    
    test_make_flow_fn <- make_flow_fn(fn, fn_id = "id1") 
    rflow_test <- test_make_flow_fn(2, 3)
    
    expect_message(test_key <- make_key_wrap(
        fn, fn_id = NULL, flow_options, class_name))
    expect_equal(test_key$action, "get")
})



test_that("make_key() with flow with same body, different name", {
    get_current_eddy()$reset()
    
    flow_options <- get_flow_options()
    class_name <- "R6Flow"
    
    test_make_flow_fn <- make_flow_fn(test_fn) 
    rflow_test <- test_make_flow_fn(2, 3)
    
    test_fn2 <- function(x, y) { x + y } 
    expect_message(test_key <- make_key_wrap(
        test_fn2, fn_id = NULL, flow_options, class_name))
    expect_equal(test_key$action, "get")
})


test_that("make_key() works with different body, same name", {
    get_current_eddy()$reset()
    
    fn <- test_fn
    flow_options <- get_flow_options()
    class_name <- "R6Flow"
    
    test_make_flow_fn <- make_flow_fn(fn)
    rflow_test <- test_make_flow_fn(2, 3)
    
    fn <- function(x) {x}
    test_key <- make_key_wrap(
        fn, fn_id = NULL, flow_options, class_name)
    expect_equal(test_key$action, "new")
})


test_that("make_key() works with flow id already existent", {
    get_current_eddy()$reset()
    
    flow_options <- get_flow_options()
    class_name <- "R6Flow"
    
    test_make_flow_fn <- make_flow_fn(test_fn, fn_id = "id1") 
    rflow_test <- test_make_flow_fn(2, 3)
    
    test_key <- make_key_wrap(
        test_fn, fn_id = "id1", flow_options, class_name)
    expect_equal(test_key$action, "get")
})


test_that("make_key() with flow with same body, same id, different name", {
    get_current_eddy()$reset()
    
    flow_options <- get_flow_options()
    class_name <- "R6Flow"
    
    test_make_flow_fn <- make_flow_fn(test_fn, fn_id = "id1") 
    rflow_test <- test_make_flow_fn(2, 3)
    
    test_fn2 <- function(x, y) { x + y } 
    test_key <- make_key_wrap(
        test_fn2, fn_id = "id1", flow_options, class_name)
    expect_equal(test_key$action, "get")
})


test_that("make_key() with flow with same function, different classes", {
    get_current_eddy()$reset()
    
    flow_options <- get_flow_options()
    
    test_make_flow_fn <- make_flow_fn(identity) 
    rflow_test <- test_make_flow_fn(2)
    
    test_make_flow_dfr <- flow_dfr( 
        x = data.frame(col = c(1, 2)), fn = identity)
    
    test_dfg <- iris[c(1:5, 70:74, 100:105), ] %>%
        dplyr::group_by(Species)
    
    test_make_flow_dfg <- flow_dfg(test_dfg, fn = identity)
    
    test_key <- make_key_wrap(
        identity, fn_id = NULL, flow_options, "R6Flow")
    expect_equal(test_key$action, "get")
    
    test_key2 <- make_key_wrap(
        identity, fn_id = NULL, flow_options, "R6FlowDfr")
    expect_equal(test_key2$action, "get")
    
    test_key3 <- make_key_wrap(
        identity, fn_id = NULL, flow_options, "R6FlowDfg")
    expect_equal(test_key3$action, "get")
})


teardown({
    get_current_eddy()$reset()
    
    base::rm(list = "test_fn", envir = .GlobalEnv)
    base::rm(list = "test_fn3", envir = .GlobalEnv)
    base::rm(list = "make_key_wrap", envir = .GlobalEnv)
})
