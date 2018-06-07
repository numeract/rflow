# Tests for helper --------------------------------------------
context("Test helper functions")


setup({
    test_fn <- function(x, y) { x + y }
    make_key_wrap <- function(fn, fn_id, flow_options, class_name) {
        match_call <- match.call()
        make_key(match_call$fn, fn, fn_id, flow_options, class_name)
    }
    assign("test_fn", test_fn, envir = .GlobalEnv)
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
    
    expect_error(require_keys(1, 2, "ana"))
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
   
    actual_result <- make_key(
        fn_name, fn, fn_id, flow_options, class_name)
    
    expect_equal(actual_result$action, "new")
    expect_equal(actual_result$fn_name, fn_name)
    expect_equal(actual_result$fn_id, fn_id)
})


test_that("make_key() works with symbols as fn_name", {
    fn <- test_fn
    fn_name <- as.symbol("test_fn")
    fn_id <- "id1"
    flow_options <- get_flow_options()
    class_name <- "R6Flow"
    
    expect_silent(actual_result <- make_key(
        fn_name, fn, fn_id, flow_options, class_name))
    
    expect_equal(actual_result$action, "new")
    expect_equal(actual_result$fn_name, "test_fn")
    expect_equal(actual_result$fn_id, fn_id)
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

    expect_silent(actual_result <- make_key_wrap(
        fn, 10.0, flow_options,  class_name))
    
    expect_equal(actual_result$fn_id, 10)
})


test_that("make_key() works with default id", {
    fn <- test_fn
    flow_options <- get_flow_options()
    class_name <- "R6Flow"
    
    expect_message(actual_result <- make_key_wrap(
        fn, fn_id = NULL, flow_options,  class_name))
    expect_equal(actual_result$fn_id, 1)
    expect_equal(actual_result$action, "new")
})


test_that("make_key() works with custom options", {
    fn <- test_fn
    flow_options <- get_flow_options(split_dataframe = TRUE)
    class_name <- "R6Flow"
    
    actual_result <- make_key_wrap(
        fn, fn_id = "id1", flow_options,  class_name)
    expect_equal(actual_result$fn_id, "id1")
    expect_equal(actual_result$action, "new")
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
    
    
    expect_error(actual_result <- make_key_wrap(
        fn, fn_id = NULL, flow_options, class_name))
    rflow_test$eddy$reset()
})


test_that("make_key() works with one flow already existent", {
    fn <- test_fn
    fn_name <- "test_fn"
    fn_id <- "id1"
    flow_options <- get_flow_options()
    class_name <- "R6Flow"
    
    test_make_flow_fn <- make_flow_fn(fn, fn_id = "id1") 
    rflow_test <- test_make_flow_fn(2, 3)
    
    expect_message(actual_result <- make_key_wrap(
        fn, fn_id = NULL, flow_options, class_name))
    expect_equal(actual_result$action, "get")
    
    rflow_test$eddy$reset()
})


test_that("make_key() works with flow id already existent", {
    flow_options <- get_flow_options()
    class_name <- "R6Flow"
    
    test_make_flow_fn <- make_flow_fn(test_fn, fn_id = "id1") 
    rflow_test <- test_make_flow_fn(2, 3)
    
    actual_result <- make_key_wrap(
        test_fn, fn_id = "id1", flow_options, class_name)
    expect_equal(actual_result$action, "get")
    
    rflow_test$eddy$reset()
})




teardown({
    base::rm(list = "test_fn", envir = .GlobalEnv)
})
