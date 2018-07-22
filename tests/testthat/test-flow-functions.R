# Tests for flow functions -----------------------------------------------------
context("Flow functions tests")

if (digest::digest(Sys.info()[-c(2, 3)]) %in% c(
    "2e85e2a3018ecf3b2e5fc03bfb20fd39"
)) {
    skip("cache-memory-file functions")
}


# test make_flow_fn ------------------------------------------------------------
setup({
    test_fn <- function(x, y) { x + y }
    test_fn2 <- function(x, y) { x * y }
    test_fn3 <- function(x) {x}
    test_fn4 <- function(x, y) {list(x = x, y = y)}
    
    assign("test_fn", test_fn, envir = .GlobalEnv)
    assign("test_fn2", test_fn2, envir = .GlobalEnv)
    assign("test_fn3", test_fn3, envir = .GlobalEnv)
    assign("test_fn4", test_fn4, envir = .GlobalEnv)
})


test_that("make_flow_fn() works", {
    test_make_flow_fn <- make_flow_fn(test_fn) 
    rflow_test <- test_make_flow_fn(2, 3)
    collected_result <- rflow_test %>% collect()
    expect_equal(collected_result, 5)
})


test_that("make_flow_fn() works with pipes", {
    get_current_eddy()$reset()
    
    test_make_flow_fn <- make_flow_fn(test_fn) 
    test_make_flow_fn2 <- make_flow_fn(test_fn2) 
    test_make_flow_fn3 <- make_flow_fn(test_fn3) 
    rflow_test <- test_make_flow_fn(1, 2) %>%
        test_make_flow_fn2(2) %>%
        test_make_flow_fn3()
    collected_result <- rflow_test %>% collect()
    expect_equal(collected_result, 6)
})


test_that("make_flow_fn() works with cache re-use", {
    get_current_eddy()$reset()
    
    test_make_flow_fn <- make_flow_fn(test_fn) 
    rflow_test <- test_make_flow_fn(2, 3)
    expect_message(
        test_make_flow_fn2 <- make_flow_fn(test_fn))
})


test_that("make_flow_fn() works with same body, different name", {
    get_current_eddy()$reset()
    
    test_fn5 <- function(x, y) { x + y }
    assign("test_fn5", test_fn5, envir = .GlobalEnv)
    test_make_flow_fn <- make_flow_fn(test_fn) 
    rflow_test <- test_make_flow_fn(2, 3)
    expect_message(
        test_make_flow_fn2 <- make_flow_fn(test_fn5))
    rflow_test$eddy$reset()
    base::rm(list = "test_fn5", envir = .GlobalEnv)
})


test_that("make_flow_fn() stops with non function argument", {
    get_current_eddy()$reset()
    
    expect_error(make_flow_fn(1))
    expect_error(make_flow_fn("a_character"))
    expect_error(make_flow_fn(TRUE))
    expect_error(make_flow_fn(c(test_fn, test_fn2)))
    expect_error(make_flow_fn(NA))
    expect_error(make_flow_fn(NULL))
    expect_error(make_flow_fn(list()))
})


test_that("make_flow_fn() works with character fn_id", {
    get_current_eddy()$reset()
    
    test_make_flow_fn <-
        make_flow_fn(test_fn, fn_id = "id1")
    rflow_test <- test_make_flow_fn(2, 3)
    collected_result <- rflow_test %>% collect()
    expect_equal(collected_result, 5)
})


test_that("make_flow_fn() works with different body/options", {
    get_current_eddy()$reset()
    
    test_make_flow_fn <-
        make_flow_fn(test_fn, fn_id = 1)
    flow_options <- get_flow_options(split_dataframe = TRUE)
    test_fn <- function(x, y) {x*y}
    expect_message(
        test_make_flow_fn <- make_flow_fn(test_fn, flow_options = flow_options))
    
    rflow_test <- test_make_flow_fn(2, 3)
    collected_result <- rflow_test %>% collect()
    expect_equal(collected_result, 6)
    expect_equal(rflow_test$fn_id, 1L)
})


# flow_call tests --------------------------------------------------------------
test_that("flow_call() works", {
    get_current_eddy()$reset()
    
    test_flow <- flow_call(test_fn(x = 1, y = 2))
    collected_result <- test_flow %>% collect()
    expect_equal(collected_result, 3)
})


test_that("flow_call() stops with non function argument", {
    get_current_eddy()$reset()
    
    expect_error(flow_call(1))
    expect_error(flow_call("a_character"))
    expect_error(flow_call(TRUE))
    expect_error(flow_call(c(test_fn, test_fn2)))
    expect_error(flow_call(NA))
    expect_error(flow_call(NULL))
    expect_error(flow_call(list()))
})


test_that("flow_call() works with different body/options", {
    get_current_eddy()$reset()
    
    test_flow <-
        flow_call(test_fn(2, 3), fn_id = 1)
    flow_options <- get_flow_options(split_dataframe = TRUE)
    test_fn <- function(x, y) {x*y}
    assign("test_fn", test_fn, envir = .GlobalEnv)
    expect_message(
        test_flow <- flow_call(test_fn(2, 3), flow_options = flow_options))
    
    collected_result <- test_flow %>% collect()
    expect_equal(collected_result, 6)
    test_fn <- function(x, y) { x + y }
    assign("test_fn", test_fn, envir = .GlobalEnv)
})


# flow_fn tests ----------------------------------------------------------------
test_that("flow_fn() works", {
    get_current_eddy()$reset()
    
    flow_fn_test <- flow_fn(2, 3, fn = test_fn)
    collected_result <- flow_fn_test %>% collect()
    
    expect_equal(collected_result, 5)
    expect_equal(flow_fn_test$fn_name, "test_fn")
})


test_that("flow_fn() works with id", {
    get_current_eddy()$reset()
    
    flow_fn_test <- flow_fn(2, 3, fn = test_fn, fn_id = "id1")
    collected_result <- flow_fn_test %>% collect()
    
    expect_equal(collected_result, 5)
    expect_equal(flow_fn_test$fn_id, "id1")
})


test_that("flow_fn() works with same body, different name", {
    get_current_eddy()$reset()
    
    test_fn6 <- function(x, y) { x + y }
    assign("test_fn6", test_fn6, envir = .GlobalEnv)
    
    flow_fn_test <- flow_fn(2, 3, fn = test_fn)
    expect_message(
        flow_fn_test2 <- flow_fn(2, 3, fn = test_fn6))
    base::rm(list = "test_fn6", envir = .GlobalEnv)
})


test_that("flow_fn() works with no arguments for function", {
    get_current_eddy()$reset()
    
    expect_silent(flow_fn_test <- flow_fn(fn = test_fn, fn_id = "id1"))
    expect_error(collect(flow_fn_test))
})

test_that("flow_fn() stops with more arguments for function", {
    expect_error(flow_fn_test <- flow_fn(1, 2, 3, fn = test_fn, fn_id = "id1"))
})


test_that("flow_fn() works with pipes", {
    get_current_eddy()$reset()
    
    flow_fn_test <- flow_fn(1, 2, fn = test_fn, fn_id = "id1")
    collected_result <- flow_fn_test %>%
        flow_fn(3, fn = test_fn2) %>%
        flow_fn(fn = test_fn3) %>%
        collect()
    expect_equal(collected_result, 9)
    
    collected_result <- flow_fn_test %>%
        flow_fn(3, fn = test_fn2) %>%
        flow_fn(fn = test_fn3) %>%
        collect()
    expect_equal(collected_result, 9)
})


# element() tests --------------------------------------------------------------
test_that("element() works", {
    get_current_eddy()$reset()
    
    test_fn_flow <- make_flow_fn(test_fn4)
    test_rflow <- test_fn_flow(2,3)
    flow_element <- element(test_rflow)
    expect_true(flow_element$is_current)
    expect_false(flow_element$is_valid)
})


test_that("element() works with valid element", {
    get_current_eddy()$reset()
    
    test_fn_flow <- make_flow_fn(test_fn4)
    test_rflow <- test_fn_flow(2,3)
    collect_result <- collect(test_rflow)
    element <- element(test_rflow, "x")
    expect_true(element$is_current)
    expect_true(element$is_valid)
})


test_that("element() works with non-current flow", {
    get_current_eddy()$reset()
    
    test_fn_flow <- make_flow_fn(test_fn4)
    test_rflow <- test_fn_flow(2,3)
    test_rflow$state_index <- 0
    element <- element(test_rflow, "x")
    expect_false(element$is_current)
    expect_false(element$is_valid)
})


test_that("element() works with non existent element", {
    get_current_eddy()$reset()
    
    test_fn_flow <- make_flow_fn(test_fn4)
    test_rflow <- test_fn_flow(2,3)
    element <- element(test_rflow, "z")
    expect_true(element$is_current)
    expect_false(element$is_valid)
    expect_silent(value <- collect(element))
    expect_null(value)
})


test_that("element() stops with non rflow object", {
    get_current_eddy()$reset()
    
    expect_error(element <- element("test_nonrflow"))
})


# collect.Element tests --------------------------------------------------------
test_that("collect.Element() works", {
    get_current_eddy()$reset()
    
    test_fn_flow <- make_flow_fn(test_fn4)
    test_rflow <- test_fn_flow(2,3)
    collect_result <- collect(test_rflow)
    element <- element(test_rflow, "x")
    element_value <- collect(element)
    expect_equal(element_value, 2)
})


test_that("collect.Element() works with pipes", {
    get_current_eddy()$reset()
    
    test_fn_flow <- make_flow_fn(test_fn4)
    test_rflow <- test_fn_flow(2,3)
    test_fn_flow2 <- make_flow_fn(test_fn)
    
    collected_result <- collect(element(test_rflow, "x")) %>%
        test_fn_flow2(1) %>%
        collect()
    
    expect(collected_result, 3)
})


test_that("collect.Element() warns when multiple arguments", {
    get_current_eddy()$reset()
    
    test_fn_flow <- make_flow_fn(test_fn4)
    test_rflow <- test_fn_flow(2,3)
    element <- element(test_rflow, "x")
    
    expect_warning(element_value <- collect(element, "x"))
})


# `[.R6Flow` tests -------------------------------------------------------------
test_that("`[.R6Flow` works", {
    get_current_eddy()$reset()
    
    test_fn_flow <- make_flow_fn(test_fn4)
    test_rflow <- test_fn_flow(2,3)
    
    expect_equal(test_rflow["x"]$elem_name, "x")
})


test_that("`[.R6Flow` works without element name", {
    get_current_eddy()$reset()
    
    test_fn_flow <- make_flow_fn(test_fn4)
    test_rflow <- test_fn_flow(2,3)
    
    expect_equal(test_rflow[]$elem_name, NULL)
})


test_that("`[.R6Flow` works with non existent element", {
    get_current_eddy()$reset()
    
    test_fn_flow <- make_flow_fn(test_fn4)
    test_rflow <- test_fn_flow(2,3)
    
    expect_silent(value <- collect(test_rflow["z"]))
    expect_null(value)
})


# is_current tests -----------------------------------------------------------
test_that("is_current() works", {
    get_current_eddy()$reset()
    
    test_flow <- flow_call(test_fn(2, 3)) 
    expect_true(is_current(test_flow))
})


test_that("is_current() works with non current flow", {
    get_current_eddy()$reset()
    
    test_flow <- flow_call(test_fn(2, 3)) 
    test_flow$state_index <- 0
    expect_false(is_current(test_flow))
})


test_that("is_current() stops with non rflow input", {
    get_current_eddy()$reset()
    
    expect_error(is_current("non_rflow"))
    expect_error(is_current(NULL))
    expect_error(is_current(NA))
    expect_error(is_current(list()))
})


# is_valid tests -----------------------------------------------------------
test_that("is_valid() works", {
    get_current_eddy()$reset()
    
    test_flow <- flow_call(test_fn(2, 3)) 
    collected_flow <- collect(test_flow)
    expect_true(is_valid(test_flow))
})


test_that("is_valid() works with not current state", {
    get_current_eddy()$reset()
    
    test_flow <- flow_call(test_fn(2, 3)) 
    expect_false(is_valid(test_flow))
})


test_that("is_valid() stops with not current state argument", {
    get_current_eddy()$reset()
    
    test_flow <- flow_call(test_fn(2, 3))
    collected_flow <- collect(test_flow)
    expect_error(is_valid(test_flow, state = "next"))
})


test_that("is_valid() stops with non rflow argument", {
    get_current_eddy()$reset()
    
    expect_error(is_valid("non_rflow"))
    expect_error(is_valid(NULL))
    expect_error(is_valid(NA))
    expect_error(is_valid(list()))
})


# forget tests -----------------------------------------------------------------
test_that("forget() works", {
    get_current_eddy()$reset()
    
    test_make_flow_fn <- make_flow_fn(test_fn) 
    rflow_test <- test_make_flow_fn(2, 3)
    collect(rflow_test)
    rflow_key <- rflow_test$fn_key
    out_hash <- rflow_test$get_state()$out_hash
    
    expect_equal(length(get_current_eddy()$flow_lst), 1)
    forget(rflow_test)
    expect_equal(length(get_current_eddy()$flow_lst), 1)
    expect_true(get_current_eddy()$has_cache(rflow_key))
    expect_true(get_current_eddy()$has_flow(rflow_key))
    expect_false(get_current_eddy()$cache$has_key(rflow_key, out_hash))
    expect_equal(length(get_current_eddy()$cache$list_keys(rflow_key)), 1)
    expect_true(get_current_eddy()$cache$has_group(rflow_key))
})


test_that("forget() stops with forgotten rflow", {
    get_current_eddy()$reset()
    
    test_make_flow_fn <- make_flow_fn(test_fn) 
    rflow_test <- test_make_flow_fn(2, 3)
    collect(rflow_test)
    rflow_key <- rflow_test$fn_key
    out_hash <- rflow_test$get_state()$out_hash
    
    expect_silent(forget(rflow_test))
    expect_false(get_current_eddy()$cache$has_key(rflow_key, out_hash))
    expect_equal(length(get_current_eddy()$flow_lst), 1)
    expect_true(get_current_eddy()$has_cache(rflow_key))
    expect_true(get_current_eddy()$has_flow(rflow_key))
    
    expect_silent(forget(rflow_test))
})


test_that("forget() stops with state_index 0", {
    get_current_eddy()$reset()
    
    test_make_flow_fn <- make_flow_fn(test_fn) 
    rflow_test <- test_make_flow_fn(2, 3)
    rflow_key <- rflow_test$fn_key
    out_hash <- rflow_test$get_state()$out_hash
    
    rflow_test$state_index <- 0
    expect_error(forget(rflow_test))
    
    expect_equal(length(get_current_eddy()$flow_lst), 1)
    expect_true(get_current_eddy()$has_cache(rflow_key))
    expect_true(get_current_eddy()$has_flow(rflow_key))
})


test_that("forget() stops with non rflow input", {
    get_current_eddy()$reset()
    
    expect_error(forget("non_rflow"))
    expect_error(forget(NULL))
    expect_error(forget(NA))
    expect_error(forget(list()))
})


test_that("forget() stops with non current state", {
    get_current_eddy()$reset()
    
    test_make_flow_fn <- make_flow_fn(test_fn) 
    rflow_test <- test_make_flow_fn(2, 3)
    collect(rflow_test)
    rflow_key <- rflow_test$fn_key
    out_hash <- rflow_test$get_state()$out_hash
    
    expect_error(forget(rflow_test, state = "non-current"))
    
    expect_true(get_current_eddy()$has_cache(rflow_key))
    expect_true(get_current_eddy()$has_flow(rflow_key))
    expect_true(get_current_eddy()$cache$has_key(rflow_key, out_hash))
})


# is_flow tests ----------------------------------------------------------------
test_that("is_flow() works R6Flow", {
    get_current_eddy()$reset()
    
    test_make_flow_fn <- make_flow_fn(test_fn) 
    rflow_test <- test_make_flow_fn(2, 3)
    expect_true(is_flow(rflow_test))
    expect_false(is_flow(test_make_flow_fn))
})


test_that("is_flow() works with Element",{
    get_current_eddy()$reset()
    
    test_make_flow_fn <- make_flow_fn(test_fn4) 
    rflow_test <- test_make_flow_fn(2, 3)
    rflow_element <- element(rflow_test, "x")
    expect_true(is_flow(rflow_element))
})


test_that("is_flow() works with non-Element and non-R6Flow input",{
    get_current_eddy()$reset()
    
    expect_false(is_flow("non-element-rflow"))
    expect_false(is_flow(NULL))
    expect_false(is_flow(NA))
    expect_false(is_flow(list()))
})


# is_flow_fn tests -------------------------------------------------------------
test_that("is_flow_fn() works", {
    get_current_eddy()$reset()
    
    test_make_flow_fn <- make_flow_fn(test_fn) 
    expect_true(is_flow_fn(test_make_flow_fn))
})


test_that("is_flow_fn() works with non flow fn", {
    get_current_eddy()$reset()
    
    expect_false(is_flow_fn("non_flow"))
    expect_false(is_flow_fn(test_fn))
    expect_false(is_flow_fn(NULL))
    expect_false(is_flow_fn(NA))
    expect_false(is_flow_fn(list()))
})


# is_not_flow_fn tests -------------------------------------------------------------
test_that("is_not_flow_fn() works", {
    get_current_eddy()$reset()
    
    test_make_flow_fn <- make_flow_fn(test_fn) 
    expect_true(is_not_flow_fn(test_fn))
    expect_false(is_not_flow_fn(test_make_flow_fn))
})


test_that("is_flow_fn() works with non function arguments", {
    get_current_eddy()$reset()
    
    expect_false(is_not_flow_fn("non_function"))
    expect_false(is_not_flow_fn(NULL))
    expect_false(is_not_flow_fn(NA))
    expect_false(is_not_flow_fn(list()))
})


teardown({
    get_current_eddy()$reset()
    
    base::rm(list = "test_fn", envir = .GlobalEnv)
    base::rm(list = "test_fn2", envir = .GlobalEnv)
    base::rm(list = "test_fn3", envir = .GlobalEnv)
    base::rm(list = "test_fn4", envir = .GlobalEnv)
})

