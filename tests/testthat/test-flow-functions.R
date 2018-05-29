# Tests for flow functions -----------------------------------------------------
context("Flow functions tests")


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
    forget(rflow_test)
})

test_that("make_flow_fn() works with pipes", {
    test_make_flow_fn <- make_flow_fn(test_fn) 
    test_make_flow_fn2 <- make_flow_fn(test_fn2) 
    test_make_flow_fn3 <- make_flow_fn(test_fn3) 
    rflow_test <- test_make_flow_fn(1, 2) %>%
        test_make_flow_fn2(2) %>%
        test_make_flow_fn3()
    collected_result <- rflow_test %>% collect()
    expect_equal(collected_result, 6)
    forget(rflow_test)
})


test_that("make_flow_fn() works with cache re-use", {
    
    test_make_flow_fn <- make_flow_fn(test_fn) 
    rflow_test <- test_make_flow_fn(2, 3)
    expect_message(
        test_make_flow_fn2 <- make_flow_fn(test_fn))
    forget(rflow_test)
})


test_that("make_flow_fn() stops with primitives", {
    expect_error(test_make_flow_fn <- make_flow_fn(sum)) 
})


test_that("make_flow_fn() stops with anonymus functions", {
    expect_error(test_make_flow_fn <- make_flow_fn(function(x) {x})) 
})


test_that("make_flow_fn() stops with non function argument", {
    expect_error(make_flow_fn(1))
    expect_error(make_flow_fn("a_character"))
    expect_error(make_flow_fn(TRUE))
    expect_error(make_flow_fn(c(test_fn, test_fn2)))
    expect_error(make_flow_fn(NA))
    expect_error(make_flow_fn(NULL))
    expect_error(make_flow_fn(list()))
})


test_that("make_flow_fn() works with character fn_id", {
    test_make_flow_fn <-
        make_flow_fn(test_fn, fn_id = "id1")
    rflow_test <- test_make_flow_fn(2, 3)
    collected_result <- rflow_test %>% collect()
    expect_equal(collected_result, 5)
    forget(rflow_test)
})


test_that("make_flow_fn() works with integer fn_id", {
    test_make_flow_fn <-
        make_flow_fn(test_fn, fn_id = 1)
    rflow_test <- test_make_flow_fn(2, 3)
    collected_result <- rflow_test %>% collect()
    expect_equal(collected_result, 5)
    forget(rflow_test)
})


test_that("make_flow_fn() works with non valid id", {
    expect_error(test_make_flow_fn <- make_flow_fn(test_fn, fn_id = NA))

    expect_error(test_make_flow_fn <- make_flow_fn(test_fn, fn_id = NA_character_))
    expect_error(test_make_flow_fn <- make_flow_fn(test_fn, fn_id = NA_integer_))
    expect_error(test_make_flow_fn <- make_flow_fn(test_fn, fn_id = character()))
    expect_error(test_make_flow_fn <- make_flow_fn(test_fn, fn_id = c("a", "b")))
    expect_error(test_make_flow_fn <- make_flow_fn(test_fn, fn_id = c(1, 2)))
    expect_error(test_make_flow_fn <- make_flow_fn(test_fn, fn_id = list()))
    
    expect_error(test_make_flow_fn <- make_flow_fn(test_fn, fn_id = Inf))
    expect_error(test_make_flow_fn <- make_flow_fn(test_fn, fn_id = -Inf))
})


test_that("make_flow_fn() works with options", {
    flow_options <- get_flow_options(split_dataframe = TRUE)
    test_make_flow_fn <-
        make_flow_fn(test_fn, fn_id = 1, flow_options = flow_options)

    rflow_test <- test_make_flow_fn(2, 3)
    collected_result <- rflow_test %>% collect()
    expect_equal(collected_result, 5)
    forget(rflow_test)
})


test_that("make_flow_fn() works with different options", {
    test_make_flow_fn <-
        make_flow_fn(test_fn, fn_id = 1)
    flow_options <- get_flow_options(split_dataframe = TRUE)
    test_fn <- function(x, y) {x*y}
    expect_message(
        test_make_flow_fn <- make_flow_fn(test_fn, flow_options = flow_options))
    
    rflow_test <- test_make_flow_fn(2, 3)
    collected_result <- rflow_test %>% collect()
    expect_equal(collected_result, 6)
    expect_equal(rflow_test$fn_id, 2L)
    forget(rflow_test)
})


# flow_call tests --------------------------------------------------------------
test_that("flow_call() works", {
    test_flow <- flow_call(test_fn(x = 1, y = 2))
    collected_result <- test_flow %>% collect()
    expect_equal(collected_result, 3)
})


test_that("flow_call() stops with primitives", {
    expect_error(test_flow <- flow_call(sum(2, 3)))
})


test_that("flow_call() stops with anonymus functions", {
    expect_error(test_flow <- flow_call(function(x) {x}))
})


test_that("flow_call() stops with non function argument", {
    expect_error(flow_call(1))
    expect_error(flow_call("a_character"))
    expect_error(flow_call(TRUE))
    expect_error(flow_call(c(test_fn, test_fn2)))
    expect_error(flow_call(NA))
    expect_error(flow_call(NULL))
    expect_error(flow_call(list()))
})


test_that("flow_call() works with character fn_id", {
    test_flow <-
        flow_call(test_fn(1, 2), fn_id = "id1")

    collected_result <- test_flow %>% collect()
    expect_equal(collected_result, 3)
    forget(test_flow)
})


test_that("flow_call() works with integer fn_id", {
    test_flow <-
        flow_call(test_fn(2, 3), fn_id = 1)
    collected_result <- test_flow %>% collect()
    expect_equal(collected_result, 5)
    forget(test_flow)
})


test_that("flow_call() works with non valid id", {
    expect_error(test_flow <- flow_call(test_fn, fn_id = NA))
    
    expect_error(test_flow <- flow_call(test_fn, fn_id = NA_character_))
    expect_error(test_flow <- flow_call(test_fn, fn_id = NA_integer_))
    expect_error(test_flow <- flow_call(test_fn, fn_id = character()))
    expect_error(test_flow <- flow_call(test_fn, fn_id = c("a", "b")))
    expect_error(test_flow <- flow_call(test_fn, fn_id = c(1, 2)))
    expect_error(test_flow <- flow_call(test_fn, fn_id = list()))
    
    expect_error(test_flow <- flow_call(test_fn, fn_id = Inf))
    expect_error(test_flow <- flow_call(test_fn, fn_id = -Inf))
})


test_that("flow_call() works with options", {
    flow_options <- get_flow_options(split_dataframe = TRUE)
    test_flow <-
        flow_call(test_fn(2, 3), fn_id = 1, flow_options = flow_options)
    
    collected_result <- test_flow %>% collect()
    expect_equal(collected_result, 5)
    forget(test_flow)
})


test_that("flow_call() works with different options", {
    test_flow <-
        flow_call(test_fn(2, 3), fn_id = 1)
    flow_options <- get_flow_options(split_dataframe = TRUE)
    test_fn <- function(x, y) {x*y}
    assign("test_fn", test_fn, envir = .GlobalEnv)
    expect_message(
        test_flow <- flow_call(test_fn(2, 3), flow_options = flow_options))
    
    collected_result <- test_flow %>% collect()
    expect_equal(collected_result, 6)
    forget(test_flow)
})


test_that("element() works", {
    test_fn_flow <- make_flow_fn(test_fn4)
    test_rflow <- test_fn_flow(2,3)
    flow_element <- element(test_rflow)
    expect_true(flow_element$is_current)
    expect_false(flow_element$is_valid)
    forget(test_rflow)
})


test_that("element() works with valid element", {
    test_fn_flow <- make_flow_fn(test_fn4)
    test_rflow <- test_fn_flow(2,3)
    collect_result <- collect(test_rflow)
    element <- element(test_rflow, "x")
    expect_true(element$is_current)
    expect_true(element$is_valid)
    forget(test_rflow)
})


test_that("element() works with non-current flow", {
    test_fn_flow <- make_flow_fn(test_fn4)
    test_rflow <- test_fn_flow(2,3)
    test_rflow$state_index <- 0
    element <- element(test_rflow, "x")
    expect_false(element$is_current)
    expect_false(element$is_valid)
})


test_that("element() stops with non existent element", {
    test_fn_flow <- make_flow_fn(test_fn4)
    test_rflow <- test_fn_flow(2,3)
    expect_error(element <- element(test_rflow, "z"))
    forget(test_rflow)
})


test_that("element() stops with non rflow object", {
    expect_error(element <- element("test_nonrflow"))
})


# collect.Element tests --------------------------------------------------------
test_that("collect.Element() works", {
    test_fn_flow <- make_flow_fn(test_fn4)
    test_rflow <- test_fn_flow(2,3)
    collect_result <- collect(test_rflow)
    element <- element(test_rflow, "x")
    element_value <- collect(element)
    expect_equal(element_value, 2)
    forget(test_rflow)
})


test_that("collect.Element() works with pipes", {
    test_fn_flow <- make_flow_fn(test_fn4)
    test_rflow <- test_fn_flow(2,3)
    test_fn_flow2 <- make_flow_fn(test_fn)
    
    collected_result <- collect(element(test_rflow, "x")) %>%
        test_fn_flow2(1) %>%
        collect()
    
    expect(collected_result, 3)
    forget(test_rflow)
})


test_that("collect.Element() warns when multiple arguments", {
    test_fn_flow <- make_flow_fn(test_fn4)
    test_rflow <- test_fn_flow(2,3)
    element <- element(test_rflow, "x")
    
    expect_warning(element_value <- collect(element, "x"))
    
    forget(test_rflow)
})


teardown({
    base::rm(list = "test_fn", envir = .GlobalEnv)
    base::rm(list = "test_fn2", envir = .GlobalEnv)
    base::rm(list = "test_fn3", envir = .GlobalEnv)
    base::rm(list = "test_fn4", envir = .GlobalEnv)
})

