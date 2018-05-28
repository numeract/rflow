# Tests for flow functions -----------------------------------------------------
context("Flow functions tests")

# test flow_fn -----------------------------------------------------------------
test_fn <- function(x, y) { x + y }
test_fn2 <- function(x, y) { x * y }
test_fn3 <- function(x) {x}

test_that("flow_fn() works", {
    test_flow_fn <- flow_fn(test_fn) 
    rflow_test <- test_flow_fn(2, 3)
    collected_result <- rflow_test %>% collect()
    expect_equal(collected_result, 5)
    forget(rflow_test)
    
})


test_that("flow_fn() stops with primitives", {
    expect_error(test_flow_fn <- flow_fn(sum)) 
})


test_that("flow_fn() stops with anonymus functions", {
    expect_error(test_flow_fn <- flow_fn(function(x) {x})) 
})


test_that("flow_fn() stops with non function argument", {
    expect_error(flow_fn(1))
    expect_error(flow_fn("a_character"))
    expect_error(flow_fn(TRUE))
    expect_error(flow_fn(c(test_fn, test_fn2)))
    expect_error(flow_fn(NA))
    expect_error(flow_fn(NULL))
    expect_error(flow_fn(list()))
})


test_that("flow_fn() works with character fn_id", {
        test_flow_fn <-
            flow_fn(test_fn, fn_id = "id1")
        rflow_test <- test_flow_fn(2, 3)
        collected_result <- rflow_test %>% collect()
        expect_equal(collected_result, 5)
        forget(rflow_test)
})


test_that("flow_fn() works with integer fn_id", {
    test_flow_fn <-
        flow_fn(test_fn, fn_id = 1)
    rflow_test <- test_flow_fn(2, 3)
    collected_result <- rflow_test %>% collect()
    expect_equal(collected_result, 5)
    forget(rflow_test)
})


test_that("flow_fn() works with integer fn_id", {
    test_flow_fn <-
        flow_fn(test_fn, fn_id = 1)
    rflow_test <- test_flow_fn(2, 3)
    collected_result <- rflow_test %>% collect()
    expect_equal(collected_result, 5)
    forget(rflow_test)
})


test_that("flow_fn() works with non valid id", {
    expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = NA))
    
    # Shouldn't return an error?
    # expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = NA_character_))
    # expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = NA_integer_))
    expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = character()))
    expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = c("a", "b")))
    expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = c(1, 2)))
    expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = list()))
    
    expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = Inf))
    expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = -Inf))
})
