# Tests for flow functions -----------------------------------------------------
context("Flow functions tests")

# test flow_fn -----------------------------------------------------------------

setup({
    test_fn <- function(x, y) { x + y }
    test_fn2 <- function(x, y) { x * y }
    test_fn3 <- function(x) {x}
    assign("test_fn", test_fn, envir = .GlobalEnv)
    assign("test_fn2", test_fn2, envir = .GlobalEnv)
    assign("test_fn3", test_fn3, envir = .GlobalEnv)
})

test_that("flow_fn() works", {
    test_flow_fn <- flow_fn(test_fn) 
    rflow_test <- test_flow_fn(2, 3)
    collected_result <- rflow_test %>% collect()
    expect_equal(collected_result, 5)
    forget(rflow_test)
    
})

test_that("flow_fn() works with cache re-use", {
    
    test_flow_fn <- flow_fn(test_fn) 
    rflow_test <- test_flow_fn(2, 3)
    expect_message(
        test_flow_fn2 <- flow_fn(test_fn), "Reusing cache for function test_fn")
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

    expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = NA_character_))
    expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = NA_integer_))
    expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = character()))
    expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = c("a", "b")))
    expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = c(1, 2)))
    expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = list()))
    
    expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = Inf))
    expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = -Inf))
})


test_that("flow_fn() works with options", {
    flow_options <- get_flow_options(split_dataframe = TRUE)
    test_flow_fn <-
        flow_fn(test_fn, fn_id = 1, flow_options = flow_options)

    rflow_test <- test_flow_fn(2, 3)
    collected_result <- rflow_test %>% collect()
    expect_equal(collected_result, 5)
    forget(rflow_test)
})


test_that("flow_fn() works with different options", {
    test_flow_fn <-
        flow_fn(test_fn, fn_id = 1)
    flow_options <- get_flow_options(split_dataframe = TRUE)
    test_fn <- function(x, y) {x*y}
    expect_message(
        test_flow_fn <- flow_fn(test_fn, flow_options = flow_options), 
        "Function test_fn exists with different options, creating a new cache.")
    
    rflow_test <- test_flow_fn(2, 3)
    collected_result <- rflow_test %>% collect()
    expect_equal(collected_result, 6)
    expect_equal(rflow_test$fn_id, 2L)
    forget(rflow_test)
})


# test_that("flow_fn() works with different options, no id", {
#     
#     test_flow_fn <- flow_fn(test_fn)
#     rflow_test <- test_flow_fn(2, 3)
#    
#     
#     flow_options <- get_flow_options(split_dataframe = TRUE)
#     
#     test_fn <- function(x, y) {x*y}
#     
#     expect_message(
#         test_flow_fn <- flow_fn(test_fn, flow_options = flow_options), 
#         "Function test_fn exists with different options, creating a new cache.")
#     
#     rflow_test <- test_flow_fn(2, 3)
#    
#     collected_result <- rflow_test %>% collect()
#     expect_equal(collected_result, 6)
#     expect_equal(rflow_test$fn_id, 1L)
#     forget(rflow_test)
# })


test_that("flow() works", {
    test_flow_fn <- flow(test_fn(x = 1, y = 2))
    collected_result <- test_flow_fn %>% collect()
    expect_equal(collected_result, 3)
})


test_that("flow() stops with primitives", {
    expect_error(test_flow <- flow(sum(2, 3)))
})


test_that("flow() stops with anonymus functions", {
    expect_error(test_flow <- flow(function(x) {x}))
})


test_that("flow() stops with non function argument", {
    expect_error(flow(1))
    expect_error(flow("a_character"))
    expect_error(flow(TRUE))
    expect_error(flow(c(test_fn, test_fn2)))
    expect_error(flow(NA))
    expect_error(flow(NULL))
    expect_error(flow(list()))
})


# test_that("flow() works with character fn_id", {
#     test_flow <-
#         flow(test_fn(1, 2), fn_id = "id1")
# 
#     collected_result <- test_flow %>% collect()
#     expect_equal(collected_result, 5)
# })


# test_that("flow_fn() works with integer fn_id", {
#     test_flow_fn <-
#         flow_fn(test_fn, fn_id = 1)
#     rflow_test <- test_flow_fn(2, 3)
#     collected_result <- rflow_test %>% collect()
#     expect_equal(collected_result, 5)
#     forget(rflow_test)
# })
# 
# 
# test_that("flow_fn() works with integer fn_id", {
#     test_flow_fn <-
#         flow_fn(test_fn, fn_id = 1)
#     rflow_test <- test_flow_fn(2, 3)
#     collected_result <- rflow_test %>% collect()
#     expect_equal(collected_result, 5)
#     forget(rflow_test)
# })
# 
# 
# test_that("flow_fn() works with non valid id", {
#     expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = NA))
#     
#     # Shouldn't return an error?
#     # expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = NA_character_))
#     # expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = NA_integer_))
#     expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = character()))
#     expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = c("a", "b")))
#     expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = c(1, 2)))
#     expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = list()))
#     
#     expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = Inf))
#     expect_error(test_flow_fn <- flow_fn(test_fn, fn_id = -Inf))
# })

teardown({
    base::rm(list = "test_fn", envir = .GlobalEnv)
    base::rm(list = "test_fn2", envir = .GlobalEnv)
    base::rm(list = "test_fn3", envir = .GlobalEnv)
})
