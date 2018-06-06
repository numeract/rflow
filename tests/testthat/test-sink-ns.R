# Tests for sink-ns ------------------------------------------------------------
context("tests for sink-ns")

if (digest::digest(Sys.info()[-c(2, 3)]) %in% c(
    "2e85e2a3018ecf3b2e5fc03bfb20fd39"
)) {
    skip("cache-memory-file functions")
}

setup({
    test_fn <- function(x, y) { x + y }
    test_fn2 <- function(x, y) { x * y }
    test_fn3 <- function(x) {x}
    test_fn4 <- function(x, y) {list(x = x, y = y)}
    test_sink_var <- "value1"
    test_env <- new.env()
    assign("test_env", test_env, envir = .GlobalEnv)
    assign("test_sink_var", test_sink_var, envir = test_env)
    assign("test_fn", test_fn, envir = .GlobalEnv)
    assign("test_fn2", test_fn2, envir = .GlobalEnv)
    assign("test_fn3", test_fn3, envir = .GlobalEnv)
    assign("test_fn4", test_fn4, envir = .GlobalEnv)
})

test_that("flow_ns_sink() works", {
    flow_sink <- flow_ns_sink("value2", "test_sink_var", test_env)
    
    expect_equal(flow_sink$fn_id, "test_sink_var")
    expect_equal(test_env[["test_sink_var"]], "value2")
    expect_equal(flow_sink$state_index, 1)
    
    flow_sink <- flow_ns_sink("value2", "test_sink_var", test_env)
    
    expect_equal(flow_sink$state_index, 1)
})


test_that("flow_ns_sink() works with R6flow", {
    test_flow <- make_flow_fn(test_fn)
    test_rflow <- test_flow(1, 2)
    flow_sink2 <- flow_ns_sink(test_rflow, "test_sink_flow", test_env)
    
    expect_equal(flow_sink2$fn_id, "test_sink_flow")
    expect_equal(test_env[["test_sink_flow"]], 3)
    expect_equal(flow_sink2$state_index, 1)
})


test_that("flow_ns_sink() works with multiple R6flow", {
    test_flow <- make_flow_fn(test_fn)
    test_rflow <- test_flow(1, 2)
    flow_sink2 <- flow_ns_sink(test_rflow, "test_sink_flow", test_env)
    
    expect_equal(flow_sink2$fn_id, "test_sink_flow")
    expect_equal(test_env[["test_sink_flow"]], 3)
    expect_equal(flow_sink2$state_index, 1)
    
    test_flow2 <- make_flow_fn(test_fn2)
    test_rflow2 <- test_flow2(1, 2)
    flow_sink2 <- flow_ns_sink(test_rflow2, "test_sink_flow", test_env)
    
    expect_equal(test_env[["test_sink_flow"]], 2)
    expect_equal(flow_sink2$state_index, 2)
})


test_that("flow_ns_sink() works with flow_fn", {
    test_flow_fn <- flow_fn(1, 2, fn = test_fn)
    flow_sink3 <- flow_ns_sink(test_flow_fn, "test_sink_fn", test_env)
    
    expect_equal(flow_sink3$fn_id, "test_sink_fn")
    expect_equal(test_env[["test_sink_fn"]], 3)
    expect_equal(flow_sink3$state_index, 1)
    
    test_flow_fn2 <- flow_fn(1, 3, fn = test_fn)
    flow_sink3 <- flow_ns_sink(test_flow_fn2, "test_sink_fn", test_env)
    
    expect_equal(test_env[["test_sink_fn"]], 4)
    expect_equal(flow_sink3$state_index, 2)
})


test_that("flow_ns_sink() works with NULL", {
    flow_sink_null <- flow_ns_sink(NULL, "test_sink_null", test_env)
    
    expect_equal(flow_sink_null$fn_id, "test_sink_null")
    expect_equal(test_env[["test_sink_null"]], NULL)
    expect_equal(flow_sink_null$state_index, 1)
})


test_that("flow_ns_sink() works with NA", {
    flow_sink_na <- flow_ns_sink(NA, "test_sink_na", test_env)
    
    expect_equal(flow_sink_na$fn_id, "test_sink_na")
    expect_equal(test_env[["test_sink_na"]], NA)
    expect_equal(flow_sink_na$state_index, 1)
})


test_that("flow_ns_sink() works with environment", {
    sink_env <- new.env()
    flow_sink_env <- flow_ns_sink(sink_env, "test_sink_env", test_env)
    
    expect_equal(flow_sink_env$fn_id, "test_sink_env")
    expect_equal(test_env[["test_sink_env"]], sink_env)
    expect_equal(flow_sink_env$state_index, 1)
})


test_that("flow_ns_sink() stops with non existent variable", {
    expect_error(
        flow_sink <- flow_ns_sink(non_existent_var, "test_sink_var", test_env))
})


test_that("flow_ns_sink stops with non valid var_name", {
    expect_error(
        flow_sink <- flow_ns_sink("value1", 1, test_env))
    expect_error(
        flow_sink <- flow_ns_sink("value1", TRUE, test_env))
    expect_error(
        flow_sink <- flow_ns_sink("value1", NULL, test_env))
    expect_error(
        flow_sink <- flow_ns_sink("value1", NA, test_env))
    expect_error(
        flow_sink <- flow_ns_sink("value1", NA_character_, test_env))
    expect_error(
        flow_sink <- flow_ns_sink("value1", character(), test_env))
    expect_error(
        flow_sink <- flow_ns_sink("value1", c("var1", "var2"), test_env))
    expect_error(
        flow_sink <- flow_ns_sink("value1", list(), test_env))
})


test_that("flow_ns_sink() stops with non environment", {
    non_environment <- function(){}
    expect_error(
        flow_sink <- flow_ns_sink("value1", "test_sink_var", non_environment))
})


teardown({
    base::rm(list = "test_fn", envir = .GlobalEnv)
    base::rm(list = "test_fn2", envir = .GlobalEnv)
    base::rm(list = "test_fn3", envir = .GlobalEnv)
    base::rm(list = "test_fn4", envir = .GlobalEnv)
    base::rm(list = "test_env", envir = .GlobalEnv)
})
