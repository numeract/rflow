# Tests for helper --------------------------------------------
context("Test helper functions")

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
