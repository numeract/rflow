# Tests for helper --------------------------------------------
context("Test helper functions")

test_that("is_key() works", {
    
    expect_true(is_key("a_key"))
})


test_that("is_key() stops with non valid input", {
    
    expect_false(is_key(NA))
    expect_false(is_key(character()))
    expect_false(is_key(c("a_key", "b_key")))
    expect_false(is_key(c(NA, "b_key")))
    expect_false(is_key(NULL))
    expect_false(is_key(2))
})

test_that("require_keys() works", {
    
    expect_silent(require_keys("a_key", "key"))
    
})


test_that("require_keys() stops with invalid input", {
    
    expect_error(require_keys(NA, "key"))
    expect_error(require_keys(NA, NA))
    expect_error(require_keys(character()))
    expect_error(require_keys(c("a_key", "b_key"), "a_key"))
    expect_error(require_keys(c(NA, "b_key")))
    expect_error(require_keys(NULL, "b_key"))
})
