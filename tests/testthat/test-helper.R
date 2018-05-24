# Tests for helper --------------------------------------------
context("Test helper functions")

test_that("is_key() works", {
    
    expect_true(is_key("a_key"))
    expect_false(is_key(NA))
    expect_false(is_key(character()))
    expect_false(is_key(c("a_key", "b_key")))
    expect_false(is_key(c(NA, "b_key")))
    expect_false(is_key(NULL))
})


