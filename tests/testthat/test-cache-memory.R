# Tests for chache memory --------------------------------------------
context("Test cache-memory functions")


simple_function <- function(x) {x + 1}
test_eddy <- new_eddy("test-eddy")
fn_key <- make_fn_key(simple_function, test_eddy)


test_that("add_group() works", {
    
    cache_memory_test <- R6CacheMemory$new()
    cache_memory_test$add_group(fn_key)
    expect_true(cache_memory_test$has_group(fn_key))
    cache_memory_test$terminate()
})


test_that("add_group() stops with NULL", {
    
    cache_memory_test <- R6CacheMemory$new()
    expect_error(cache_memory_test$add_group(NULL))
    cache_memory_test$terminate()
})


test_that("add_group() stops with NA", {
    
    cache_memory_test <- R6CacheMemory$new()
    expect_error(cache_memory_test$add_group(NA))
    cache_memory_test$terminate()
})


test_that("add_group() works with vector input", {
    
    cache_memory_test <- R6CacheMemory$new()
    expect_warning(
        cache_memory_test$add_group(c("a_group", "b_group")))
    cache_memory_test$terminate()
})

# TODO: is this the expected behaviour though?
# test_that("add_group() stops with existing group", {
#     
#     cache_memory_test <- R6CacheMemory$new()
#     cache_memory_test$add_group(fn_key)
#     cache_memory_test$add_group(fn_key)
#     print(cache_memory_test$list_groups)
#     cache_memory_test$terminate()
# })



test_eddy <- NULL
