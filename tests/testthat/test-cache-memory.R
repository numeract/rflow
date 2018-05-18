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


# TBD: is this the expected behaviour though?
# test_that("add_group() works with vector input", {
#     
#     cache_memory_test <- R6CacheMemory$new()
#     expect_error(
#         cache_memory_test$add_group(c("a_group", "b_group")))
#     expect_error(
#         cache_memory_test$add_group(c("a_group", NA_character_)))
#     expect_error(
#         cache_memory_test$add_group(c(NA_character_, "b_group")))
#     expect_error(
#         cache_memory_test$add_group(c(NA_character_, NA_character_)))
#     cache_memory_test$terminate()
# })


# TBD: is this the expected behaviour though?
# test_that("add_group() stops with already existing group", {
#     
#     cache_memory_test <- R6CacheMemory$new()
#     cache_memory_test$add_group(fn_key)
#     expect_error(
#            cache_memory_test$add_group(fn_key))
#     cache_memory_test$terminate()
# })


test_that("has_group() works", {
    
    cache_memory_test <- R6CacheMemory$new()
    
    expect_false(cache_memory_test$has_group(fn_key))
    cache_memory_test$add_group(fn_key)
    expect_true(cache_memory_test$has_group(fn_key))
    
    cache_memory_test$terminate()
})


test_that("has_group() stops with NULL", {
    
    cache_memory_test <- R6CacheMemory$new()
    cache_memory_test$add_group(fn_key)
    
    expect_error(cache_memory_test$has_group(NULL))
    
    cache_memory_test$terminate()
})


test_that("has_group() stops with NA", {
    
    cache_memory_test <- R6CacheMemory$new()
    cache_memory_test$add_group(fn_key)
    
    expect_error(cache_memory_test$has_group(NA))
    
    cache_memory_test$terminate()
})


# TBD: is expected behaviour?
# test_that("has_group() stops with vector", {
#     
#     cache_memory_test <- R6CacheMemory$new()
#     cache_memory_test$add_group(fn_key)
#     
#     expect_error(
#         cache_memory_test$has_group(c(fn_key, "b_key")))
#     expect_error(
#         cache_memory_test$has_group(c(NA, NA)))
#     expect_error(
#         cache_memory_test$has_group(c(NA, "b_key")))
#     expect_error(
#         cache_memory_test$has_group(c("a_key", NA)))
#     
#     cache_memory_test$terminate()
# })


test_that("delete_group() works", {
    
    cache_memory_test <- R6CacheMemory$new()
    cache_memory_test$add_group(fn_key)
    
    expect_true(
        cache_memory_test$delete_group(fn_key))
    expect_false(
        cache_memory_test$has_group(fn_key))
    
    cache_memory_test$terminate()
})


test_that("delete_group() works with non existent group", {
    
    cache_memory_test <- R6CacheMemory$new()
    
    expect_error(
        cache_memory_test$delete_group(fn_key))
    
    cache_memory_test$terminate()
})


test_that("delete_group() stops with NULL", {
    
    cache_memory_test <- R6CacheMemory$new()
    
    expect_error(
        cache_memory_test$delete_group(NULL))
    
    cache_memory_test$terminate()
})


test_that("delete_group() stops with NA", {
    
    cache_memory_test <- R6CacheMemory$new()
    
    expect_error(
        cache_memory_test$delete_group(NA))
    
    cache_memory_test$terminate()
})

# This one works because of rm, which is vectorized
test_that("delete_group() works with vectors", {
    cache_memory_test <- R6CacheMemory$new()
    cache_memory_test$add_group(fn_key)
    cache_memory_test$add_group("b_key")
    cache_memory_test$delete_group(c(fn_key, "b_key"))
    
    expect_equal(cache_memory_test$list_groups(), character())
    
    cache_memory_test$terminate()
})


test_that("forget_group() works", {
    cache_memory_test <- R6CacheMemory$new()
    cache_memory_test$add_data(fn_key, "key", "value")
    
    cache_memory_test$forget_group(fn_key)
    expect_equal(
        cache_memory_test$list_keys(fn_key),
        character())
    cache_memory_test$terminate()
})


test_that("forget_group() stops with NULL", {
    cache_memory_test <- R6CacheMemory$new()
    cache_memory_test$add_data(fn_key, "key", "value")
    
    expect_error(
        cache_memory_test$forget_group(NULL))
   
    cache_memory_test$terminate()
})


test_that("forget_group() stops with NA", {
    cache_memory_test <- R6CacheMemory$new()
    cache_memory_test$add_data(fn_key, "key", "value")
    
    expect_error(
        cache_memory_test$forget_group(NA))
    expect_error(
        cache_memory_test$forget_group(c(NA, NA)))
    
    cache_memory_test$terminate()
})


test_that("forget_group() works with vector", {
    cache_memory_test <- R6CacheMemory$new()
    cache_memory_test$add_data(fn_key, "key", "value")
    cache_memory_test$add_data("a_key", "key1", "value1")
    expect_error(
        cache_memory_test$forget_group(c()))
    expect_error(
        cache_memory_test$forget_group(c(fn_key, "a_key")))
    expect_error(
        cache_memory_test$forget_group(c(NA, "a_key")))
    expect_error(
        cache_memory_test$forget_group(c(fn_key, NA)))
    # cache_memory_test$forget_group(c(fn_key, "a_key"))
    # expect_equal(cache_memory_test$cache_env[[fn_key]], character())
    
    cache_memory_test$terminate()
})


test_that("list_keys() works", {
    
    cache_memory_test <- R6CacheMemory$new()
    cache_memory_test$add_data(fn_key, "key", "value")
    cache_memory_test$add_data(fn_key, "key1", "value1")
    
    expect_equal(
        cache_memory_test$list_keys(fn_key), c("key", "key1"))
    
    cache_memory_test$terminate()
})


# test_that("list_keys() stops with non-existent group", {
#     
#     cache_memory_test <- R6CacheMemory$new()
#     cache_memory_test$add_data(fn_key, "key", "value")
#     cache_memory_test$add_data(fn_key, "key1", "value1")
#     
#     expect_error(
#         cache_memory_test$list_keys("a_key"))
#     
#     cache_memory_test$terminate()
# })


test_that("list_keys() works with non-existent group", {
    
    cache_memory_test <- R6CacheMemory$new()
    cache_memory_test$add_data(fn_key, "key", "value")
    cache_memory_test$add_data(fn_key, "key1", "value1")
    
    expect_silent(
        cache_memory_test$list_keys("a_key"))
    
    cache_memory_test$terminate()
})


test_that("list_keys() stops with NULL", {
    
    cache_memory_test <- R6CacheMemory$new()
 
    expect_error(
        cache_memory_test$list_keys(NULL))
    
    cache_memory_test$terminate()
})


test_that("list_keys() stops with NA", {
    
    cache_memory_test <- R6CacheMemory$new()
    
    expect_error(
        cache_memory_test$list_keys(NA))
    expect_error(
        cache_memory_test$list_keys(c(NA, NA)))
    
    cache_memory_test$terminate()
})


test_that("list_keys works with vectors", {
    
    cache_memory_test <- R6CacheMemory$new()
    cache_memory_test$add_data(fn_key, "key", "value")
    cache_memory_test$add_data("a_key", "key1", "value1")
    
    # Bad behaviour: acts silently and returns only the keys of fn_key 
    # without any warning or error
    expect_warning(
        cache_memory_test$list_keys(c(fn_key, "a_key")),
        c("key", "key1"))
    expect_error(
        cache_memory_test$list_keys(c(NA, "a_key")))
    expect_error(
        cache_memory_test$list_keys(c(fn_key, NA)),
        c("key", "key1"))
    
    cache_memory_test$terminate()
}) 


test_eddy <- NULL
