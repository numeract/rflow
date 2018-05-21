# Tests for chache memory --------------------------------------------
context("Test cache-memory functions")

fn_group <- "default_group"

# add_group tests ----------------------------------------------------
test_that("add_group() works", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_group(fn_group)
    expect_true(cache_memory_test$has_group(fn_group))
    cache_memory_test$terminate()
})


test_that("add_group() stops with NULL", {
    
    cache_memory_test <- cache_memory()
    expect_error(cache_memory_test$add_group(NULL))
    cache_memory_test$terminate()
})


test_that("add_group() stops with NA", {
    
    cache_memory_test <- cache_memory()
    expect_error(cache_memory_test$add_group(NA))
    cache_memory_test$terminate()
})

test_that("add_group() stops with empty string", {
    
    cache_memory_test <- cache_memory()
    expect_error(cache_memory_test$add_group(character()))
    cache_memory_test$terminate()
})


test_that("add_group() works with vector input", {

    cache_memory_test <- cache_memory()
    expect_error(
        cache_memory_test$add_group(c("a_group", "b_group")))
    expect_error(
        cache_memory_test$add_group(c("a_group", NA_character_)))
    expect_error(
        cache_memory_test$add_group(c(NA_character_, "b_group")))
    expect_error(
        cache_memory_test$add_group(c(NA_character_, NA_character_)))
    cache_memory_test$terminate()
})


# has_group tests ----------------------------------------------------
test_that("has_group() works", {
    
    cache_memory_test <- cache_memory()
    
    expect_false(cache_memory_test$has_group(fn_group))
    cache_memory_test$add_group(fn_group)
    expect_true(cache_memory_test$has_group(fn_group))
    
    cache_memory_test$terminate()
})


test_that("has_group() stops with NULL", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_group(fn_group)
    
    expect_error(cache_memory_test$has_group(NULL))
    
    cache_memory_test$terminate()
})


test_that("has_group() stops with NA", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_group(fn_group)
    
    expect_error(cache_memory_test$has_group(NA))
    
    cache_memory_test$terminate()
})


test_that("has_group() stops with empty string", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_group(fn_group)
    
    expect_error(cache_memory_test$has_group(character()))
    
    cache_memory_test$terminate()
})


test_that("has_group() stops with vector", {

    cache_memory_test <- cache_memory()
    cache_memory_test$add_group(fn_group)

    expect_error(
        cache_memory_test$has_group(c(fn_group, "b_group")))
    expect_error(
        cache_memory_test$has_group(c(NA, NA)))
    expect_error(
        cache_memory_test$has_group(c(NA, "b_group")))
    expect_error(
        cache_memory_test$has_group(c("a_group", NA)))

    cache_memory_test$terminate()
})


# delete_group tests ----------------------------------------------------
test_that("delete_group() works", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_group(fn_group)
    
    expect_true(
        cache_memory_test$delete_group(fn_group))
    expect_false(
        cache_memory_test$has_group(fn_group))
    
    cache_memory_test$terminate()
})


test_that("delete_group() works with non existent group", {
    
    cache_memory_test <- cache_memory()
    
    expect_silent(
        cache_memory_test$delete_group(fn_group))
    
    cache_memory_test$terminate()
})


test_that("delete_group() stops with NULL", {
    
    cache_memory_test <- cache_memory()
    
    expect_error(
        cache_memory_test$delete_group(NULL))
    
    cache_memory_test$terminate()
})


test_that("delete_group() stops with NA", {
    
    cache_memory_test <- cache_memory()
    
    expect_error(
        cache_memory_test$delete_group(NA))
    
    cache_memory_test$terminate()
})


test_that("delete_group() stops with empty string", {
    
    cache_memory_test <- cache_memory()
    
    expect_error(
        cache_memory_test$delete_group(character()))
    
    cache_memory_test$terminate()
})

test_that("delete_group() works with vectors", {
    cache_memory_test <- cache_memory()
    cache_memory_test$add_group(fn_group)
    cache_memory_test$add_group("a_group")

    expect_error(
        cache_memory_test$delete_group(c(fn_group,"a_group")))
    
    cache_memory_test$terminate()
})


# forget_group tests ----------------------------------------------------
test_that("forget_group() works", {
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    
    cache_memory_test$forget_group(fn_group)
    expect_equal(
        cache_memory_test$list_keys(fn_group),
        character())
    cache_memory_test$terminate()
})


test_that("forget_group() stops with NULL", {
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    
    expect_error(
        cache_memory_test$forget_group(NULL))
   
    cache_memory_test$terminate()
})


test_that("forget_group() stops with NA", {
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    
    expect_error(
        cache_memory_test$forget_group(NA))
    expect_error(
        cache_memory_test$forget_group(c(NA, NA)))
    
    cache_memory_test$terminate()
})


test_that("forget_group() works with vector", {
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data("a_group", "key1", "value1")
    expect_error(
        cache_memory_test$forget_group(c()))
    expect_error(
        cache_memory_test$forget_group(c(fn_group, "a_group")))
    expect_error(
        cache_memory_test$forget_group(c(NA, "a_group")))
    expect_error(
        cache_memory_test$forget_group(c(fn_group, NA)))
    
    cache_memory_test$terminate()
})


test_that("forget_group() stops with empty string", {
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    
    expect_error(
        cache_memory_test$forget_group(character()))
    
    cache_memory_test$terminate()
})


# list_keys tests ----------------------------------------------------
test_that("list_keys() works", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key1", "value1")
    
    expect_equal(
        cache_memory_test$list_keys(fn_group), c("key", "key1"))
    
    cache_memory_test$terminate()
})


test_that("list_keys() works with non-existent group", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key1", "value1")
    
    expect_silent(
        cache_memory_test$list_keys("a_group"))
    
    cache_memory_test$terminate()
})


test_that("list_keys() stops with NULL", {
    
    cache_memory_test <- cache_memory()
 
    expect_error(
        cache_memory_test$list_keys(NULL))
    
    cache_memory_test$terminate()
})


test_that("list_keys() stops with NA", {
    
    cache_memory_test <- cache_memory()
    
    expect_error(
        cache_memory_test$list_keys(NA))
    expect_error(
        cache_memory_test$list_keys(c(NA, NA)))
    
    cache_memory_test$terminate()
})


test_that("list_keys() stops with empty string", {
    
    cache_memory_test <- cache_memory()
    
    expect_error(
        cache_memory_test$list_keys(character()))
    
    cache_memory_test$terminate()
})


test_that("list_keys works with vectors", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data("a_group", "key1", "value1")

    expect_error(
        cache_memory_test$list_keys(c(fn_group, "a_group")))
    expect_error(
        cache_memory_test$list_keys(c(NA, "a_group")))
    expect_error(
        cache_memory_test$list_keys(c(fn_group, NA)))
    
    cache_memory_test$terminate()
}) 


# has_key tests ----------------------------------------------------
test_that("has_key() works", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key1", "value1")
    
    expect_true(
        cache_memory_test$has_key(fn_group, "key"))
    
    cache_memory_test$terminate()
})


test_that("has_key() works with key vector", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key1", "value1")

    expect_error(
        cache_memory_test$has_key(fn_group, c("key", "key1")))
    expect_error(
        cache_memory_test$has_key(fn_group, c("key", "key2")))
    
    cache_memory_test$terminate()
})


test_that("has_key() stops with key NA", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key1", "value1")
    
    expect_error(
        cache_memory_test$has_key(fn_group, NA))
    expect_error(
        cache_memory_test$has_key(fn_group, c(NA, NA)))
    expect_error(
        cache_memory_test$has_key(fn_group, c(NA, "key1")))
    expect_error(
        cache_memory_test$has_key(fn_group, c("key", NA)))
    
    cache_memory_test$terminate()
})


test_that("has_key() stops with key NULL", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key1", "value1")
    
    expect_error(
        cache_memory_test$has_key(fn_group, NULL))

    cache_memory_test$terminate()
})


test_that("has_key() stops with key empty string", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key1", "value1")
    
    expect_error(
        cache_memory_test$has_key(fn_group, character()))
    
    cache_memory_test$terminate()
})


# get_data tests ----------------------------------------------------
test_that("get_data() works", {
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key1", "value1")
    
    expect_equal(cache_memory_test$get_data(fn_group, "key"), "value")
    
    cache_memory_test$terminate()
})


test_that("get_data() stops with non-existent key", {
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key1", "value1")
    
    expect_error(cache_memory_test$get_data(fn_group, "key2"))
    
    cache_memory_test$terminate()
})


test_that("get_data() stops with non-existent group", {
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key1", "value1")
    
    expect_error(cache_memory_test$get_data("a_group", "key2"))
    
    cache_memory_test$terminate()
})


test_that("get_data() stops with NULL key", {
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key1", "value1")
    
    expect_error(cache_memory_test$get_data(fn_group, NULL))
    
    cache_memory_test$terminate()
})


test_that("get_data() stops with NA key", {
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key1", "value1")
    
    expect_error(cache_memory_test$get_data(fn_group, NA))
    
    cache_memory_test$terminate()
})


test_that("get_data() stops with empty string key", {
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key1", "value1")
    
    expect_error(cache_memory_test$get_data(fn_group, character()))
    
    cache_memory_test$terminate()
})


test_that("get_data() works with vector of keys", {
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key1", "value1")
    
    expect_error(cache_memory_test$get_data(fn_group, c("key", "key1")))
    expect_error(cache_memory_test$get_data(fn_group, c(NA, "key1")))
    expect_error(cache_memory_test$get_data(fn_group, c("key", NA)))
    expect_error(cache_memory_test$get_data(fn_group, c(NA, NA)))
    
    cache_memory_test$terminate()
})


# add_data tests ----------------------------------------------------------
test_that("add_data() works", {
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key1", "value1")
    
    expect_equal(cache_memory_test$get_data(fn_group, "key"), "value")
    
    cache_memory_test$terminate()
})


test_that("add_data() stops with NULL args", {
    cache_memory_test <- cache_memory()
    
    expect_error(cache_memory_test$add_data(NULL, "key", "value"))
    expect_error(cache_memory_test$add_data(fn_group, NULL, "value"))
    expect_silent(cache_memory_test$add_data(fn_group, "key", NULL))
    
    cache_memory_test$terminate()
})



test_that("add_data() works NULL args", {
    cache_memory_test <- cache_memory()
    
    expect_error(cache_memory_test$add_data(NULL, "key", "value"))
    expect_error(cache_memory_test$add_data(fn_group, NULL, "value"))
    cache_memory_test$add_data(fn_group, "key", NULL)
    expect_equal(cache_memory_test$get_data(fn_group, "key"), NULL)
    
    cache_memory_test$terminate()
})


test_that("add_data() stops with NA", {
    cache_memory_test <- cache_memory()
    
    expect_error(cache_memory_test$add_data(NA, "key", "value"))
    expect_error(cache_memory_test$add_data(fn_group, NA, "value"))
    cache_memory_test$add_data(fn_group, "key", NA)
    expect_equal(cache_memory_test$get_data(fn_group, "key"), NA)
    
    cache_memory_test$terminate()
})


test_that("add_data() works with vectors", {
    cache_memory_test <- cache_memory()
    
    expect_error(cache_memory_test$add_data(c("a", "b"), "key", "value"))
    expect_error(cache_memory_test$add_data(fn_group, c("a", "b"), "value"))
    
    expect_error(cache_memory_test$add_data(c(NA, "b"), "key", "value"))
    expect_error(cache_memory_test$add_data(fn_group, c(NA, "b"), "value"))
    
    expect_error(cache_memory_test$add_data(c(NA, NA), "key", "value"))
    expect_error(cache_memory_test$add_data(fn_group, c(NA, NA), "value"))
    
    # TODO: Can the "value" argument be a vector?
    cache_memory_test$add_data(fn_group, "key", c("a", "b"))
    expect_equal(cache_memory_test$get_data(fn_group, "key"), c("a", "b"))

    cache_memory_test$terminate()
})


test_that("add_data() works with empty strings", {
    cache_memory_test <- cache_memory()
    
    expect_error(cache_memory_test$add_data(character(), "key", "value"))
    expect_error(cache_memory_test$add_data(fn_group, character(), "value"))
    cache_memory_test$add_data(fn_group, "key", character())
    expect_equal(cache_memory_test$get_data(fn_group, "key"), character())
    
    cache_memory_test$terminate()
})


# delete_data tests ----------------------------------------------------------
test_that("delete_data() works", {
    
    cache_memory_test <- cache_memory()
    
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$delete_data(fn_group, "key")
    expect_false(cache_memory_test$has_key(fn_group, "key"))
    
    cache_memory_test$terminate()
})


test_that("delete_data() works with group not present", {
    
    cache_memory_test <- cache_memory()

    cache_memory_test$delete_data(fn_group, "key")
    expect_false(cache_memory_test$has_key(fn_group, "key"))
    
    cache_memory_test$terminate()
})


test_that("delete_data() stops with NULL args", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    
    expect_error(cache_memory_test$delete_data(NULL, "key"))
    expect_error(cache_memory_test$delete_data(fn_group, NULL))
    expect_error(cache_memory_test$delete_data(NULL, NULL))
    
    cache_memory_test$terminate()
})


test_that("delete_data() stops with NA args", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    
    expect_error(cache_memory_test$delete_data(NA, "key"))
    expect_error(cache_memory_test$delete_data(fn_group, NA))
    expect_error(cache_memory_test$delete_data(NA, NA))
    
    cache_memory_test$terminate()
})


test_that("delete_data() stops with empty strings", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    
    expect_error(cache_memory_test$delete_data(character(), "key"))
    expect_error(cache_memory_test$delete_data(fn_group, character()))
    expect_error(cache_memory_test$delete_data(character(), character()))
    
    cache_memory_test$terminate()
})


test_that("delete_data() stops with vectors", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key2", "value2")
    cache_memory_test$add_data("a_group", "key3", "value3")
    
    expect_error(cache_memory_test$delete_data(fn_group, c("key", "key2")))
    expect_error(cache_memory_test$delete_data(fn_group, c(NA, "key2")))
    expect_error(
        cache_memory_test$delete_data(c(fn_group, "a_group"), "key2"))

    cache_memory_test$terminate()
})


test_that("delete_data() stops with empty strings", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key2", "value2")
    cache_memory_test$add_data("a_group", "key3", "value3")
    
    expect_error(cache_memory_test$delete_data(character(), "key"))
    expect_error(cache_memory_test$delete_data(fn_group, character()))
   
    cache_memory_test$terminate()
})


test_that("delete_data() stops with non-existent key", {
    
    cache_memory_test <- cache_memory()
    cache_memory_test$add_data(fn_group, "key", "value")
    cache_memory_test$add_data(fn_group, "key2", "value2")
    cache_memory_test$add_data("a_group", "key3", "value3")
    
    expect_error(cache_memory_test$delete_data(fn_group, "key3"))
    
    cache_memory_test$terminate()
})


# reset tests -----------------------------------------------------------------
