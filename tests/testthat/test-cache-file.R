# Tests for chache file --------------------------------------------
context("Test cache-file functions")
cache_dir <- "cache_dir"
fn_group <- "default_group"

# add_group tests ----------------------------------------------------
test_that("add_group() works", {
    
    cache_file_test <- cache_file(cache_dir)
    
    cache_file_test$add_group(fn_group)
    cache_group_dir <- fs::path(cache_dir, fn_group)
    expect_true(fs::dir_exists(cache_group_dir))
    
    cache_file_test$terminate()
})


test_that("add_group() stops with NULL", {

    cache_file_test <- cache_file(cache_dir)
    expect_error(cache_file_test$add_group(NULL))
    
    cache_file_test$terminate()
})


test_that("add_group() stops with NA", {
     
     cache_file_test <- cache_file(cache_dir)
     expect_error(cache_file_test$add_group(NA))
     cache_file_test$terminate()
})
 
test_that("add_group() stops with empty string", {

    cache_file_test <- cache_file(cache_dir)
    expect_error(cache_file_test$add_group(character()))
    cache_file_test$terminate()
})


test_that("add_group() works with vector input", {

    cache_file_test <- cache_file(cache_dir)
    expect_error(
        cache_file_test$add_group(c("a_group", "b_group")))
    expect_error(
        cache_file_test$add_group(c("a_group", NA_character_)))
    expect_error(
        cache_file_test$add_group(c(NA_character_, "b_group")))
    expect_error(
        cache_file_test$add_group(c(NA_character_, NA_character_)))
    cache_file_test$terminate()
})


# has_group tests ----------------------------------------------------
test_that("has_group() works", {

    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_false(cache_file_test$has_group(fn_group))
    expect_false(fs::dir_exists(cache_group_dir))
    
    cache_file_test$add_group(fn_group)
    expect_true(cache_file_test$has_group(fn_group))
    expect_true(fs::dir_exists(cache_group_dir))

    cache_file_test$terminate()
})


test_that("has_group() stops with NULL", {

    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    cache_file_test$add_group(fn_group)

    expect_error(cache_file_test$has_group(NULL))
    
    cache_file_test$terminate()
})


test_that("has_group() stops with NA", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_group(fn_group)

    expect_error(cache_file_test$has_group(NA))

    cache_file_test$terminate()
})


test_that("has_group() stops with empty string", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_group(fn_group)

    expect_error(cache_file_test$has_group(character()))

    cache_file_test$terminate()
})


test_that("has_group() stops with vector", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_group(fn_group)

    expect_error(
        cache_file_test$has_group(c(fn_group, "b_group")))
    expect_error(
        cache_file_test$has_group(c(NA, NA)))
    expect_error(
        cache_file_test$has_group(c(NA, "b_group")))
    expect_error(
        cache_file_test$has_group(c("a_group", NA)))

    cache_file_test$terminate()
})


# delete_group tests ----------------------------------------------------
test_that("delete_group() works", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_group(fn_group)
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_true(
        cache_file_test$delete_group(fn_group))
    expect_false(
        cache_file_test$has_group(fn_group))
    expect_false(fs::dir_exists(cache_group_dir))

    cache_file_test$terminate()
})


test_that("delete_group() works with non existent group", {

    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_silent(
        cache_file_test$delete_group(fn_group))
    expect_false(fs::dir_exists(cache_group_dir))

    cache_file_test$terminate()
})


test_that("delete_group() stops with NULL", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_group(fn_group)
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_error(
        cache_file_test$delete_group(NULL))
    expect_true(fs::dir_exists(cache_group_dir))

    cache_file_test$terminate()
})


test_that("delete_group() stops with NA", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_group(fn_group)
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_error(
        cache_file_test$delete_group(NA))
    expect_true(fs::dir_exists(cache_group_dir))

    cache_file_test$terminate()
})


test_that("delete_group() stops with empty string", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_group(fn_group)
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_error(
        cache_file_test$delete_group(character()))
    expect_true(fs::dir_exists(cache_group_dir))
    
    cache_file_test$terminate()
})


test_that("delete_group() works with vectors", {
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_group(fn_group)
    cache_file_test$add_group("a_group")

    expect_error(
        cache_file_test$delete_group(c(fn_group,"a_group")))

    cache_file_test$terminate()
})

 
# forget_group tests ----------------------------------------------------
test_that("forget_group() works", {
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_equal(length(fs::dir_ls(cache_group_dir)), 1)
    cache_file_test$forget_group(fn_group)
    expect_equal(
        cache_file_test$list_keys(fn_group),
        character())
    expect_equal(length(fs::dir_ls(cache_group_dir)), 0)
    
    cache_file_test$terminate()
})


test_that("forget_group() stops with NULL", {
    
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_error(
        cache_file_test$forget_group(NULL))
    expect_true(length(fs::dir_ls(cache_group_dir)) != 0)
    
    cache_file_test$terminate()
})


test_that("forget_group() stops with NA", {
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_error(
        cache_file_test$forget_group(NA))
    expect_error(
        cache_file_test$forget_group(c(NA, NA)))
    expect_true(length(fs::dir_ls(cache_group_dir)) != 0)
    
    cache_file_test$terminate()
})


test_that("forget_group() works with vector", {
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data("a_group", "key1", "value1")
    
    expect_error(
        cache_file_test$forget_group(c()))
    expect_error(
        cache_file_test$forget_group(c(fn_group, "a_group")))
    expect_error(
        cache_file_test$forget_group(c(NA, "a_group")))
    expect_error(
        cache_file_test$forget_group(c(fn_group, NA)))

    cache_file_test$terminate()
})


test_that("forget_group() stops with empty string", {
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")

    expect_error(
        cache_file_test$forget_group(character()))

    cache_file_test$terminate()
})


# list_keys tests ----------------------------------------------------
test_that("list_keys() works", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key1", "value1")
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    cache_files <- as.character(fs::path_file(
        fs::dir_ls(cache_group_dir, type = "file")))
    expect_equal(
        cache_file_test$list_keys(fn_group), cache_files)

    cache_file_test$terminate()
})


test_that("list_keys() works with non-existent group", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key1", "value1")

    expect_equal(
        cache_file_test$list_keys("a_group"), character())
    expect_false(
        fs::dir_exists(fs::path(cache_dir, "a_group")))

    cache_file_test$terminate()
})


test_that("list_keys() stops with NULL", {

    cache_file_test <- cache_file(cache_dir)

    expect_error(
        cache_file_test$list_keys(NULL))

    cache_file_test$terminate()
})


test_that("list_keys() stops with NA", {

    cache_file_test <- cache_file(cache_dir)

    expect_error(
        cache_file_test$list_keys(NA))
    expect_error(
        cache_file_test$list_keys(c(NA, NA)))

    cache_file_test$terminate()
})


test_that("list_keys() stops with empty string", {

    cache_file_test <- cache_file(cache_dir)

    expect_error(
        cache_file_test$list_keys(character()))

    cache_file_test$terminate()
})


test_that("list_keys stops with vectors", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data("a_group", "key1", "value1")

    expect_error(
        cache_file_test$list_keys(c(fn_group, "a_group")))
    expect_error(
        cache_file_test$list_keys(c(NA, "a_group")))
    expect_error(
        cache_file_test$list_keys(c(fn_group, NA)))

    cache_file_test$terminate()
})


# has_key tests ----------------------------------------------------
test_that("has_key() works", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key1", "value1")
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_true(
        cache_file_test$has_key(fn_group, "key"))
    expect_true(
        cache_file_test$has_key(fn_group, "key1"))
    expect_true(
        fs::file_exists(fs::path(cache_group_dir, "key")))
    expect_true(
        fs::file_exists(fs::path(cache_group_dir, "key1")))

    cache_file_test$terminate()
})


test_that("has_key() stops with key vector", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key1", "value1")

    expect_error(
        cache_file_test$has_key(fn_group, c("key", "key1")))
    expect_error(
        cache_file_test$has_key(fn_group, c("key", "key2")))

    cache_file_test$terminate()
})


test_that("has_key() stops with key NA", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key1", "value1")

    expect_error(
        cache_file_test$has_key(fn_group, NA))
    expect_error(
        cache_file_test$has_key(fn_group, c(NA, NA)))
    expect_error(
        cache_file_test$has_key(fn_group, c(NA, "key1")))
    expect_error(
        cache_file_test$has_key(fn_group, c("key", NA)))

    cache_file_test$terminate()
})


test_that("has_key() stops with key NULL", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key1", "value1")

    expect_error(
        cache_file_test$has_key(fn_group, NULL))

    cache_file_test$terminate()
})


test_that("has_key() stops with key empty string", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key1", "value1")

    expect_error(
        cache_file_test$has_key(fn_group, character()))

    cache_file_test$terminate()
})


# get_data tests ----------------------------------------------------
test_that("get_data() works", {
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key1", "value1")
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_equal(cache_file_test$get_data(fn_group, "key"), "value")
    expect_true(fs::dir_exists(cache_group_dir))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key")))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key1")))

    cache_file_test$terminate()
})


test_that("get_data() stops with non-existent key", {
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key1", "value1")

    expect_error(cache_file_test$get_data(fn_group, "key2"))

    cache_file_test$terminate()
})

 
test_that("get_data() stops with non-existent group", {
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key1", "value1")

    expect_error(cache_file_test$get_data("a_group", "key2"))

    cache_file_test$terminate()
})

 
test_that("get_data() stops with NULL key", {
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key1", "value1")

    expect_error(cache_file_test$get_data(fn_group, NULL))

    cache_file_test$terminate()
})


test_that("get_data() stops with NA key", {
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key1", "value1")

    expect_error(cache_file_test$get_data(fn_group, NA))

    cache_file_test$terminate()
})


test_that("get_data() stops with empty string key", {
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key1", "value1")

    expect_error(cache_file_test$get_data(fn_group, character()))

    cache_file_test$terminate()
})


test_that("get_data() stops with vector of keys", {
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key1", "value1")

    expect_error(cache_file_test$get_data(fn_group, c("key", "key1")))
    expect_error(cache_file_test$get_data(fn_group, c(NA, "key1")))
    expect_error(cache_file_test$get_data(fn_group, c("key", NA)))
    expect_error(cache_file_test$get_data(fn_group, c(NA, NA)))

    cache_file_test$terminate()
})


# add_data tests ----------------------------------------------------------
test_that("add_data() works", {
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key1", "value1")
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_equal(cache_file_test$get_data(fn_group, "key"), "value")
    expect_true(fs::dir_exists(cache_group_dir))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key")))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key1")))
    
    cache_file_test$terminate()
})


test_that("add_data() stops with NULL args", {
    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_error(cache_file_test$add_data(NULL, "key", "value"))
    expect_error(cache_file_test$add_data(fn_group, NULL, "value"))
    expect_false(fs::dir_exists(cache_group_dir))
    expect_false(fs::file_exists(fs::path(cache_group_dir, "key")))
    
    cache_file_test$terminate()
})


test_that("add_data() works with NULL value", {
    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    expect_silent(cache_file_test$add_data(fn_group, "key", NULL))
    expect_true(fs::dir_exists(cache_group_dir))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key")))
    
    cache_file_test$terminate()
})


test_that("add_data() stops with NA", {
    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_error(cache_file_test$add_data(NA, "key", "value"))
    expect_error(cache_file_test$add_data(fn_group, NA, "value"))
    expect_false(fs::dir_exists(cache_group_dir))
    expect_false(fs::file_exists(fs::path(cache_group_dir, "key")))

    cache_file_test$terminate()
})


test_that("add_data() works with NA values", {
    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)
  
    cache_file_test$add_data(fn_group, "key", NA)
    expect_equal(cache_file_test$get_data(fn_group, "key"), NA)
    expect_true(fs::dir_exists(cache_group_dir))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key")))
    
    cache_file_test$terminate()
})

test_that("add_data() stops with vectors", {
    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_error(cache_file_test$add_data(c("a", "b"), "key", "value"))
    expect_error(cache_file_test$add_data(fn_group, c("a", "b"), "value"))

    expect_error(cache_file_test$add_data(c(NA, "b"), "key", "value"))
    expect_error(cache_file_test$add_data(fn_group, c(NA, "b"), "value"))

    expect_error(cache_file_test$add_data(c(NA, NA), "key", "value"))
    expect_error(cache_file_test$add_data(fn_group, c(NA, NA), "value"))
    
    expect_false(fs::dir_exists(cache_group_dir))
    expect_false(fs::file_exists(fs::path(cache_group_dir, "key")))

    cache_file_test$terminate()
})


test_that("add_data() works with vector values", {
    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    cache_file_test$add_data(fn_group, "key", c("a", "b"))
    expect_equal(cache_file_test$get_data(fn_group, "key"), c("a", "b"))
    expect_true(fs::dir_exists(cache_group_dir))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key")))
    
    cache_file_test$terminate()
})


test_that("add_data() stops with empty strings", {
    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_error(cache_file_test$add_data(character(), "key", "value"))
    expect_error(cache_file_test$add_data(fn_group, character(), "value"))
    expect_false(fs::dir_exists(cache_group_dir))
    expect_false(fs::file_exists(fs::path(cache_group_dir, "key")))

    cache_file_test$terminate()
})


test_that("add_data() works with empty string values", {
    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    cache_file_test$add_data(fn_group, "key", character())
    expect_equal(cache_file_test$get_data(fn_group, "key"), character())
    expect_true(fs::dir_exists(cache_group_dir))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key")))
    
    cache_file_test$terminate()
})


# delete_data tests ----------------------------------------------------------
test_that("delete_data() works", {
    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$delete_data(fn_group, "key")
    expect_false(cache_file_test$has_key(fn_group, "key"))
    expect_false(fs::file_exists(fs::path(cache_group_dir, "key")))

    cache_file_test$terminate()
})


test_that("delete_data() works with group not present", {
    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    cache_file_test$delete_data(fn_group, "key")
    expect_false(cache_file_test$has_key(fn_group, "key"))
    expect_false(fs::file_exists(fs::path(cache_group_dir, "key")))
    
    cache_file_test$terminate()
})


test_that("delete_data() stops with NULL args", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_error(cache_file_test$delete_data(NULL, "key"))
    expect_error(cache_file_test$delete_data(fn_group, NULL))
    expect_error(cache_file_test$delete_data(NULL, NULL))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key")))
    
    cache_file_test$terminate()
})


test_that("delete_data() stops with NA args", {
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_error(cache_file_test$delete_data(NA, "key"))
    expect_error(cache_file_test$delete_data(fn_group, NA))
    expect_error(cache_file_test$delete_data(NA, NA))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key")))
    
    cache_file_test$terminate()
})


test_that("delete_data() stops with empty strings", {
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_error(cache_file_test$delete_data(character(), "key"))
    expect_error(cache_file_test$delete_data(fn_group, character()))
    expect_error(cache_file_test$delete_data(character(), character()))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key")))
    
    cache_file_test$terminate()
})


test_that("delete_data() stops with vectors", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key2", "value2")
    cache_file_test$add_data("a_group", "key3", "value3")
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    expect_error(
        cache_file_test$delete_data(fn_group, c("key", "key2")))
    expect_error(
        cache_file_test$delete_data(fn_group, c(NA, "key2")))
    expect_error(
        cache_file_test$delete_data(c(fn_group, "a_group"), "key2"))
    expect_true(
        fs::file_exists(fs::path(cache_group_dir, "key")))
    
    cache_file_test$terminate()
})


test_that("delete_data() stops with empty strings", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_group_dir <- fs::path(cache_dir, fn_group)

    expect_error(
        cache_file_test$delete_data(character(), "key"))
    expect_error(
        cache_file_test$delete_data(fn_group, character()))
    expect_true(
        fs::file_exists(fs::path(cache_group_dir, "key")))

    cache_file_test$terminate()
})

 
test_that("delete_data() works with non-existent key", {
    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)
    
    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key2", "value2")
    cache_file_test$add_data("a_group", "key3", "value3")

    expect_silent(
        cache_file_test$delete_data(fn_group, "key3"))
    expect_false(
        fs::file_exists(fs::path(cache_group_dir, "key3")))
    
    cache_file_test$terminate()
})


# reset tests -----------------------------------------------------------------
test_that("reset() works", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    
    cache_file_test$reset()
    expect_equal(cache_file_test, cache_file(cache_dir))
    expect_equal(length(fs::dir_ls(cache_file_test$cache_dir)), 0)

    cache_file_test$terminate()
})


test_that("reset() works with empty cache", {

    cache_file_test <- cache_file(cache_dir)

    cache_file_test$reset()
    expect_equal(cache_file_test, cache_file(cache_dir))
    expect_equal(length(fs::dir_ls(cache_file_test$cache_dir)), 0)

    cache_file_test$terminate()
})


# terminate tests -----------------------------------------------------------------
test_that("terminate() works", {

    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")

    cache_file_test$terminate()
    expect_equal(cache_file_test$cache_dir, NULL)
    expect_false(fs::dir_exists(cache_dir))
})


test_that("terminate() works with empty cache", {

    cache_file_test <- cache_file(cache_dir)

    expect_true(fs::dir_exists(cache_file_test$cache_dir))

    cache_file_test$terminate()
    expect_equal(cache_file_test$cache_dir, NULL)
    expect_false(fs::dir_exists(cache_dir))
    
    expect_silent(cache_file_test$terminate())
})
