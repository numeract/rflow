# Tests for chache file --------------------------------------------
context("Test cache-file functions")

if (digest::digest(Sys.info()[-c(2, 3)]) %in% c(
    "2e85e2a3018ecf3b2e5fc03bfb20fd39"
)) {
    skip("cache-memory-file functions")
}

cache_dir <- "cache_dir"
fn_group <- "default_group"


# add_group tests ----------------------------------------------------
test_that("add_group() works", {

    cache_file_test <- cache_file(cache_dir)

    expect_true(cache_file_test$add_group(fn_group))
    cache_group_dir <- fs::path(cache_dir, fn_group)
    expect_true(fs::dir_exists(cache_group_dir))

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


# list_groups tests ----------------------------------------------------
test_that("list_groups() works",{
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_group(fn_group)

    expect_equal(cache_file_test$list_groups(), fn_group)

    cache_file_test$terminate()
})

test_that("list_groups() works with no group",{
    cache_file_test <- cache_file(cache_dir)

    expect_equal(cache_file_test$list_groups(), character())

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

    expect_true(
        cache_file_test$delete_group(fn_group))
    expect_false(fs::dir_exists(cache_group_dir))

    cache_file_test$terminate()
})


# forget_group tests ----------------------------------------------------
test_that("forget_group() works", {
    cache_file_test <- cache_file(cache_dir)
    cache_file_test$add_data(fn_group, "key", "value")
    cache_group_dir <- fs::path(cache_dir, fn_group)

    expect_equal(length(fs::dir_ls(cache_group_dir)), 1)
    expect_true(cache_file_test$forget_group(fn_group))
    expect_equal(
        cache_file_test$list_keys(fn_group),
        character())
    expect_equal(length(fs::dir_ls(cache_group_dir)), 0)

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

    expect_silent(
        cache_file_test$list_keys("a_group"))

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


# add_data tests ----------------------------------------------------------
test_that("add_data() works", {
    cache_file_test <- cache_file(cache_dir)
    expect_true(cache_file_test$add_data(fn_group, "key", "value"))
    cache_file_test$add_data(fn_group, "key1", "value1")
    cache_group_dir <- fs::path(cache_dir, fn_group)

    expect_equal(cache_file_test$get_data(fn_group, "key"), "value")
    expect_true(fs::dir_exists(cache_group_dir))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key")))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key1")))

    cache_file_test$terminate()
})


test_that("add_data() works with NULL value", {
    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    expect_true(cache_file_test$add_data(fn_group, "key", NULL))
    expect_true(fs::dir_exists(cache_group_dir))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key")))

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


test_that("add_data() works with vector values", {
    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    cache_file_test$add_data(fn_group, "key", c("a", "b"))
    expect_equal(cache_file_test$get_data(fn_group, "key"), c("a", "b"))
    expect_true(fs::dir_exists(cache_group_dir))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key")))

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
    expect_true(cache_file_test$delete_data(fn_group, "key"))
    expect_false(cache_file_test$has_key(fn_group, "key"))
    expect_false(fs::file_exists(fs::path(cache_group_dir, "key")))

    cache_file_test$terminate()
})


test_that("delete_data() works with group not present", {
    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    expect_true(cache_file_test$delete_data(fn_group, "key"))
    expect_false(cache_file_test$has_key(fn_group, "key"))
    expect_false(fs::file_exists(fs::path(cache_group_dir, "key")))

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


# summary tests ---------------------------------------------------
test_that("summary() works", {

    cache_file_test <- cache_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    cache_file_test$add_data(fn_group, "key", "value")
    cache_file_test$add_data(fn_group, "key2", "value2")
    cache_file_test$add_data("a_group", "key3", "value3")

    groups <- c("a_group", fn_group)
    on_disk <- c(1L, 2L)

    expected_output <- tibble::tibble(
        fn_key = groups,
        on_disk = on_disk)

    expect_equal(cache_file_test$summary(), expected_output)

    cache_file_test$terminate()
})


test_that("summary() works with no data", {

    cache_file_test <- cache_file(cache_dir)

    groups <- character()
    on_disk <- integer()

    expected_output <- tibble::tibble(
        fn_key = groups,
        on_disk = on_disk)

    expect_equal(cache_file_test$summary(), expected_output)

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
})
