# Tests for chache-memory-file --------------------------------------------
context("Test cache-memory-file functions")
cache_dir <- "cache_dir"
fn_group <- "default_group"
skip("faster tests")

if (digest::digest(Sys.info()[-c(2, 3)]) %in% c(
    # "b61ec0997d50f9810bd123aa5220382b"
)) {
    skip("cache-memory-file functions")
}


# add_group tests ----------------------------------------------------
test_that("add_group() works", {

    cache_fmem_test <- cache_memory_file(cache_dir)
    expect_true(cache_fmem_test$add_group(fn_group))
    cache_group_dir <- fs::path(cache_dir, fn_group)

    expect_true(fs::dir_exists(cache_group_dir))
    expect_true(cache_fmem_test$has_group(fn_group))
    expect_true(base::exists(fn_group,
                             envir = cache_fmem_test$cache_env,
                             inherits = FALSE))

    cache_fmem_test$terminate()
})


# has_group tests ----------------------------------------------------
test_that("has_group() works", {

    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    expect_false(cache_fmem_test$has_group(fn_group))
    expect_false(fs::dir_exists(cache_group_dir))

    cache_fmem_test$add_group(fn_group)
    expect_true(cache_fmem_test$has_group(fn_group))
    expect_true(fs::dir_exists(cache_group_dir))

    cache_fmem_test$terminate()
})


# list_groups tests ----------------------------------------------------
test_that("list_groups() works",{
    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_fmem_test$add_group(fn_group)

    expect_equal(cache_fmem_test$list_groups(), fn_group)

    cache_fmem_test$terminate()
})

test_that("list_groups() works with no group",{
    cache_fmem_test <- cache_memory_file(cache_dir)

    expect_equal(cache_fmem_test$list_groups(), character())

    cache_fmem_test$terminate()
})


# delete_group tests ----------------------------------------------------
test_that("delete_group() works", {

    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_fmem_test$add_group(fn_group)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    expect_true(
        cache_fmem_test$delete_group(fn_group))
    expect_false(
        cache_fmem_test$has_group(fn_group))
    expect_false(fs::dir_exists(cache_group_dir))
    expect_false(base::exists(fn_group,
                              envir = cache_fmem_test$cache_env,
                              inherits = FALSE))

    cache_fmem_test$terminate()
})


test_that("delete_group() works with non existent group", {

    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    expect_true(
        cache_fmem_test$delete_group(fn_group))
    expect_false(fs::dir_exists(cache_group_dir))
    expect_false(base::exists(fn_group,
                              envir = cache_fmem_test$cache_env,
                              inherits = FALSE))

    cache_fmem_test$terminate()
})


# forget_group tests ----------------------------------------------------
test_that("forget_group() works", {
    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_fmem_test$add_data(fn_group, "key", "value")
    cache_group_dir <- fs::path(cache_dir, fn_group)

    expect_equal(length(fs::dir_ls(cache_group_dir)), 1)
    expect_true(cache_fmem_test$forget_group(fn_group))
    expect_equal(
        cache_fmem_test$list_keys(fn_group),
        character())
    expect_equal(length(fs::dir_ls(cache_group_dir)), 0)
    expect_equal(names(base::get0(
        fn_group,
        envir = cache_fmem_test$cache_env,
        inherits = FALSE)), NULL)

    cache_fmem_test$terminate()
})


# list_keys tests ----------------------------------------------------
test_that("list_keys() works", {

    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_fmem_test$add_data(fn_group, "key", "value")
    cache_fmem_test$add_data(fn_group, "key1", "value1")
    cache_group_dir <- fs::path(cache_dir, fn_group)

    cache_disk_files <- as.character(fs::path_file(
        fs::dir_ls(cache_group_dir, type = "file")))
    cache_mem_files <- base::get0(fn_group,
                                  cache_fmem_test$cache_env,
                                  inherits = FALSE)
    expect_equal(
        cache_fmem_test$list_keys(fn_group), cache_disk_files)
    expect_equal(
        cache_fmem_test$list_keys(fn_group), names(cache_mem_files))

    cache_fmem_test$terminate()
})


test_that("list_keys() works with non-existent group", {

    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_fmem_test$add_data(fn_group, "key", "value")
    cache_fmem_test$add_data(fn_group, "key1", "value1")

    expect_equal(
        cache_fmem_test$list_keys("a_group"), character())
    expect_false(
        fs::dir_exists(fs::path(cache_dir, "a_group")))
    expect_false(base::exists("a_group",
                              cache_fmem_test$cache_env,
                              inherits = FALSE)
    )
    cache_fmem_test$terminate()
})


# has_key tests ----------------------------------------------------
test_that("has_key() works", {

    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_fmem_test$add_data(fn_group, "key", "value")
    cache_fmem_test$add_data(fn_group, "key1", "value1")
    cache_group_dir <- fs::path(cache_dir, fn_group)

    expect_true(
        cache_fmem_test$has_key(fn_group, "key"))
    expect_true(
        cache_fmem_test$has_key(fn_group, "key1"))
    expect_true(
        fs::file_exists(fs::path(cache_group_dir, "key")))
    expect_true(
        fs::file_exists(fs::path(cache_group_dir, "key1")))
    expect_true(base::exists(fn_group,
                             envir = cache_fmem_test$cache_env,
                             inherits = FALSE))
    cache_fmem_test$terminate()
})


# get_data tests ----------------------------------------------------
test_that("get_data() works", {
    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_fmem_test$add_data(fn_group, "key", "value")
    cache_fmem_test$add_data(fn_group, "key1", "value1")
    cache_group_dir <- fs::path(cache_dir, fn_group)

    expect_equal(cache_fmem_test$get_data(fn_group, "key"), "value")
    expect_equal(cache_fmem_test$get_data(fn_group, "key1"), "value1")

    cache_fmem_test$terminate()
})


test_that("get_data() works with empty memory", {
    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    cache_fmem_test$add_data(fn_group, "key", "value")

    kv_lst <- base::get(
        fn_group, envir = cache_fmem_test$cache_env, inherits = FALSE)
    kv_lst[["key"]] <- NULL
    base::assign(fn_group, value = kv_lst, pos = cache_fmem_test$cache_env)
    expect_true(
        fs::file_exists(fs::path(cache_group_dir, "key")))

    expect_equal(cache_fmem_test$get_data(fn_group, "key"), "value")

    cache_fmem_test$terminate()
})


test_that("get_data() works with empty memory and no file", {
    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    cache_fmem_test$add_data(fn_group, "key", "value")

    kv_lst <- base::get(
        fn_group, envir = cache_fmem_test$cache_env, inherits = FALSE)
    kv_lst[["key"]] <- NULL
    base::assign(fn_group, value = kv_lst, pos = cache_fmem_test$cache_env)

    fs::file_delete(fs::path(cache_group_dir, "key"))

    expect_error(cache_fmem_test$get_data(fn_group, "key"))

    cache_fmem_test$terminate()
})


test_that("get_data() works with empty memory and NULL value", {
    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    cache_fmem_test$add_data(fn_group, "key", NULL)

    kv_lst <- base::get(
        fn_group, envir = cache_fmem_test$cache_env, inherits = FALSE)
    kv_lst[["key"]] <- NULL
    base::assign(fn_group, value = kv_lst, pos = cache_fmem_test$cache_env)

    expect_silent(cache_fmem_test$get_data(fn_group, "key"))

    cache_fmem_test$terminate()
})



test_that("get_data() stops with non-existent group", {
    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_fmem_test$add_data(fn_group, "key", "value")
    cache_fmem_test$add_data(fn_group, "key1", "value1")

    expect_error(cache_fmem_test$get_data("a_group", "key2"))

    cache_fmem_test$terminate()
})


# add_data tests ----------------------------------------------------------
test_that("add_data() works", {
    cache_fmem_test <- cache_memory_file(cache_dir)
    expect_true(
        cache_fmem_test$add_data(fn_group, "key", "value"))
    expect_true(
        cache_fmem_test$add_data(fn_group, "key1", "value1"))
    cache_group_dir <- fs::path(cache_dir, fn_group)

    expect_equal(cache_fmem_test$get_data(fn_group, "key"), "value")
    expect_true(fs::dir_exists(cache_group_dir))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key")))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key1")))
    expect_true(base::exists(fn_group,
                             envir = cache_fmem_test$cache_env,
                             inherits = FALSE))

    cache_fmem_test$terminate()
})

test_that("add_data() works with NULL value", {
    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    expect_silent(cache_fmem_test$add_data(fn_group, "key", NULL))
    expect_true(fs::dir_exists(cache_group_dir))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key")))
    expect_true(base::exists(fn_group,
                             envir = cache_fmem_test$cache_env,
                             inherits = FALSE))

    cache_fmem_test$terminate()
})


test_that("add_data() works with NA values", {
    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    cache_fmem_test$add_data(fn_group, "key", NA)
    expect_equal(cache_fmem_test$get_data(fn_group, "key"), NA)
    expect_true(fs::dir_exists(cache_group_dir))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key")))
    expect_true(base::exists(fn_group,
                             envir = cache_fmem_test$cache_env,
                             inherits = FALSE))

    cache_fmem_test$terminate()
})


test_that("add_data() works with vector values", {
    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    cache_fmem_test$add_data(fn_group, "key", c("a", "b"))
    expect_equal(cache_fmem_test$get_data(fn_group, "key"), c("a", "b"))
    expect_true(fs::dir_exists(cache_group_dir))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key")))
    expect_true(base::exists(fn_group,
                             envir = cache_fmem_test$cache_env,
                             inherits = FALSE))

    cache_fmem_test$terminate()
})


test_that("add_data() works with empty string values", {
    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    cache_fmem_test$add_data(fn_group, "key", character())
    expect_equal(cache_fmem_test$get_data(fn_group, "key"), character())
    expect_true(fs::dir_exists(cache_group_dir))
    expect_true(fs::file_exists(fs::path(cache_group_dir, "key")))
    expect_true(base::exists(fn_group,
                             envir = cache_fmem_test$cache_env,
                             inherits = FALSE))

    cache_fmem_test$terminate()
})


# delete_data tests ----------------------------------------------------------
test_that("delete_data() works", {
    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)


    cache_fmem_test$add_data(fn_group, "key", "value")
    kv_lst <- base::get(fn_group, envir = cache_fmem_test$cache_env, inherits = FALSE)

    cache_fmem_test$delete_data(fn_group, "key")
    expect_false(cache_fmem_test$has_key(fn_group, "key"))
    expect_false(fs::file_exists(fs::path(cache_group_dir, "key")))

    cache_fmem_test$terminate()
})


test_that("delete_data() works with group not present", {
    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    cache_fmem_test$delete_data(fn_group, "key")
    expect_false(cache_fmem_test$has_key(fn_group, "key"))
    expect_false(fs::file_exists(fs::path(cache_group_dir, "key")))

    cache_fmem_test$terminate()
})


test_that("delete_data() works with non-existent key", {
    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    cache_fmem_test$add_data(fn_group, "key", "value")
    cache_fmem_test$add_data(fn_group, "key2", "value2")
    cache_fmem_test$add_data("a_group", "key3", "value3")

    expect_silent(
        cache_fmem_test$delete_data(fn_group, "key3"))
    expect_false(
        fs::file_exists(fs::path(cache_group_dir, "key3")))

    cache_fmem_test$terminate()
})


# summary tests ---------------------------------------------------
test_that("summary() works", {

    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    cache_fmem_test$add_data(fn_group, "key", "value")
    cache_fmem_test$add_data(fn_group, "key2", "value2")
    cache_fmem_test$add_data("a_group", "key3", "value3")
    kv_lst <- base::get(
        fn_group, envir = cache_fmem_test$cache_env, inherits = FALSE)
    kv_lst[["key"]] <- NULL
    base::assign(fn_group, value = kv_lst, pos = cache_fmem_test$cache_env)

    groups <- c("a_group", fn_group)
    in_mem <- c(1L, 1L)
    on_disk <- c(1L, 2L)

    expected_output <- tibble::tibble(
        fn_key = groups,
        in_mem = in_mem,
        on_disk = on_disk)

    expect_equal(cache_fmem_test$summary(), expected_output)

    cache_fmem_test$terminate()
})


test_that("summary() works with no file cache", {

    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_group_dir <- fs::path(cache_dir, fn_group)

    cache_fmem_test$add_data(fn_group, "key", "value")
    fs::dir_delete(cache_group_dir)

    groups <- c(fn_group)
    in_mem <- c(1L)
    on_disk <- c(0L)

    expected_output <- tibble::tibble(
        fn_key = groups,
        in_mem = in_mem,
        on_disk = on_disk)

    expect_equal(cache_fmem_test$summary(), expected_output)

    cache_fmem_test$terminate()
})


test_that("summary() works with no data", {

    cache_fmem_test <- cache_memory_file(cache_dir)

    groups <- character()
    in_mem <- integer()
    on_disk <- integer()

    expected_output <- tibble::tibble(
        fn_key = groups,
        in_mem = in_mem,
        on_disk = on_disk)

    expect_equal(cache_fmem_test$summary(), expected_output)

    cache_fmem_test$terminate()
})


# reset tests -----------------------------------------------------------------
test_that("reset() works", {

    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_fmem_test$add_data(fn_group, "key", "value")

    cache_fmem_test$reset()
    expect_equal(cache_fmem_test, cache_memory_file(cache_dir))
    expect_equal(length(fs::dir_ls(cache_fmem_test$cache_dir)), 0)
    expect_true(is.environment(cache_fmem_test$cache_env))

    cache_fmem_test$terminate()
})


test_that("reset() works with empty cache", {

    cache_fmem_test <- cache_memory_file(cache_dir)

    cache_fmem_test$reset()
    expect_equal(cache_fmem_test, cache_memory_file(cache_dir))
    expect_equal(length(fs::dir_ls(cache_fmem_test$cache_dir)), 0)
    expect_true(is.environment(cache_fmem_test$cache_env))

    cache_fmem_test$terminate()
})


# terminate tests -----------------------------------------------------------------
test_that("terminate() works", {

    cache_fmem_test <- cache_memory_file(cache_dir)
    cache_fmem_test$add_data(fn_group, "key", "value")

    cache_fmem_test$terminate()
    expect_equal(cache_fmem_test$cache_dir, NULL)
    expect_equal(cache_fmem_test$cache_env, NULL)
    expect_false(fs::dir_exists(cache_dir))
})


test_that("terminate() works with empty cache", {

    cache_fmem_test <- cache_memory_file(cache_dir)

    expect_true(fs::dir_exists(cache_fmem_test$cache_dir))

    cache_fmem_test$terminate()
    expect_equal(cache_fmem_test$cache_dir, NULL)
    expect_equal(cache_fmem_test$cache_env, NULL)
    expect_false(fs::dir_exists(cache_dir))

})
