# # Tests for chache memory --------------------------------------------
# context("Test cache-memory functions")
# 
# fn_group <- "default_group"
# 
# # add_group tests ----------------------------------------------------
# test_that("add_group() works", {
#     
#     cache_memory_test <- cache_memory()
#     expect_true(cache_memory_test$add_group(fn_group))
#     expect_true(cache_memory_test$has_group(fn_group))
#     cache_memory_test$terminate()
# })
# 
# 
# # has_group tests ----------------------------------------------------
# test_that("has_group() works", {
#     
#     cache_memory_test <- cache_memory()
#     
#     expect_false(cache_memory_test$has_group(fn_group))
#     cache_memory_test$add_group(fn_group)
#     expect_true(cache_memory_test$has_group(fn_group))
#     
#     cache_memory_test$terminate()
# })
# 
# 
# # list_groups tests ----------------------------------------------------
# test_that("list_groups() works",{
#     cache_memory_test <- cache_memory()
#     cache_memory_test$add_group(fn_group)
#     expected_value <- as.character(
#         ls.str(pos = cache_memory_test$cache_env, all.names = TRUE))
#     
#     expect_equal(cache_memory_test$list_groups(), expected_value)
#     
#     cache_memory_test$terminate()
# })
# 
# 
# # delete_group tests ----------------------------------------------------
# test_that("delete_group() works", {
#     
#     cache_memory_test <- cache_memory()
#     cache_memory_test$add_group(fn_group)
#     
#     expect_true(
#         cache_memory_test$delete_group(fn_group))
#     expect_false(
#         cache_memory_test$has_group(fn_group))
#     
#     cache_memory_test$terminate()
# })
# 
# 
# test_that("delete_group() works with non existent group", {
#     
#     cache_memory_test <- cache_memory()
#     
#     expect_true(
#         cache_memory_test$delete_group(fn_group))
#     
#     cache_memory_test$terminate()
# })
# 
# 
# # forget_group tests ----------------------------------------------------
# test_that("forget_group() works", {
#     cache_memory_test <- cache_memory()
#     cache_memory_test$add_data(fn_group, "key", "value")
#     
#     expect_true(cache_memory_test$forget_group(fn_group))
#     expect_equal(
#         cache_memory_test$list_keys(fn_group),
#         character())
#     cache_memory_test$terminate()
# })
# 
# 
# # list_keys tests ----------------------------------------------------
# test_that("list_keys() works", {
#     
#     cache_memory_test <- cache_memory()
#     cache_memory_test$add_data(fn_group, "key", "value")
#     cache_memory_test$add_data(fn_group, "key1", "value1")
#     
#     expect_equal(
#         cache_memory_test$list_keys(fn_group), c("key", "key1"))
#     
#     cache_memory_test$terminate()
# })
# 
# 
# test_that("list_keys() works with non-existent group", {
#     
#     cache_memory_test <- cache_memory()
#     cache_memory_test$add_data(fn_group, "key", "value")
#     cache_memory_test$add_data(fn_group, "key1", "value1")
#     
#     expect_silent(
#         cache_memory_test$list_keys("a_group"))
#     
#     cache_memory_test$terminate()
# })
# 
# 
# # has_key tests ----------------------------------------------------
# test_that("has_key() works", {
#     
#     cache_memory_test <- cache_memory()
#     cache_memory_test$add_data(fn_group, "key", "value")
#     cache_memory_test$add_data(fn_group, "key1", "value1")
#     
#     expect_true(
#         cache_memory_test$has_key(fn_group, "key"))
#     
#     cache_memory_test$terminate()
# })
# 
# 
# # get_data tests ----------------------------------------------------
# test_that("get_data() works", {
#     cache_memory_test <- cache_memory()
#     cache_memory_test$add_data(fn_group, "key", "value")
#     cache_memory_test$add_data(fn_group, "key1", "value1")
#     
#     expect_equal(cache_memory_test$get_data(fn_group, "key"), "value")
#     
#     cache_memory_test$terminate()
# })
# 
# 
# test_that("get_data() stops with non-existent key", {
#     cache_memory_test <- cache_memory()
#     cache_memory_test$add_data(fn_group, "key", "value")
#     cache_memory_test$add_data(fn_group, "key1", "value1")
#     
#     expect_error(cache_memory_test$get_data(fn_group, "key2"))
#     
#     cache_memory_test$terminate()
# })
# 
# 
# test_that("get_data() stops with non-existent group", {
#     cache_memory_test <- cache_memory()
#     cache_memory_test$add_data(fn_group, "key", "value")
#     cache_memory_test$add_data(fn_group, "key1", "value1")
#     
#     expect_error(cache_memory_test$get_data("a_group", "key2"))
#     
#     cache_memory_test$terminate()
# })
# 
# 
# # add_data tests ----------------------------------------------------------
# test_that("add_data() works", {
#     cache_memory_test <- cache_memory()
#     expect_true(
#         cache_memory_test$add_data(fn_group, "key", "value"))
#     expect_equal(
#         cache_memory_test$get_data(fn_group, "key"), "value")
#     cache_memory_test$add_data(fn_group, "key1", "value1")
#     expect_true(
#         cache_memory_test$add_data(fn_group, "key", NULL))
#     
#     cache_memory_test$terminate()
# })
# 
# 
# # delete_data tests ----------------------------------------------------------
# test_that("delete_data() works", {
#     
#     cache_memory_test <- cache_memory()
#     
#     cache_memory_test$add_data(fn_group, "key", "value")
#     expect_true(cache_memory_test$delete_data(fn_group, "key"))
#     expect_false(cache_memory_test$has_key(fn_group, "key"))
#     
#     cache_memory_test$terminate()
# })
# 
# 
# test_that("delete_data() works with group not present", {
#     
#     cache_memory_test <- cache_memory()
#     
#     cache_memory_test$delete_data(fn_group, "key")
#     expect_false(cache_memory_test$has_key(fn_group, "key"))
#     
#     cache_memory_test$terminate()
# })
# 
# 
# test_that("delete_data() works with non-existent key", {
#     
#     cache_memory_test <- cache_memory()
#     cache_memory_test$add_data(fn_group, "key", "value")
#     cache_memory_test$add_data(fn_group, "key2", "value2")
#     cache_memory_test$add_data("a_group", "key3", "value3")
#     
#     expect_true(cache_memory_test$delete_data(fn_group, "key3"))
#     
#     cache_memory_test$terminate()
# })
# 
# # summary tests ---------------------------------------------------
# test_that("summary() works", {
#     
#     cache_memory_test <- cache_memory()
#     
#     cache_memory_test$add_data(fn_group, "key", "value")
#     cache_memory_test$add_data(fn_group, "key2", "value2")
#     cache_memory_test$add_data("a_group", "key3", "value3")
#     
#     groups <- c("a_group", fn_group)
#     in_memory <- c(1L, 2L)
#     
#     expected_output <- tibble::tibble(
#         fn_key = groups,
#         in_memory = in_memory)
#     
#     expect_equal(cache_memory_test$summary(), expected_output)
#     
#     cache_memory_test$terminate()
# })
# 
# 
# test_that("summary() works with no data", {
#     
#     cache_memory_test <- cache_memory()
#     
#     groups <- character()
#     in_memory <- integer()
#     
#     expected_output <- tibble::tibble(
#         fn_key = groups,
#         in_memory = in_memory)
#     
#     expect_equal(cache_memory_test$summary(), expected_output)
#     
#     cache_memory_test$terminate()
# })
# 
# 
# # reset tests -----------------------------------------------------------------
# test_that("reset() works", {
#     
#     cache_memory_test <- cache_memory()
#     cache_memory_test$add_data(fn_group, "key", "value")
#     
#     cache_memory_test$reset()
#     expect_equal(cache_memory_test, cache_memory())
#     expect_true(is.environment(cache_memory_test$cache_env))
#     
#     cache_memory_test$terminate()
# })
# 
# 
# test_that("reset() works with empty cache", {
#     
#     cache_memory_test <- cache_memory()
#     
#     cache_memory_test$reset()
#     expect_equal(cache_memory_test, cache_memory())
#     expect_true(is.environment(cache_memory_test$cache_env))
#     
#     cache_memory_test$terminate()
# })
# 
# 
# # terminate tests -----------------------------------------------------------------
# test_that("terminate() works", {
#     
#     cache_memory_test <- cache_memory()
#     cache_memory_test$add_data(fn_group, "key", "value")
#     
#     cache_memory_test$terminate()
#     expect_equal(cache_memory_test$cache_env, NULL)
#     expect_false(is.environment(cache_memory_test$cache_env))
# })
# 
# 
# test_that("terminate() works with empty cache", {
#     
#     cache_memory_test <- cache_memory()
#     
#     expect_true(is.environment(cache_memory_test$cache_env))
#     
#     cache_memory_test$terminate()
#     expect_equal(cache_memory_test$cache_env, NULL)
#     expect_false(is.environment(cache_memory_test$cache_env))
# })
