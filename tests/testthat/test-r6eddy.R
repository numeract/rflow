cache_path <- "cache"
key <- "sum_result"
eddy_name <- "new_eddy"


# R6Eddy functions ----
context("R6Eddy functions")

test_that("new_eddy() creates cache path folder", {

    eddy <- new_eddy(eddy_name = eddy_name, cache_path = cache_path)

    expect_true(dir.exists(cache_path))
    
    delete_eddy(eddy_name = eddy_name, cache_path = cache_path)
})


test_that("find_rflow() works", {

    eddy <- new_eddy(eddy_name = eddy_name, cache_path = cache_path)
    
    rf <- make_rflow(sum, eddy = eddy)
    rflow <- environment(rf)$self

    rf(1, 2) # save some state data to disk
    
    expect_equal(eddy$find_rflow(rflow$fn_key), "memory")

    eddy$delete_rflow(rflow$fn_key, "memory")
    
    expect_equal(eddy$find_rflow(rflow$fn_key), "disk")
    
    delete_eddy(eddy_name = eddy_name, cache_path = cache_path)
})


test_that("delete_rflow() works", {
    
    eddy <- new_eddy(eddy_name = eddy_name, cache_path = cache_path)
    
    rf <- make_rflow(sum, eddy = eddy)
    rflow <- environment(rf)$self
    
    rf(1, 2) # save some state data to disk
    
    expect_equal(eddy$find_rflow(rflow$fn_key), "memory")
    
    eddy$delete_rflow(rflow$fn_key, "memory")
    
    expect_equal(eddy$find_rflow(rflow$fn_key), "disk")
    
    eddy$delete_rflow(rflow$fn_key, "disk")
    
    expect_equal(eddy$find_rflow(rflow$fn_key), "missing")
    
    delete_eddy(eddy_name = eddy_name, cache_path = cache_path)
})


test_that("add_data() & delete_data() fail if no rflow ", {

    eddy <- new_eddy(eddy_name = eddy_name)
    fn_key <- make_fn_key(sum, eddy)

    expect_error(eddy$add_data(key, "foo", fn_key))
    expect_error(eddy$get_data(key, fn_key))
    expect_error(eddy$delete_data(key, fn_key))
    
    delete_eddy(eddy_name = eddy_name)
})


test_that("delete_data() works with rflow", {
    
    eddy <- new_eddy(eddy_name = eddy_name, cache_path = cache_path)
    
    rf <- make_rflow(sum, eddy = eddy)
    rflow <- environment(rf)$self
    
    fn_key <- make_fn_key(sum, eddy)
    expect_equal(rflow$fn_key, fn_key)
    
    eddy$add_data(key, "foo", fn_key)
    eddy$add_data(key, "bar", fn_key)
    
    eddy$delete_data(key, fn_key)
    eddy$delete_data(key, fn_key)
    
    expect_equal(eddy$find_key(key, fn_key), "missing")
    
    delete_eddy(eddy_name = eddy_name, cache_path = cache_path)
})


# rflow functions ----
context("rflow functions")

test_that("add_rflow() stops if already exist", {

    rf <- make_rflow(sum)
    rflow <- environment(rf)$self

    eddy <- new_eddy(eddy_name = eddy_name)

    expect_error(eddy$add_rflow(rflow$fn_key, rflow), NA)
    expect_error(eddy$add_rflow(rflow$fn_key, rflow))

    delete_eddy(eddy_name = eddy_name)
})


test_that("forget_rflow() works", {

    eddy <- new_eddy(eddy_name = eddy_name, cache_path = cache_path)
    
    rf <- make_rflow(sum, eddy = eddy)
    rflow <- environment(rf)$self
    
    rf(1, 2) # save some state data to disk
    
    expect_warning(eddy$forget_rflow("invalid key"))
    
    expect_equal(eddy$find_rflow(rflow$fn_key), "memory")
    eddy$forget_rflow(rflow$fn_key)
    expect_equal(eddy$find_rflow(rflow$fn_key), "memory")

    delete_eddy(eddy_name = eddy_name, cache_path = cache_path)
})


test_that("get_rflow() throws warning if not found", {
    
    eddy <- new_eddy(eddy_name = eddy_name)
    
    expect_warning(eddy$get_rflow("111111"))
    
    delete_eddy(eddy_name = eddy_name)
})


test_that("reset() works", {

    eddy <- new_eddy(eddy_name = eddy_name, cache_path = cache_path)
    fn_key <- make_fn_key(sum, eddy)

    rf <- make_rflow(sum)
    rflow <- environment(rf)$self

    eddy$add_rflow(rflow$fn_key, rflow)
    eddy$add_data(key, "foo", fn_key)

    fn_path <- file.path(cache_path, fn_key)

    expect_true(rflow$fn_key %in% names(eddy$rflow_lst))
    expect_true(fn_key %in% names(eddy$cache_lst))
    expect_true(dir.exists(fn_path))

    eddy$reset()

    expect_false(rflow$fn_key %in% names(eddy$rflow_lst))
    expect_false(fn_key %in% names(eddy$cache_lst))
    expect_false(dir.exists(fn_path))

    delete_eddy(eddy_name = eddy_name, cache_path = cache_path)
})


# eddy.R ----
context("eddy.R (R6Eddy public wrappers)")

test_that("delete_eddy() works", {

    eddy <- new_eddy(eddy_name = eddy_name)

    expect_true(base::exists(eddy_name,
                             envir = get_default_env(),
                             inherits = FALSE))

    delete_eddy(eddy_name = eddy_name)

    expect_false(base::exists(eddy_name,
                              envir = get_default_env(),
                              inherits = FALSE))
})


test_that("new_eddy() checks for already existing eddy", {

    eddy <- new_eddy(eddy_name = eddy_name)

    expect_error(new_eddy(eddy_name = eddy_name))

    delete_eddy(eddy_name = eddy_name)
})


test_that("get_eddy() works with memory", {
    
    eddy <- new_eddy(eddy_name = eddy_name)
    
    expect_equal(get_eddy(eddy_name), eddy)
    
    delete_eddy(eddy_name = eddy_name)
})


test_that("get_eddy() works with disk", {
    
    eddy <- new_eddy(eddy_name = eddy_name, cache_path)
    # Delete only from memory
    delete_eddy(eddy_name = eddy_name)
    
    # Test if right eddy can still be found on disk
    expect_equal(get_eddy(cache_path, eddy_name), eddy)
    delete_eddy(eddy_name = eddy_name, cache_path = cache_path)
})


teardown({
    unlink(cache_path, recursive = TRUE)
})
