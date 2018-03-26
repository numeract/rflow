context("R6Eddy functions")

cache_path <- 'cache'
key <- "sum_result"
eddy_name <- "new_eddy"

test_that("new_eddy() creates cache path folder", {
    
    eddy <- new_eddy(eddy_name = eddy_name, cache_path = cache_path)
    
    expect_true(dir.exists(cache_path))
    
    delete_eddy(eddy_name = eddy_name, cache_path = cache_path)
})


test_that("find_rflow() works", {

    rf <- make_rflow(sum)
    rflow <- environment(rf)$self

    eddy <- new_eddy(eddy_name = eddy_name)
    eddy$add_rflow(rflow$fn_key, rflow)

    expect_equal(eddy$find_rflow(rflow$fn_key), "memory")
    # TODO: Test for disk as well

    delete_eddy(eddy_name = eddy_name)
    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("add_data() works", {

    eddy <- new_eddy(eddy_name = eddy_name)

    fn_key <- make_fn_key(sum, eddy)

    eddy$add_data(key, "foo", fn_key)

    expect_true(fn_key %in% names(eddy$cache_lst))

    delete_eddy(eddy_name = eddy_name)
})


test_that("delete_data() works", {

    eddy <- new_eddy(eddy_name = eddy_name)

    fn_key <- make_fn_key(sum, eddy)

    eddy$add_data(key, "foo", fn_key)
    eddy$delete_data(key, fn_key)

    expect_equal(eddy$find_key(key, fn_key), "missing")

    delete_eddy(eddy_name = eddy_name)
})


test_that("get_data() works", {

    eddy <- new_eddy(eddy_name = eddy_name)

    fn_key <- make_fn_key(sum, eddy)

    eddy$add_data(key, "foo", fn_key)
    data <- eddy$get_data(key, fn_key)

    expect_equal(data, "foo")

    delete_eddy(eddy_name = eddy_name)
})


context("rflow functions")

test_that("add_rflow() stops if already exist", {

    rf <- make_rflow(sum)
    rflow <- environment(rf)$self

    eddy <- new_eddy(eddy_name = eddy_name)

    expect_error(eddy$add_rflow(rflow$fn_key, rflow), NA)
    expect_error(eddy$add_rflow(rflow$fn_key, rflow))

    delete_eddy(eddy_name = eddy_name)
})


test_that("get_rflow() throws warning if not found", {

    eddy <- new_eddy(eddy_name = eddy_name)

    expect_warning(eddy$get_rflow("111111"))

    delete_eddy(eddy_name = eddy_name)
})


test_that("reset() works", {

    eddy <- new_eddy(eddy_name = eddy_name, cache_path = cache_path)
    fn_key <- make_fn_key(sum, eddy)

    rf <- make_rflow(diff)
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


context("eddy.R (R6Eddy public wrappers)")

test_that("delete_eddy() works", {

    eddy <- new_eddy(eddy_name = eddy_name)

    expect_true(base::exists(eddy_name, envir = get_default_env(), inherits = FALSE))

    delete_eddy(eddy_name = eddy_name)

    expect_false(base::exists(eddy_name, envir = get_default_env(), inherits = FALSE))
})


test_that("new_eddy() checks for already existing eddy", {

    eddy <- new_eddy(eddy_name = eddy_name)

    expect_error(new_eddy(eddy_name = eddy_name))

    delete_eddy(eddy_name = eddy_name)
})
