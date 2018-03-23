context("R6Eddy functions")

# default cache folder used for tests, listed in .gitignore
# R CMD does not like it, build fails with warning if folder present
# tests should delete this folder if already present, to achieve corverage 
# test must delete this folder by deleting the eddy
cache_path <- 'cache'
key <- "sum_result"
test_that("initialize works when cache_path not valid", {

    eddy <- R6Eddy$new(is_reactive = FALSE, 
                       cache_path = cache_path, 
                       algo = "xxhash64")
    
    expect_true(dir.exists(cache_path))
})


test_that("find_rflow works with memory", {
    
    rf <- make_rflow(sum)
    rflow <- environment(rf)$self
    eddy <- get_default_eddy()
    
    expect_equal(eddy$find_rflow(rflow$fn_key), "memory")
})


test_that("add_data works", {
    
    rf <- make_rflow(sum)
    rflow <- environment(rf)$self 
    eddy <- rflow$eddy
    
    eddy$add_data(key, rflow, rflow$fn_key)
    
    expect_true(rflow$fn_key %in% names(eddy$cache_lst))
    
    eddy$delete_data(key, rflow$fn_key)
})


test_that("delete_data works", {
    
    rf <- make_rflow(sum)
    rflow <- environment(rf)$self
    eddy <- rflow$eddy
    
    eddy$add_data(key, 3, rflow$fn_key)
    eddy$delete_data(key, rflow$fn_key)
    
    expect_equal(eddy$find_key(key, rflow$fn_key), "missing")
})


test_that("get_data() works", {
    
    rf <- make_rflow(sum)
    rflow <- environment(rf)$self
    
    eddy <- new_eddy(eddy_name = "custom")
    
    eddy$add_data(key, 4, rflow$fn_key)
    data <- eddy$get_data(key, rflow$fn_key)
    
    expect_equal(data, 4)
    
    delete_eddy(eddy)
})


context("rflow functions")

test_that("add_rflow stops if already exist", {
    
    rf <- make_rflow(sum)
    rflow <- environment(rf)$self
    eddy <- rflow$eddy
    
    expect_error(eddy$add_rflow(rflow$fn_key, rflow))
})


test_that("get_flow throws warning if not found", {
    
    rf <- make_rflow(sum)
    rflow <- environment(rf)$self
    eddy <- rflow$eddy
    
    expect_warning(eddy$get_rflow("111111"))
})

# clean up test cache folder created
unlink(cache_path, recursive = TRUE)


test_that("reset() works", {
    
    eddy <- new_eddy(cache_path = cache_path)
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
    
    delete_eddy(eddy)
})
