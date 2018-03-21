context("R6Eddy functions")

# default cache folder used for tests
cache_path <- 'cache_folder'
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
    
    rf <- make_rflow(sum, eddy = get_default_eddy(cache_path))
    rflow <- environment(rf)$self
    
    eddy <- get_default_eddy(cache_path)
    eddy$cache_path <- cache_path
    eddy$add_data(key, rflow, rflow$fn_key)
    
    expect_true(rflow$fn_key %in% names(eddy$cache_lst))
    expect_error(readRDS(file = file.path(eddy$cache_path,
                        rflow$fn_key, paste0(key, ".rds"))), NA)
    
    eddy$cache_path <- NULL
})


test_that("delete_data works", {
    
    rf <- make_rflow(sum, eddy = get_default_eddy(cache_path))
    rflow <- environment(rf)$self
    
    eddy <- rflow$eddy
    eddy$cache_path <- cache_path
    
    eddy$add_data(key, 3, rflow$fn_key)
    eddy$delete_data(key, rflow$fn_key)
    key_path <- file.path(eddy$cache_path, rflow$fn_key, paste0(key, ".rds"))
    
    expect_equal(eddy$find_key(key, rflow$fn_key), "missing")
    expect_false(file.exists(key_path))
    
    eddy$cache_path <- NULL
})


test_that("get_data works", {
    
    rf <- make_rflow(sum, eddy = get_default_eddy(cache_path))
    rflow <- environment(rf)$self
    eddy <- rflow$eddy
    
    eddy$cache_path <- cache_path
    eddy$add_data(key, rflow, rflow$fn_key)
    
    data <- eddy$get_data(key, rflow$fn_key)
    expect_equal(data, rflow)
    
    # delete data from memory
    cache_env <- eddy$cache_lst[[rflow$fn_key]]
    rm(list = key, envir = cache_env, inherits = TRUE)

    # check from disk
    data <- eddy$get_data(key, rflow$fn_key)
    path <- file.path(eddy$cache_path, rflow$fn_key, paste0(key, ".rds"))
    expect_equal(data, readRDS(file = path))
    
    # delete from disk and memory
    cache_env <- eddy$cache_lst[[rflow$fn_key]]
    rm(list = key, envir = cache_env, inherits = TRUE)
    unlink(path)
    
    expect_error(eddy$get_data(key, rflow$fn_key))

    eddy$add_data(key, rflow, rflow$fn_key)
    eddy$cache_lst <- NULL
    
    data <- eddy$get_data(key, rflow$fn_key)
    expect_equal(data, rflow)
    
    rflow$eddy$cache_path <- NULL
})


context("rflow functions")


test_that("add_rflow stops if already exist", {
    
    rf <- make_rflow(sum)
    rflow <- environment(rf)$self
    
    expect_error(rflow$eddy$add_rflow(rflow$fn_key))
})


test_that("get_flow throws warning if not found", {
    
    rf <- make_rflow(sum)
    rflow <- environment(rf)$self
    
    expect_warning(rflow$eddy$get_rflow("111111"))
})


test_that("find_rflow doesn't work for wrong path", {
    
    rf <- make_rflow(sum, eddy = get_default_eddy(cache_path))
    rflow <- environment(rf)$self
    
    eddy <- rflow$eddy
    eddy$cache_path <- cache_path

    # Simulate deletion from memory (delete_rflow is not ready yet)
    eddy$rflow_lst[[rflow$fn_key]] <- NULL
    expect_equal(eddy$find_rflow(rflow$fn_key), 'disk')

    eddy$cache_path <- "wrong_path"
    expect_equal(eddy$find_rflow(rflow$fn_key), 'missing')

    eddy$cache_path <- NULL
})


# clean up test cache folder created
unlink(cache_path, recursive = TRUE)
