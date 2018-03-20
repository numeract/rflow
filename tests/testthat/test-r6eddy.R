context("key functions")

test_that("initialize works when cache_path not valid", {
    cache_path <- 'folder'
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
    
    rflow$eddy$cache_path <- 'folder'
    rflow$eddy$add_data("sum_result", rflow, rflow$fn_key)
    
    expect_true(rflow$fn_key %in% names(rflow$eddy$cache_lst))
    expect_error(readRDS(file = file.path(rflow$eddy$cache_path,
                        rflow$fn_key, paste0("sum_result", ".rds"))), NA)
    
    rflow$eddy$cache_path <- NULL
})


# test_that("delete_data works", {
#     rf <- make_rflow(sum)
#     rflow <- environment(rf)$self
# 
#     rflow$eddy$cache_path <- 'folder'
#     
#     rflow$eddy$add_data("sum_result", rflow, rflow$fn_key)
#     rflow$eddy$delete_data("sum_result", rflow$fn_key)
# 
#     expect_equal(rflow$eddy$find_key("sum_result", rflow$fn_key), "missing")
# 
#     rflow$eddy$cache_path <- NULL
# })


test_that("get_data works", {
    rf <- make_rflow(sum)
    rflow <- environment(rf)$self
    
    rflow$eddy$cache_path <- 'folder'
    
    rflow$eddy$add_data("sum_result", rflow, rflow$fn_key)
    
    data <- rflow$eddy$get_data("sum_result", rflow$fn_key)
    expect_equal(data, rflow)
    
    # delete data from memory
    cache_env <- rflow$eddy$cache_lst[[rflow$fn_key]]
    rm("sum_result", envir = cache_env, inherits = TRUE)

    # check from disk
    data <- rflow$eddy$get_data("sum_result", rflow$fn_key)
    path <- file.path(rflow$eddy$cache_path, rflow$fn_key, "sum_result.rds")
    expect_equal(data, readRDS(file = path))
    
    # delete from disk and memory
    cache_env <- rflow$eddy$cache_lst[[rflow$fn_key]]
    rm("sum_result", envir = cache_env, inherits = TRUE)
    unlink(path)
    
    expect_error(rflow$eddy$get_data("sum_result", rflow$fn_key))

    rflow$eddy$add_data("sum_result", rflow, rflow$fn_key)
    
    rflow$eddy$cache_lst <- NULL
    
    data <- rflow$eddy$get_data("sum_result", rflow$fn_key)
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

test_that("find_rflow doesn't find", {
    rf <- make_rflow(sum)
    rflow <- environment(rf)$self
    
    rflow$eddy$cache_path <- 'folder'

    # Simulate deletion from memory (delete_rflow is not ready yet)
    rflow$eddy$rflow_lst[[rflow$fn_key]] <- NULL

    expect_equal(rflow$eddy$find_rflow(rflow$fn_key), 'disk')

    rflow$eddy$cache_path <- "wrong_path"

    expect_equal(rflow$eddy$find_rflow(rflow$fn_key), 'missing')

    rflow$eddy$cache_path <- NULL
})

