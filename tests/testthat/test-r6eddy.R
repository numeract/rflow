context("r6eddy")

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


test_that("delete_data works", {
    rf <- make_rflow(sum)
    rflow <- environment(rf)$self
    
    rflow$eddy$cache_path <- 'folder'
    rflow$eddy$add_data("sum_result", rflow, rflow$fn_key)
    browser()
    rflow$eddy$delete_data("sum_result",  rflow$fn_key)
    
    expect_false(rflow$fn_key %in% names(rflow$eddy$cache_lst))
    # expect_error(readRDS(file = file.path(rflow$eddy$cache_path, 
    #                                       rflow$fn_key, paste0("sum_result", ".rds"))))
    
    rflow$eddy$cache_path <- NULL
})
