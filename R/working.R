# working ----
func_global <- function(x, l = list(a = 1:2, 3:4), td = test_dupe) {
    cat('func_global:', '\n')
    cat(exists('test_global'), '\n')
    cat(exists('test_env'), '\n')
    cat(exists('test_local'), '\n')
    cat(td, '\n')
    # browser()
    purrr::map(l, ~ . * x)
}
test_global <- 'rflow::'
test_dupe <- 'rflow::'


func_env <- function(x, l = list(a = 1:2, 3:4), td = test_dupe) {
    cat('func_env:', '\n')
    cat(exists('test_global'), '\n')
    cat(exists('test_env'), '\n')
    cat(exists('test_local'), '\n')
    cat(td, '\n')
    # browser()
    purrr::map(l, ~ . * x)
}
encl_env <- new.env(parent = baseenv())
encl_env$test_env <- 'env'
encl_env$test_dupe <- 'env'
environment(func_env) <- encl_env


test <- function() {
    
    func_local <- function(x, l = list(a = 1:2, 3:4), td = test_dupe) {
        cat('func_local:', '\n')
        cat(exists('test_global'), '\n')
        cat(exists('test_env'), '\n')
        cat(exists('test_local'), '\n')
        cat(td, '\n')
        # browser()
        purrr::map(l, ~ . * x)
    }
    test_local <- 'test()'
    test_dupe <- 'test()'
    
    rf_func_global <- make_rflow(func_global)
    res_global <- rf_func_global(x = 1)
    
    cat('\n')
    rf_func_env <- make_rflow(func_env)
    res_env <- rf_func_env(x = 2)
    
    cat('\n')
    rf_func_local <- make_rflow(func_local)
    res_local <- rf_func_local(x = 3)
    
    # browser()
}
