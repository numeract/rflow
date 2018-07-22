# Eddy functions tests ----
context("Eddy functions tests")

if (digest::digest(Sys.info()[-c(2, 3)]) %in% c(
    "2e85e2a3018ecf3b2e5fc03bfb20fd39"
)) {
    skip("cache-memory-file functions")
}


test_that("new_eddy() works", {
    eddy <- new_eddy("new_eddy_test")
    expect_equal(get_eddy("new_eddy_test"), eddy)
    delete_eddy("new_eddy_test") 
})


test_that("set_current_eddy() works", {
    eddy <- new_eddy("eddy_test")
    set_current_eddy("eddy_test")
    expect_equal(get_current_eddy(), eddy)
    delete_eddy("eddy_test") 
})


test_that("parse_flow_options() works", {
    
    eval_arg_fn <- function(x) {list(x)}
    eddy <- new_eddy(eddy_name = "test_eddy")
    options <- parse_flow_options(
        excluded_arg = "x",
        eval_arg_fn = eval_arg_fn,
        split_bare_list = TRUE,
        split_dataframe = FALSE,
        split_fn = NULL,
        eddy = eddy) 
    
    expected_val <- list(
        excluded_arg = "x",
        eval_arg_fn = eval_arg_fn,
        split_bare_list = TRUE,
        split_dataframe = FALSE,
        split_fn = NULL)
    
    expect_equal(options, expected_val)
    delete_eddy("test_eddy") 
})


test_that("parse_flow_options() works with NULL args", {

    options <- parse_flow_options(
        excluded_arg = "x",
        eval_arg_fn = NULL,
        split_bare_list = TRUE,
        split_dataframe = FALSE,
        split_fn = NULL,
        eddy = NULL) 
    
    expected_val <- list(
        excluded_arg = "x",
        eval_arg_fn = NULL,
        split_bare_list = TRUE,
        split_dataframe = FALSE,
        split_fn = NULL)
    
    expect_equal(options, expected_val)
})
