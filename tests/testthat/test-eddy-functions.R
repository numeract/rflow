# Eddy functions tests----------------------------------------------------------
context("Eddy functions tests")

test_that("parse_flow_options() works", {
    eval_arg_fn <- function(x) {list(x)}
    eddy <- new_eddy(eddy_name = "test_eddy")
    options <- parse_flow_options(excluded_arg = "x",
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
    eddy$reset()
    
    # delete_eddy() seems to have probles
})
