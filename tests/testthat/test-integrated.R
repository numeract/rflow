# Integrated tests for cacheing ------------------------------------------------

context("Integrated tests")

setup({
    cache_dir <- "cache_dir"
    fn_group <- "default_group"
    df <- tibble::as.tibble(iris)
    df <- df[c(1:7, 71:73, 101:110), ]
    file1 <- tempfile(pattern = "test-rflow-")
    file_path <- as.character(fs::path(file1))
    
    df_fn <- function(df, i = NULL) {
        if (is.null(i)) {
            dfi <- df
        } else {
            dfi <- df[i, , drop = FALSE]
        }
        dfi <- dfi %>% 
            dplyr::group_by(Species) %>%
            dplyr::mutate(rm = mean(Sepal.Length))
    }
    cache_fmem_test <- cache_memory_file(cache_dir)
    test_eddy <- use_eddy("test_eddy", cache = cache_fmem_test)
    
    assign("cache_fmem_test", cache_fmem_test, envir = .GlobalEnv)
    assign("df", df, envir = .GlobalEnv)
    assign("cache_dir", cache_dir, envir = .GlobalEnv)
    assign("file1", file1, envir = .GlobalEnv)
    assign("file_path", file_path, envir = .GlobalEnv)
    assign("df_fn", df_fn, envir = .GlobalEnv)
})


test_that("cacheing flow works", {
   
    write.csv(df, file1, row.names = FALSE)
    
    test_rflow <- flow_file_source(file_path) %>%
        flow_fn(fn = read.csv) %>%
        flow_dfg(1:3, fn = df_fn, group_by = "Species") %>%
        flow_dfr(1, fn = df_fn) 
    
    eddy <- get_current_eddy()
  
    expect_equal(length(eddy$flow_lst), 4)
    collected_result <- test_rflow %>% collect()
    
    flow1 <- eddy$flow_lst[[1]]
    flow2 <- eddy$flow_lst[[2]]
    flow3 <- eddy$flow_lst[[3]]
    flow4 <- eddy$flow_lst[[4]]
    row_hash1 <- flow4$out_df[1, "..row_hash.."]
    expect_equal(nrow(flow4$out_df), 1)
    
    test_eddy <- NULL
    test_eddy2 <- use_eddy("test_eddy2", cache = cache_fmem_test)
    eddy <- get_current_eddy()
    
    df[1, "Petal.Length"] <-  10
    write.csv(df, file1, row.names = FALSE)
    
    test_rflow <- flow_file_source(file_path) %>%
        flow_fn(fn = read.csv) %>%
        flow_dfg(1:3, fn = df_fn, group_by = "Species") %>%
        flow_dfr(1, fn = df_fn) 
    
    collected_result <- test_rflow %>% collect()
    
    flow1 <- eddy$flow_lst[[1]]
    flow2 <- eddy$flow_lst[[2]]
    flow3 <- eddy$flow_lst[[3]]
    flow4 <- eddy$flow_lst[[4]]
    
    row_hash2 <- flow4$out_df[2, "..row_hash.."]
    expect_false(row_hash1 == row_hash2)
    expect_equal(nrow(flow4$out_df), 2)
})


teardown({
    unlink(cache_dir)
    unlink(file_path)
    get_current_eddy()$terminate()
})
