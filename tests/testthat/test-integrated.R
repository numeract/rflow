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
    
    assign("cache_fmem_test", cache_fmem_test, envir = .GlobalEnv)
    assign("df", df, envir = .GlobalEnv)
    assign("cache_dir", cache_dir, envir = .GlobalEnv)
    assign("file1", file1, envir = .GlobalEnv)
    assign("file_path", file_path, envir = .GlobalEnv)
    assign("df_fn", df_fn, envir = .GlobalEnv)
})


test_that("cacheing flow works", {
   
    use_eddy("current_eddy", cache = cache_fmem_test)
    
    write.csv(df, file1, row.names = FALSE)
    
    test_rflow <- flow_file_source(file_path) %>%
        flow_fn(fn = read.csv) %>%
        flow_dfg(1:3, fn = df_fn, group_by = "Species") %>%
        flow_dfr(1, fn = df_fn) 
    
    eddy <- get_current_eddy()
    
    flow1 <- eddy$flow_lst[1]
    flow2 <- eddy$flow_lst[2]
    flow3 <- eddy$flow_lst[3]
    flow4 <- eddy$flow_lst[4]
    
    expect_equal(length(eddy$flow_lst), 4)
    collected_result <- test_rflow %>% collect()
    
    
    base::rm(list = "eddy", envir = .GlobalEnv)
    
    df[1, "Petal.Length"] <-  10
    write.csv(df, file1, row.names = FALSE)
    
    test_rflow <- flow_file_source(file_path) %>%
        flow_fn(fn = read.csv) %>%
        flow_dfg(1:3, fn = df_fn, group_by = "Species") %>%
        flow_dfr(1, fn = df_fn) 
})


teardown({
    unlink(cache_dir)
    unlink(file_path)
    get_current_eddy()$terminate()
})
