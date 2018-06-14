# Integrated tests for cacheing ------------------------------------------------

context("Integrated tests")


setup({
    cache_dir <- "cache_dir"
    cache_fmem_test <- cache_memory_file(cache_dir)
    test_eddy <- use_eddy("test_eddy", cache = cache_fmem_test)
    
    df <- tibble::as.tibble(iris)
    df <- df[c(1:7, 71:73, 101:110), ]
    file_path <- as.character(fs::path(tempfile(pattern = "test-rflow-")))
    
    df_fn <- function(df, i = NULL) {
        if (is.null(i)) {
            dfi <- df
        } else {
            dfi <- df[i, , drop = FALSE]
        }
        dfi %>% 
            dplyr::group_by(Species) %>%
            dplyr::mutate(rm = mean(Sepal.Length))
    }
    
    assign("cache_fmem_test", cache_fmem_test, envir = .GlobalEnv)
    assign("cache_dir", cache_dir, envir = .GlobalEnv)
    assign("file_path", file_path, envir = .GlobalEnv)
    assign("df", df, envir = .GlobalEnv)
    assign("df_fn", df_fn, envir = .GlobalEnv)
})


test_that("cacheing flow works", {
  
    # pass #1
    write.csv(df, file_path, row.names = FALSE)
    eddy <- get_current_eddy()
    env <- new.env()
    df[, "Species"] <- as.character(df[, "Species"])
    test_rflow <- 
        file_path %>%
        flow_file_source() %>%
        flow_fn(stringsAsFactors = FALSE, fn = read.csv) %>%
        flow_dfg(1:3, fn = df_fn, group_by = "Species") %>%
        flow_dfr(fn = identity) %>%
        flow_ns_sink("collected_result", ns = env)
    
    expect_equal(length(eddy$flow_lst), 5)
    
    flow1 <- eddy$flow_lst[[1]]
    flow2 <- eddy$flow_lst[[2]]
    flow3 <- eddy$flow_lst[[3]]
    flow4 <- eddy$flow_lst[[4]]
    flow5 <- eddy$flow_lst[[5]]
    
    collected_flow1 <- flow1 %>% collect()
    collected_flow2 <- flow2 %>% collect()
    collected_flow3 <- flow3 %>% collect()
    collected_flow4 <- flow4 %>% collect()
    
    row_hash1 <- flow4$out_df[1, "..row_hash.."]
    expected_df <-  df[1:3, , drop = FALSE]
    expected_df <- expected_df %>% 
        dplyr::group_by(Species) %>%
        dplyr::mutate(rm = mean(Sepal.Length))
    expected_df[ ,"Species"] <- as.character(expected_df[ ,"Species"])
    
    expect_true(test_rflow$is_valid)
    expect_equal(nrow(env[["collected_result"]]), 3)
    expect_true(fs::is_absolute_path(collected_flow1))
    expect_equal(nrow(collected_flow2), 20)
    expect_equal(nrow(flow3$out_df), 3)
    expect_equal(nrow(flow4$out_df), 3)
    
    # pass #2
    df[1, "Species"] <- "versicolor"
    write.csv(df, file_path, row.names = FALSE)
    
    eddy <- use_eddy("test_eddy2", cache = cache_fmem_test)
    test_rflow <- 
        file_path %>%
        flow_file_source() %>%
        flow_fn(stringsAsFactors = FALSE, fn = read.csv) %>%
        flow_dfg(1:3, fn = df_fn, group_by = "Species") %>%
        flow_dfr(fn = identity)  %>%
        flow_ns_sink("collected_result")
    
    expect_equal(length(eddy$flow_lst), 5)
   
    
    flow1 <- eddy$flow_lst[[1]]
    flow2 <- eddy$flow_lst[[2]]
    flow3 <- eddy$flow_lst[[3]]
    flow4 <- eddy$flow_lst[[4]]
    
    
    collected_flow1 <- flow1 %>% collect()
    collected_flow2 <- flow2 %>% collect()
    collected_flow3 <- flow3 %>% collect()
    collected_flow4 <- flow4 %>% collect()
    
    row_hash2 <- flow4$out_df[2, "..row_hash.."]
    expect_false(row_hash1 == row_hash2)
    expect_equal(nrow(flow3$out_df), 6)
    expect_equal(nrow(flow4$out_df), 6)
    expect_equal(nrow(env[["collected_result"]]), 3)
    expect_true(fs::is_absolute_path(collected_flow1))
    expect_equal(nrow(collected_flow2), 20)
})


teardown({
    unlink(file_path, force = TRUE)
    get_current_eddy()$terminate()
    set_current_eddy("default_eddy")
    
    base::rm(list = "cache_fmem_test", envir = .GlobalEnv)
    base::rm(list = "cache_dir", envir = .GlobalEnv)
    base::rm(list = "file_path", envir = .GlobalEnv)
    base::rm(list = "df", envir = .GlobalEnv)
    base::rm(list = "df_fn", envir = .GlobalEnv)
})
