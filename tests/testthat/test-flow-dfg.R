# Tests for flow-dfg -----------------------------------------------------------

context("flow-dfg tests")

# if (digest::digest(Sys.info()[-c(2, 3)]) %in% c(
#     "2e85e2a3018ecf3b2e5fc03bfb20fd39"
# )) {
#     skip("cache-memory-file functions")
# }


setup({
    df <- tibble::as.tibble(iris)
    set.seed(1)
    rand <- sample(nrow(df))
    df <- df[rand, ]
    df_fn <- function(df) {
        df$Sepal.Length  <- df$Sepal.Length * 2
        df
    }
    
    identity_df <- function(df) {df}
    assign("df", df, envir = .GlobalEnv)
    assign("df_fn", df_fn, envir = .GlobalEnv)
    assign("identity_df", identity_df, envir = .GlobalEnv)
})


test_that("flow_dfg works", {
    
    dfg1 <- flow_dfg(head(df, n = 35), fn = df_fn, group_by = "Species")
    
    expect_equal(dfg1$state_index, 1)
    expect_false(dfg1$is_valid)
    
    collected_dfg <- dfg1 %>% collect()
    expected_df <- head(df, n = 35)
    expected_df <- expected_df %>%
        dplyr::mutate(Sepal.Length  = Sepal.Length * 2)
    
    expect_true(dfg1$is_valid)
    expect_equal(collected_dfg, expected_df)
    
    dfg1$eddy$reset()
})


test_that("flow_dfg works without group_by argument but already grouped df", {
    
    dfg_test <- head(df, n = 35) %>%
        dplyr::group_by(Species)
    
    dfg1 <- flow_dfg(dfg_test, fn = df_fn)
    
    expect_equal(dfg1$state_index, 1)
    expect_false(dfg1$is_valid)
    
    collected_dfg <- dfg1 %>% collect()
    expected_df <- head(df, n = 35)
    expected_df <- expected_df %>%
        dplyr::mutate(Sepal.Length  = Sepal.Length * 2)
    
    expect_true(dfg1$is_valid)
    expect_equal(collected_dfg, expected_df)
    
    dfg1$eddy$reset()
})


test_that("flow_dfg stops with NULL group_by  and non-grouped df", {
    
    dfg_test <- head(df, n = 35) 
    
    dfg1 <- flow_dfg(dfg_test, fn = df_fn)
    
    expect_equal(dfg1$state_index, 1)
    expect_false(dfg1$is_valid)
    
    expect_error(collected_dfg <- dfg1 %>% collect())
    
    dfg1$eddy$reset()
})


test_that("flow_dfg stops with non valid df argument", {
    
    dfg_test <- list(col1 = c(1, 2, 3)) 
    expect_error(dfg1 <- flow_dfg(dfg_test, fn = df_fn))
    
    dfg_test <- data.frame()
    expect_error(dfg1 <- flow_dfg(dfg_test, fn = df_fn))
})


test_that("flow_dfg stops with non valid group_by", {
    dfg_test <- head(df, n = 35) 
    
    expect_error(
        dfg1 <- flow_dfg(dfg_test, fn = df_fn, group_by = NA))
    expect_error(
        dfg1 <- flow_dfg(dfg_test, fn = df_fn, group_by = NA_character_))
    expect_error(
        dfg1 <- flow_dfg(dfg_test, fn = df_fn, group_by = character()))
    expect_error(
        dfg1 <- flow_dfg(dfg_test, fn = df_fn, group_by = 2))
    expect_error(
        dfg1 <- flow_dfg(dfg_test, fn = df_fn, group_by = TRUE))
    expect_error(
        dfg1 <- flow_dfg(dfg_test, fn = df_fn, group_by = "inexistent_col"))
    
})



teardown({
    base::rm(list = "df", envir = .GlobalEnv)
    base::rm(list = "df_fn", envir = .GlobalEnv)
    base::rm(list = "identity_df", envir = .GlobalEnv)
})
