# Tests for flow-dfg -----------------------------------------------------------

context("flow-dfg tests")

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
})

teardown({
    base::rm(list = "df", envir = .GlobalEnv)
    base::rm(list = "df_fn", envir = .GlobalEnv)
    base::rm(list = "identity_df", envir = .GlobalEnv)
})
