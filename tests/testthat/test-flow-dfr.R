# Tests for flow_dfr -----------------------------------------------------------
context("flow-dfr tests")

setup({
    df <- tibble::as.tibble(mtcars)
    df_fn <- function(df, i = NULL) {
        if (is.null(i)) {
            dfi <- df
        } else {
            dfi <- df[i, , drop = FALSE]
        }
        dfi$rm <- rowMeans(dfi[1:10])
        dfi
    }
    
    assign("df", df, envir = .GlobalEnv)
    assign("df_fn", df_fn, envir = .GlobalEnv)
})


test_that("flow_dfr() works", {
    dfr1 <- flow_dfr(head(df), fn = df_fn)
    
    expect_equal(dfr1$state_index, 1)
    expect_false(dfr1$is_valid)
    
    collected_dfr <- dfr1 %>% collect()
    expected_df <- head(df)
    expected_df$rm <- rowMeans(expected_df[1:10])
    
    expect_true(dfr1$is_valid)
    expect_equal(collected_dfr, expected_df)
})


test_that("flow_dfr() works with different states", {
    dfr1 <- flow_dfr(head(df), fn = df_fn)
    
    expect_equal(dfr1$state_index, 1)
    
    dfr1 <- flow_dfr(tail(df), fn = df_fn)
    
    collected_dfr1 <- dfr1 %>% collect()
    expect_equal(dfr1$state_index, 2)
    
    expected_df <- tail(df)
    expected_df$rm <- rowMeans(expected_df[1:10])
    
    expect_true(dfr1$is_valid)
    expect_equal(collected_dfr1, expected_df)
})


test_that("flow_dfr() stops with primitive function", {
    expect_error(dfr1 <- flow_dfr(head(df), fn = sum))
})


teardown({
    base::rm(list = df, envir = .GlobalEnv)
    base::rm(list = df_fn, envir = .GlobalEnv)
})
