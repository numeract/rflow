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
    
    identity_df <- function(df) {df}
    assign("df", df, envir = .GlobalEnv)
    assign("df_fn", df_fn, envir = .GlobalEnv)
    assign("identity_df", identity_df, envir = .GlobalEnv)
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
    expect_equal(nrow(dfr1$out_df), 6)
    
    expect_message(dfr1 <- flow_dfr(tail(df), fn = df_fn))
    
    collected_dfr1 <- dfr1 %>% collect()
    expect_equal(dfr1$state_index, 2)
    expect_equal(nrow(dfr1$out_df), 12)
    expected_df <- tail(df)
    expected_df$rm <- rowMeans(expected_df[1:10])
    
    expect_true(dfr1$is_valid)
    expect_equal(collected_dfr1, expected_df)
})


test_that("flow_dfr() stops with anonymus function", {
    expect_error(dfr1 <- flow_dfr(head(df), function(x){x}))
})

test_that("flow_dfr() stops with primitive function", {
    expect_error(dfr1 <- flow_dfr(head(df), sum))
})


test_that("flow_dfr() with same name, but different body", {
    dfr_test <- flow_dfr(head(df), fn = df_fn)
    expect_equal(dfr_test$state_index, 1)
    
    df_fn <- function(df, i = NULL) {
        if (is.null(i)) {
            dfi <- df
        } else {
            dfi <- df[i, , drop = FALSE]
        }
        
        dfi$mr <- rowSums(dfi[1:10])
        dfi
    }
    assign("df_fn", df_fn, envir = .GlobalEnv)
    
    expect_message(dfr_test <- flow_dfr(head(df), fn = df_fn))
    collected_dfr <- dfr_test %>% 
        collect()
    
    expected_df <- head(df)
    expected_df$mr <- rowSums(expected_df[1:10])
    
    expect_equal(dfr_test$state_index, 1)
    expect_equal(collected_dfr, expected_df)
    
    df_fn <- function(df, i = NULL) {
        if (is.null(i)) {
            dfi <- df
        } else {
            dfi <- df[i, , drop = FALSE]
        }
        dfi$rm <- rowMeans(dfi[1:10])
        dfi
    }
    
    assign("df_fn", df_fn, envir = .GlobalEnv)
})

test_that("flow_dfr() with same name, but different body with id", {
    dfr_test <- flow_dfr(head(df), fn = df_fn)
    expect_equal(dfr_test$state_index, 1)
    
    df_fn <- function(df, i = NULL) {
        if (is.null(i)) {
            dfi <- df
        } else {
            dfi <- df[i, , drop = FALSE]
        }
        
        dfi$mr <- rowSums(dfi[1:10])
        dfi
    }
    assign("df_fn", df_fn, envir = .GlobalEnv)
    
    expect_silent(
        dfr_test <- flow_dfr(head(df), fn = df_fn, fn_id = "id1"))
    collected_dfr <- dfr_test %>% 
        collect()
    
    expected_df <- head(df)
    expected_df$mr <- rowSums(expected_df[1:10])
    
    expect_equal(dfr_test$state_index, 1)
    expect_equal(collected_dfr, expected_df)
    
    df_fn <- function(df, i = NULL) {
        if (is.null(i)) {
            dfi <- df
        } else {
            dfi <- df[i, , drop = FALSE]
        }
        dfi$rm <- rowMeans(dfi[1:10])
        dfi
    }
    
    assign("df_fn", df_fn, envir = .GlobalEnv)
})


test_that("flow_dfr() works with same body, different name", {
    df_fn2 <- function(df, i = NULL) {
        if (is.null(i)) {
            dfi <- df
        } else {
            dfi <- df[i, , drop = FALSE]
        }
        dfi$rm <- rowMeans(dfi[1:10])
        dfi
    }
    assign("df_fn2", df_fn, envir = .GlobalEnv)
    
    dfr1 <- flow_dfr(head(df), fn = df_fn)
    
    expect_equal(dfr1$state_index, 1)
    
    # Instead of showing a message and creating new cache, returns an error
    expect_message(dfr2 <- flow_dfr(head(df), fn = df_fn2))
    
    collected_dfr <- dfr2 %>% collect()
    expected_df <- head(df)
    expected_df$rm <- rowMeans(expected_df[1:10])
    
    expect_equal(collected_dfr, expected_df)
    expect_equal(dfr2$state_index, 1)
})


test_that("flow_dfr() stops with empty dataframe", {
    df <- data.frame()
    expect_error(dfr <- flow_dfr(df, fn = identity_df))
})


test_that("flow_dfr() stops with 0 rows dataframe", {
    df <- data.frame(col1 = c(), col2 = c())
    expect_error(dfr <- flow_dfr(df, fn = identity_df))
})


test_that("flow_dfr() stops with non df input", {
    expect_error(dfr <- flow_dfr(NULL, fn = identity_df))
    expect_error(dfr <- flow_dfr(c(), fn = identity_df))
    expect_error(dfr <- flow_dfr(list(), fn = identity_df))
    expect_error(dfr <- flow_dfr(NA, fn = identity_df))
    expect_error(dfr <- flow_dfr(character(), fn = identity_df))
    expect_error(dfr <- flow_dfr(1, fn = identity_df))
    expect_error(dfr <- flow_dfr(TRUE, fn = identity_df))
})


teardown({
    base::rm(list = "df", envir = .GlobalEnv)
    base::rm(list = "df_fn", envir = .GlobalEnv)
    base::rm(list = "identity_df", envir = .GlobalEnv)
})
