# Tests for flow_dfr -----------------------------------------------------------
context("flow-dfr tests")

if (digest::digest(Sys.info()[-c(2, 3)]) %in% c(
    "2e85e2a3018ecf3b2e5fc03bfb20fd39"
)) {
    skip("cache-memory-file functions")
}

setup({
    df <- tibble::as.tibble(mtcars)
    df_fn <- function(df, i = NULL) {
        if (is.null(i)) {
            dfi <- df
            dfi$rm <- rowMeans(dfi[1:10])
        } else {
            dfi <- df[i, , drop = FALSE]
        }
        dfi
    }
    
    df_fn2 <- function(df) {
        df <- df %>%
            dplyr::mutate(hp = hp + 1)
        df
    }

    assign("df", df, envir = .GlobalEnv)
    assign("df_fn", df_fn, envir = .GlobalEnv)
    assign("df_fn2", df_fn2, envir = .GlobalEnv)
})


test_that("flow_dfr() works", {
    get_current_eddy()$reset()
    dfr1 <- flow_dfr(head(df), fn = df_fn)
    
    expect_equal(dfr1$state_index, 1)
    expect_false(dfr1$is_valid)
    
    collected_dfr <- dfr1 %>% collect()
    expected_df <- head(df)
    expected_df$rm <- rowMeans(expected_df[1:10])
    
    expect_true(dfr1$is_valid)
    expect_equal(collected_dfr, expected_df)
})


test_that("flow_dfr() works with factor column", {
    get_current_eddy()$reset()
    test_df <- head(df)
    
    test_df$gear <- as.factor(test_df$gear)
    dfr1 <- flow_dfr(test_df, fn = df_fn2)
    
    expect_equal(dfr1$state_index, 1)
    expect_false(dfr1$is_valid)
    
    collected_dfr <- dfr1 %>% collect()
    expected_df <- head(df) %>%
        dplyr::mutate(hp = hp + 1,
                      gear = as.factor(gear))
    
    expect_true(dfr1$is_valid)
    expect_equal(collected_dfr, expected_df)
    expect_true(is.factor(collected_dfr$gear))
})


test_that("flow_dfr() works with second function argument", {
    get_current_eddy()$reset()
    
    df_test <- head(df)
    
    dfr1 <- flow_dfr(df_test, 1, fn = df_fn)
    
    expect_equal(dfr1$state_index, 1)
    expect_false(dfr1$is_valid)
    
    collected_dfr <- dfr1 %>% collect()
    expected_df <-  df_test[1, , drop = FALSE]
    
    expect_true(dfr1$is_valid)
    expect_equal(collected_dfr, expected_df)
})


test_that("flow_dfr() works with different states", {
    get_current_eddy()$reset()
    
    dfr1 <- flow_dfr(head(df), fn = df_fn)
    
    collected_dfr1 <- dfr1 %>% collect()
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


test_that("flow_dfr() with same name, but different body", {
    get_current_eddy()$reset()
    
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
    get_current_eddy()$reset()
    
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
    get_current_eddy()$reset()
    
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
    expect_message(dfr2 <- flow_dfr(head(df), fn = df_fn2))

    collected_dfr <- dfr2 %>% collect()
    expected_df <- head(df)
    expected_df$rm <- rowMeans(expected_df[1:10])

    expect_equal(collected_dfr, expected_df)
    expect_equal(dfr2$state_index, 1)
})


test_that("flow_dfr() stops with empty dataframe", {
    get_current_eddy()$reset()
    
    df <- data.frame()
    expect_error(dfr <- flow_dfr(df, fn = identity))
})


test_that("flow_dfr() works with 0 rows dataframe", {
    get_current_eddy()$reset()
    
    df <- data.frame(col1 = character(), col2 = character())
    dfr <- flow_dfr(df, fn = identity)
    expect_equal(collect(dfr), tibble::as_tibble(df))
})


test_that("flow_dfr() stops with non df input", {
    get_current_eddy()$reset()
    
    expect_error(dfr <- flow_dfr(NULL, fn = identity))
    expect_error(dfr <- flow_dfr(list(), fn = identity))
    expect_error(dfr <- flow_dfr(NA, fn = identity))
    expect_error(dfr <- flow_dfr(character(), fn = identity))
    expect_error(dfr <- flow_dfr(1, fn = identity))
    expect_error(dfr <- flow_dfr(TRUE, fn = identity))
})


teardown({
    get_current_eddy()$reset()
    
    base::rm(list = "df", envir = .GlobalEnv)
    base::rm(list = "df_fn", envir = .GlobalEnv)
    base::rm(list = "df_fn2", envir = .GlobalEnv)
})
