# Tests for flow-dfg -----------------------------------------------------------

context("flow-dfg tests")

if (digest::digest(Sys.info()[-c(2, 3)]) %in% c(
    "2e85e2a3018ecf3b2e5fc03bfb20fd39"
)) {
    skip("cache-memory-file functions")
}


setup({
    df <- tibble::as.tibble(iris)
    df <- df[c(1:7, 71:73, 101:110), ]
    
    df_fn <- function(df) {
        df <- df %>%
            dplyr::group_by(Species) %>%
            dplyr::mutate(Sepal.Length = Sepal.Length * 2)
    }
    
    df_fn2 <- function(df, i = NULL) {
        if (is.null(i)) {
            dfi <- df
        } else {
            dfi <- df[i, , drop = FALSE]
        }
        dfi <- dfi %>% 
            dplyr::group_by(Species) %>%
            dplyr::mutate(rm = mean(Sepal.Length))
    }
    
    df_fn3 <- function(df) {
        df <- df %>%
            dplyr::filter(Sepal.Length < 5) %>%
            dplyr::group_by(Species) %>%
            dplyr::mutate(Sepal.Length = "small")
    }
    
    df_fn4 <- function(df) {
        if (nrow(df) > 15) {
            colnames(df)[2] <- "new_name"
        }
        df
    }
    
    df_fn5 <- function(df) {
        df2 <- dplyr::data_frame(
            Sepal.Length = 1,
            Sepal.Width = 2,
            Petal.Length = 3,
            Petal.Width = 4,
            Species = "setosa",
            stringsAsFactors = FALSE)
        
        df <- df %>% dplyr::bind_rows(df2, stringsAsFactors = FALSE)
        df
    }
    assign("df", df, envir = .GlobalEnv)
    assign("df_fn", df_fn, envir = .GlobalEnv)
    assign("df_fn2", df_fn2, envir = .GlobalEnv)
    assign("df_fn3", df_fn3, envir = .GlobalEnv)
    assign("df_fn4", df_fn4, envir = .GlobalEnv)
    assign("df_fn5", df_fn5, envir = .GlobalEnv)
})


test_that("flow_dfg works", {
    get_current_eddy()$reset()
    
    dfg1 <- flow_dfg(df, fn = df_fn, group_by = "Species")
    
    expect_equal(dfg1$state_index, 1)
    expect_false(dfg1$is_valid)
    
    collected_dfg <- dfg1 %>% collect()
    expected_df <- df %>%
        dplyr::group_by(Species) %>%
        dplyr::mutate(Sepal.Length  = Sepal.Length * 2)
    
    expect_true(dfg1$is_valid)
    expect_equal(collected_dfg, expected_df)
})


test_that("flow_dfg works without group_by argument but already grouped df", {
    get_current_eddy()$reset()
    
    dfg_test <- df %>%
        dplyr::group_by(Species)

    dfg1 <- flow_dfg(dfg_test, fn = df_fn)

    expect_equal(dfg1$state_index, 1)
    expect_false(dfg1$is_valid)

    collected_dfg <- dfg1 %>% collect()
    expected_df <- df %>%
        dplyr::group_by(Species) %>%
        dplyr::mutate(Sepal.Length  = Sepal.Length * 2)

    expect_true(dfg1$is_valid)
    expect_equal(collected_dfg, expected_df)
})


test_that("flow_dfg works with pipes", {
    get_current_eddy()$reset()
    
    dfg_test <- df %>%
        dplyr::group_by(Species)
    
    dfg <- flow_dfg(dfg_test, fn = df_fn)
    
    collected_dfg <- dfg %>%
        flow_dfg(fn = identity) %>%
        collect()
    
    expected_df <- df %>%
        dplyr::group_by(Species) %>%
        dplyr::mutate(Sepal.Length  = Sepal.Length * 2)
    
    expect_true(dfg$is_valid)
    expect_equal(collected_dfg, expected_df)
})


test_that("flow_dfg stops with NULL group_by  and non-grouped df", {
    get_current_eddy()$reset()
    
    dfg_test <- df

    dfg1 <- flow_dfg(dfg_test, fn = df_fn)

    expect_equal(dfg1$state_index, 1)
    expect_false(dfg1$is_valid)

    expect_error(collected_dfg <- dfg1 %>% collect())
})


test_that("flow_dfg stops with non valid df argument", {
    get_current_eddy()$reset()
    
    dfg_test <- list(col1 = c(1, 2, 3))
    expect_error(dfg <- flow_dfg(dfg_test, fn = df_fn))

    dfg_test <- data.frame()
    expect_error(dfg <- flow_dfg(dfg_test, fn = df_fn))

    expect_error(dfg <- flow_dfg(NULL, fn = identity_df))
    expect_error(dfg <- flow_dfg(list(), fn = identity_df))
    expect_error(dfg <- flow_dfg(NA, fn = identity_df))
    expect_error(dfg <- flow_dfg(character(), fn = identity_df))
    expect_error(dfg <- flow_dfg(1, fn = identity_df))
    expect_error(dfg <- flow_dfg(TRUE, fn = identity_df))
})


test_that("flow_dfg stops with non valid group_by", {
    get_current_eddy()$reset()
    dfg_test <-  df

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


test_that("flow_dfg works with function that adds a column", {
    get_current_eddy()$reset()
    dfg_test <- df %>%
        dplyr::group_by(Species)
    
    dfg1 <- flow_dfg(dfg_test, fn = df_fn2)
    
    collected_dfg <- dfg1 %>% collect()
    expected_df <- df %>% 
        dplyr::group_by(Species) %>%
        dplyr::mutate(rm = mean(Sepal.Length))
    
    expect_true(dfg1$is_valid)
    expect_equal(collected_dfg, expected_df)
})


test_that("flow_dfg stops with function that adds new row", {
    dfg_test <- df %>%
        dplyr::group_by(Species)

    dfg1 <- flow_dfg(dfg_test, fn = df_fn5)

    expect_error(dfg1 %>% collect())

    dfg1$eddy$reset()
})


teardown({
    base::rm(list = "df", envir = .GlobalEnv)
    base::rm(list = "df_fn", envir = .GlobalEnv)
    base::rm(list = "df_fn2", envir = .GlobalEnv)
    base::rm(list = "df_fn3", envir = .GlobalEnv)
    base::rm(list = "df_fn4", envir = .GlobalEnv)
    base::rm(list = "df_fn5", envir = .GlobalEnv)
})
