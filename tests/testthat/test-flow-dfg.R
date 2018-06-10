# Tests for flow-dfg -----------------------------------------------------------

context("flow-dfg tests")

if (digest::digest(Sys.info()[-c(2, 3)]) %in% c(
    "2e85e2a3018ecf3b2e5fc03bfb20fd39"
)) {
    skip("cache-memory-file functions")
}

get_hash <- function(df, filter_by, hash_of) {
    hash_of <- rlang::sym(hash_of)
    
    hash <- df %>%
        dplyr::group_by(Species) %>%
        dplyr::filter(Species == filter_by) %>%
        dplyr::distinct((!!hash_of))
    hash
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
            dplyr::group_by(Species) 
        df[1, "Petal.Length"] <- "large"
        df
    }
    
    df_fn4 <- function(df) {
        if (nrow(df) > 15) {
            colnames(df)[2] <- "new_name"
        }
        df
    }
    
    df_fn5 <- function(df) {
        df <- df %>%
            dplyr::add_row(
                Sepal.Length = 5, Sepal.Width = 6, Petal.Length = 7, 
                Petal.Width = 4, Species = "setosa")
    }
    
    df_fn6 <- function(df) {
        df <- df[0, , drop = FALSE]
    }
    
    assign("df", df, envir = .GlobalEnv)
    assign("df_fn", df_fn, envir = .GlobalEnv)
    assign("df_fn2", df_fn2, envir = .GlobalEnv)
    assign("df_fn3", df_fn3, envir = .GlobalEnv)
    assign("df_fn4", df_fn4, envir = .GlobalEnv)
    assign("df_fn5", df_fn5, envir = .GlobalEnv)
    assign("df_fn6", df_fn6, envir = .GlobalEnv)
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


test_that("flow_dfg works with function with second argument", {
    get_current_eddy()$reset()
    
    dfg1 <- flow_dfg(df, i = 1, fn = df_fn2, group_by = "Species")
    
    expect_equal(dfg1$state_index, 1)
    expect_false(dfg1$is_valid)
    
    collected_dfg <- dfg1 %>% collect()
    expected_df <- df[1, , drop = FALSE] %>%
        dplyr::mutate(rm = mean(Sepal.Length))
    
    expect_true(dfg1$is_valid)
    expect_equal(collected_dfg, expected_df)
    expect_equal(nrow(dfg1$out_df), 1)
})


test_that("flow_dfg works when adding new row", {
    get_current_eddy()$reset()
    
    test_df <- df
    dfg1 <- flow_dfg(test_df, fn = df_fn, group_by = "Species")
    collected_dfg <- dfg1 %>% collect()
    
    group_hash <- get_hash(
        df = dfg1$out_df, filter_by = "setosa", hash_of = "..group_hash..")
    
    test_df <- test_df %>%
        dplyr::add_row(
            Sepal.Length = 5, Sepal.Width = 6, Petal.Length = 7, 
            Petal.Width = 4, Species = "setosa")
    dfg1 <- flow_dfg(test_df, fn = df_fn, group_by = "Species")
    collected_dfg <- dfg1 %>% collect()
    
    group_hash2 <- get_hash(
        df = dfg1$out_df, filter_by = "setosa", hash_of = "..group_hash..")
        
    expect_equal(nrow(group_hash), 1)
    expect_equal(nrow(group_hash2), 2)
    expect_equal(nrow(dfg1$out_df), 28)
    expect_equal(dfg1$state_index, 2)
})


test_that("flow_dfg works when deleting row", {
    get_current_eddy()$reset()
    
    test_df <- df
    dfg1 <- flow_dfg(test_df, fn = df_fn, group_by = "Species")
    collected_dfg <- dfg1 %>% collect()
    
    group_hash <- get_hash(
        df = dfg1$out_df, filter_by = "setosa", hash_of = "..group_hash..")
    
    test_df <- test_df[-1, ]
    dfg1 <- flow_dfg(test_df, fn = df_fn, group_by = "Species")
    collected_dfg <- dfg1 %>% collect()
    
    group_hash2 <- get_hash(
        df = dfg1$out_df, filter_by = "setosa", hash_of = "..group_hash..")
    
    expect_equal(nrow(group_hash), 1)
    expect_equal(nrow(group_hash2), 2)
    expect_equal(nrow(dfg1$out_df), 26)
    expect_equal(dfg1$state_index, 2)
})



test_that("flow_dfg works when changing existing row", {
    get_current_eddy()$reset()
    
    test_df <- df
    dfg1 <- flow_dfg(test_df, fn = df_fn, group_by = "Species")
    collected_dfg <- dfg1 %>% collect()
    
    group_hash <- get_hash(
        df = dfg1$out_df, filter_by = "setosa", hash_of = "..group_hash..")
    row_hash <- get_hash(
        df = dfg1$out_df, filter_by = "setosa", hash_of = "..row_hash..")
    
    test_df[1, "Sepal.Length"] <- 3
    dfg1 <- flow_dfg(test_df, fn = df_fn, group_by = "Species")
    collected_dfg <- dfg1 %>% collect()
    
    group_hash2 <- get_hash(
        df = dfg1$out_df, filter_by = "setosa", hash_of = "..group_hash..")
    row_hash2 <- get_hash(
        df = dfg1$out_df, filter_by = "setosa", hash_of = "..row_hash..")
    
    expect_equal(nrow(group_hash), 1)
    expect_equal(nrow(group_hash2), 2)
    expect_equal(nrow(row_hash), 7)
    expect_equal(nrow(row_hash2), 8)
    expect_equal(nrow(dfg1$out_df), 27)
    expect_equal(dfg1$state_index, 2)
})



test_that("flow_dfg works when moving row from one group to another", {
    get_current_eddy()$reset()
    
    test_df <- df
    dfg1 <- flow_dfg(test_df, fn = df_fn, group_by = "Species")
    collected_dfg <- dfg1 %>% collect()
    
    setosa_group_hash <- get_hash(
        df = dfg1$out_df, filter_by = "setosa", hash_of = "..group_hash..")
    versicolor_group_hash <- get_hash(
        df = dfg1$out_df, filter_by = "versicolor", hash_of = "..group_hash..")
    row_hash1 <- get_hash(
        df = dfg1$out_df, filter_by = "versicolor", hash_of = "..row_hash..")
    
    test_df[2, "Species"] <- "versicolor"
    dfg1 <- flow_dfg(test_df, fn = df_fn, group_by = "Species")
    collected_dfg <- dfg1 %>% collect()
    
    setosa_group_hash2 <- get_hash(
        df = dfg1$out_df, filter_by = "setosa", hash_of = "..group_hash..")
    versicolor_group_hash2 <- get_hash(
        df = dfg1$out_df, filter_by = "versicolor", hash_of = "..group_hash..")
    row_hash2 <- get_hash(
        df = dfg1$out_df, filter_by = "versicolor", hash_of = "..row_hash..")
    
    expect_equal(nrow(setosa_group_hash), 1)
    expect_equal(nrow(versicolor_group_hash), 1)
    expect_equal(nrow(setosa_group_hash2), 2)
    expect_equal(nrow(versicolor_group_hash2), 2)
    expect_equal(nrow(row_hash1), 3)
    expect_equal(nrow(row_hash2), 4)
    expect_equal(nrow(dfg1$out_df), 30)
    expect_equal(dfg1$state_index, 2)
})



test_that("flow_dfg works with pipes", {
    get_current_eddy()$reset()
    
    test_df <- df
    test_dfg <- test_df %>%
        dplyr::group_by(Species)
    
    dfg <- flow_dfg(test_dfg, fn = df_fn)
    
    collected_dfg <- dfg %>%
        flow_dfg(fn = identity, group_by = "Species") %>%
        collect()
    
    expected_df <- test_df %>%
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

    expect_error(dfg <- flow_dfg(NULL, fn = identity))
    expect_error(dfg <- flow_dfg(list(), fn = identity))
    expect_error(dfg <- flow_dfg(NA, fn = identity))
    expect_error(dfg <- flow_dfg(character(), fn = identity))
    expect_error(dfg <- flow_dfg(1, fn = identity))
    expect_error(dfg <- flow_dfg(TRUE, fn = identity))
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
    get_current_eddy()$reset()
    
    dfg_test <- df %>%
        dplyr::group_by(Species)

    dfg1 <- flow_dfg(dfg_test, fn = df_fn5)

    expect_error(dfg1 %>% collect())
})


test_that("flow_dfg works with function that returns 0 row df", {
    get_current_eddy()$reset()
    
    dfg_test <- df %>%
        dplyr::group_by(Species)
    
    dfg1 <- flow_dfg(dfg_test, fn = df_fn6)
    
    collected_result <- dfg1 %>% collect()

    expect_true(dfg1$is_valid)
    expect_equal(nrow(dfg1$out_df), 0)

})


test_that("flow_dfg() works with same body, different name", {
    
    get_current_eddy()$reset()
    df_fn_new <- function(df, i = NULL) {
        if (is.null(i)) {
            dfi <- df
        } else {
            dfi <- df[i, , drop = FALSE]
        }
        dfi <- dfi %>% 
            dplyr::group_by(Species) %>%
            dplyr::mutate(rm = mean(Sepal.Length))
    }
    assign("df_fn_new", df_fn_new, envir = .GlobalEnv)
    
    test_dfg <- df %>%
        dplyr::group_by(Species)
    
    dfg1 <- flow_dfg(test_dfg, fn = df_fn2)
    
    expect_equal(dfg1$state_index, 1)
    expect_message(dfg2 <- flow_dfg(test_dfg, fn = df_fn_new))
    
    collected_dfr <- dfg2 %>% collect()
    expected_df <- df %>% 
        dplyr::group_by(Species) %>%
        dplyr::mutate(rm = mean(Sepal.Length))
    
    expect_equal(collected_dfr, expected_df)
    expect_equal(dfg2$state_index, 1)
})


test_that("flow_dfg() works with same name, different body", {
    get_current_eddy()$reset()
    
    test_dfg <- df %>%
        dplyr::group_by(Species)
    
    dfg1 <- flow_dfg(test_dfg, fn = df_fn2)
    expect_equal(dfg1$state_index, 1)
    
    df_fn2 <- function(df, i = NULL) {
        if (is.null(i)) {
            dfi <- df
        } else {
            dfi <- df[i, , drop = FALSE]
        }
        dfi
    }
    
    assign("df_fn2", df_fn2, envir = .GlobalEnv)
    
    expect_message(dfg1 <- flow_dfg(test_dfg, fn = df_fn2))
    collected_dfg <- dfg1 %>% collect()
    
    expected_df <- test_dfg
    expect_equal(dfg1$state_index, 1)
    expect_equal(collected_dfg, expected_df)
    
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
    assign("df_fn2", df_fn2, envir = .GlobalEnv)
})

# test_that("flow_dfg stops with function that modifies column type", {
#     get_current_eddy()$reset()
#     
#     dfg_test <- df %>%
#         dplyr::group_by(Species)
#     
#     dfg1 <- flow_dfg(dfg_test, fn = df_fn3)
#     expect_error(dfg1 %>% collect())
# })


# test_that("flow_dfg stops with function that changes column name", {
#     get_current_eddy()$reset()
#     dfg_test <- head(df, n = 35) %>%
#         dplyr::group_by(Species)
#     
#     dfg1 <- flow_dfg(dfg_test, fn = df_fn4)
#     
#     expect_error(dfg1 %>% collect())
# })

teardown({
    base::rm(list = "df", envir = .GlobalEnv)
    base::rm(list = "df_fn", envir = .GlobalEnv)
    base::rm(list = "df_fn2", envir = .GlobalEnv)
    base::rm(list = "df_fn3", envir = .GlobalEnv)
    base::rm(list = "df_fn4", envir = .GlobalEnv)
    base::rm(list = "df_fn5", envir = .GlobalEnv)
    base::rm(list = "df_fn6", envir = .GlobalEnv)
})
