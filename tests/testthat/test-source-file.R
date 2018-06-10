# Tests for source file --------------------------------------------------------
context("tests for source-file")

if (digest::digest(Sys.info()[-c(2, 3)]) %in% c(
    "2e85e2a3018ecf3b2e5fc03bfb20fd39"
)) {
    skip("cache-memory-file functions")
}

setup({
    file1 <- tempfile(pattern = "test-rflow-")
    file2 <- tempfile(pattern = "test-rflow-")
    file3 <- tempfile(pattern = "test-rflow-")
    
    df1 <- tibble::remove_rownames(head(mtcars))
    df2 <- tibble::remove_rownames(tail(mtcars))

    assign("file1", file1, envir = .GlobalEnv)
    assign("file2", file2, envir = .GlobalEnv)
    assign("file3", file3, envir = .GlobalEnv)
    assign("df1", df1, envir = .GlobalEnv)
    assign("df2", df2, envir = .GlobalEnv)
})


test_that("flow_file_source() works", {
    get_current_eddy()$reset()
    
    write.csv(df1, file1, row.names = FALSE)
    
    file_path <- as.character(fs::path(file1))
    test_rflow_source <- flow_file_source(file_path)
    
    expect_equal(test_rflow_source$state_index, 1L)
})


test_that("flow_file_source() works when file modified", {
    get_current_eddy()$reset()
    
    write.csv(df1, file1, row.names = FALSE)
    
    file_path <- as.character(fs::path(file1))
    test_rflow_source <- flow_file_source(file_path)
    
    expect_equal(test_rflow_source$state_index, 1L)
    
    write.csv(df2, file1, row.names = FALSE)
    test_rflow_source <- flow_file_source(file_path)
    
    expect_equal(test_rflow_source$state_index, 2L)
})


test_that("flow_file_source() works when adding same file", {
    get_current_eddy()$reset()
    
    write.csv(df1, file1, row.names = FALSE)

    file_path <- as.character(fs::path(file1))
    test_rflow_source <- flow_file_source(file_path)

    expect_equal(test_rflow_source$state_index, 1L)

    test_rflow_source <- flow_file_source(file_path)

    expect_equal(test_rflow_source$state_index, 1L)
})


test_that("flow_file_source() works with fs::path type", {
    get_current_eddy()$reset()
    
    write.csv(df1, file1, row.names = FALSE)
    
    file_path <- fs::path(file1)
    expect_silent(test_rflow_source <- flow_file_source(file_path))
    
    expect_equal(test_rflow_source$state_index, 1L)
})


test_that("flow_file_source() works with non existent file path", {
    get_current_eddy()$reset()
    
    file_path <- as.character(fs::path("test", "path"))
    expect_silent(test_rflow_source <- flow_file_source(file_path))
})


test_that("flow_file_source() works when file present, missing, and changed", {
    get_current_eddy()$reset()
    
    write.csv(df1, file1, row.names = FALSE)
    
    file_path <- as.character(fs::path(file1))
    test_rflow_source <- flow_file_source(file_path)
    
    # here there are already two states
    expect_equal(test_rflow_source$state_index, 1L)
    unlink(file1)
    
    test_rflow_source <- flow_file_source(file_path)
    expect_equal(test_rflow_source$state_index, 2L)
    
    write.csv(df2, file1, row.names = FALSE)
    test_rflow_source <- flow_file_source(file_path)
    
    expect_equal(test_rflow_source$state_index, 3L)
    expect_equal(NROW(test_rflow_source$state), 3)
})


test_that("flow_file_source() stops with non valid input", {
    get_current_eddy()$reset()
    
    expect_error(test_rflow_source <- flow_file_source(1))
    expect_error(test_rflow_source <- flow_file_source(TRUE))
    expect_error(test_rflow_source <- flow_file_source(character()))
    expect_error(test_rflow_source <- flow_file_source(character()))
    expect_error(test_rflow_source <- flow_file_source(list()))
    expect_error(test_rflow_source <- flow_file_source(NULL))
    expect_error(test_rflow_source <- flow_file_source(NA))
    expect_error(test_rflow_source <- flow_file_source(NA_character_))
   
})

test_that("flow_file_source() works with 2 paths", {
    get_current_eddy()$reset()
    
    expect_silent(test_rflow_source <- flow_file_source(c("path1", "path2")))
    expect_true(test_rflow_source$get_element("path1")$is_current)
    expect_true(test_rflow_source$get_element("path2")$is_current)
})

teardown({
    # get_current_eddy()$terminate()
    
    unlink(c(file1, file2, file3))
    base::rm(list = "file1", envir = .GlobalEnv)
    base::rm(list = "file2", envir = .GlobalEnv)
    base::rm(list = "file3", envir = .GlobalEnv)
    base::rm(list = "df1", envir = .GlobalEnv)
    base::rm(list = "df2", envir = .GlobalEnv)
})
