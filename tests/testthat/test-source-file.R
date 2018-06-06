# Tests for source file --------------------------------------------------------
context("tests for source-file")

skip("faster tests")
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
    write.csv(df1, file1, row.names = FALSE)
    
    file_path <- as.character(fs::path(file1))
    test_rflow_source <- flow_file_source(file_path)
    
    expect_equal(test_rflow_source$state_index, 1L)
    test_rflow_source <- NULL
})


test_that("flow_file_source() works when file modified", {
    write.csv(df1, file1, row.names = FALSE)
    
    file_path <- as.character(fs::path(file1))
    test_rflow_source <- flow_file_source(file_path)
    
    expect_equal(test_rflow_source$state_index, 1L)
    
    write.csv(df2, file1, row.names = FALSE)
    test_rflow_source <- flow_file_source(file_path)
    
    expect_equal(test_rflow_source$state_index, 2L)
    forget(test_rflow_source)
    test_rflow_source <- NULL
})


test_that("flow_file_source() works when adding same file", {
    write.csv(df1, file1, row.names = FALSE)

    file_path <- as.character(fs::path(file1))
    test_rflow_source <- flow_file_source(file_path)

    expect_equal(test_rflow_source$state_index, 1L)

    test_rflow_source <- flow_file_source(file_path)

    expect_equal(test_rflow_source$state_index, 1L)
    test_rflow_source <- NULL
})


test_that("flow_file_source() works with fs::path type", {
    write.csv(df1, file1, row.names = FALSE)
    
    file_path <- fs::path(file1)
    expect_silent(test_rflow_source <- flow_file_source(file_path))
    
    expect_equal(test_rflow_source$state_index, 1L)
    test_rflow_source <- NULL
})


test_that("flow_file_source() works with non existent file path", {
    
    file_path <- as.character(fs::path("test", "path"))
    expect_silent(test_rflow_source <- flow_file_source(file_path))
    test_rflow_source <- NULL
})


test_that("flow_file_source() works when file present and then missing", {
    write.csv(df1, file1, row.names = FALSE)
    
    file_path <- as.character(fs::path(file1))
    test_rflow_source <- flow_file_source(file_path)
    
    # here there are already two states
    expect_equal(test_rflow_source$state_index, 1L)
    unlink(file1)
    
    test_rflow_source <- flow_file_source(file_path)
    
    # Shouldn't this be 2? No, same path has been used before, eddy is not reset
    # TODO; start with new path or new eddy
    expect_equal(test_rflow_source$state_index, 3L)
    test_rflow_source <- NULL
})

# TODO: file missing, then present

test_that("flow_file_source() stops with non valid input", {
    
    expect_error(test_rflow_source <- flow_file_source(1))
    expect_error(test_rflow_source <- flow_file_source(TRUE))
    expect_error(test_rflow_source <- flow_file_source(character()))
    expect_silent(test_rflow_source <- flow_file_source(c("path1", "path2")))
    # TODO: tests with multiple paths, they become elements
    expect_error(test_rflow_source <- flow_file_source(character()))
    expect_error(test_rflow_source <- flow_file_source(list()))
    expect_error(test_rflow_source <- flow_file_source(NULL))
    expect_error(test_rflow_source <- flow_file_source(NA))
    expect_error(test_rflow_source <- flow_file_source(NA_character_))
})


teardown({
    unlink(c(file1, file2, file3))
    base::rm(list = "file1", envir = .GlobalEnv)
    base::rm(list = "file2", envir = .GlobalEnv)
    base::rm(list = "file3", envir = .GlobalEnv)
    base::rm(list = "df1", envir = .GlobalEnv)
    base::rm(list = "df2", envir = .GlobalEnv)
})
