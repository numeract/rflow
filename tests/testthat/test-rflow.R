context("make_rflow() special cases")


test_that("make_rflow() works", {

    # no arguments, just a side effect to determine when f ran
    i <- 0
    f <- function() { i <<- i + 1; i }
    rf <- make_rflow(f)

    expect_warning(rf, NA)
    expect_equal(f(), 1)
    expect_equal(f(), 2)
    # +1 due to f running one more time
    expect_equal(collect(rf()), 3)
    # +0 due to remembering previous value
    expect_equal(collect(rf()), 3)

    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("make_rflow() works with function with one argument", {

    # no arguments, just a side effect to determine when f ran
    i <- 0
    f <- function(j) { i <<- i + 1; i }
    rf <- make_rflow(f)

    expect_warning(rf, NA)
    expect_equal(f(), 1)
    expect_equal(f(), 2)
    # +1 due to f running one more time
    expect_equal(collect(rf()), 3)
    # +0 due to remembering previous value
    expect_equal(collect(rf()), 3)

    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("make_rflow() works with function with variadic arguments", {

    # no arguments, just a side effect to determine when f ran
    i <- 0
    f <- function(...) { i <<- i + 1; i }

    expect_warning(rf <- make_rflow(f), NA)
    expect_equal(f(), 1)
    expect_equal(f(), 2)
    # +1 due to f running one more time
    expect_equal(collect(rf()), 3)
    # +0 due to remembering previous value
    expect_equal(collect(rf()), 3)

    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("make_rflow() works with function with one default argument", {

    f <- function(j = 1) { i <<- i + 1; i }
    i <- 0

    expect_warning(rf <- make_rflow(f), NA)
    expect_equal(f(), 1)
    expect_equal(f(), 2)
    # +1 due to f running one more time
    expect_equal(collect(rf()), 3)
    # +0 due to remembering previous value
    expect_equal(collect(rf()), 3)

    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that(
    "make_rflow() works with function with one default function argument", {

    expect_false(exists("g"))
    g <- function() 1
    f <- function(j = g()) { i <<- i + 1; i }
    i <- 0

    expect_warning(rf <- make_rflow(f), NA)
    expect_equal(f(), 1)
    expect_equal(f(), 2)
    # +1 due to f running one more time
    expect_equal(collect(rf()), 3)
    # +0 due to remembering previous value
    expect_equal(collect(rf()), 3)

    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("make_rflow() works with anonymous function", {

    expect_warning(rf <- make_rflow(function(a = 1) a), NA)
    expect_equal(names(formals(rf))[[1]], "a")

    expect_equal(collect(rf(1)), 1)
    expect_equal(collect(rf(2)), 2)
    expect_equal(collect(rf(1)), 1)

    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("make_rflow() works with primitive function", {

    expect_warning(rf <- make_rflow(`+`), NA)
    expect_equal(names(formals(rf)), names(formals(args(`+`))))

    expect_equal(collect(rf(1, 2)), 1 + 2)
    expect_equal(collect(rf(2, 3)), 2 + 3)
    expect_equal(collect(rf(1, 2)), 1 + 2)

    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("make_rflow() works with missing arguments", {

    f <- function(x, y) {
        i <<- i + 1
        if (missing(y)) {
            y <- 1
        }
        x + y
    }
    rf <- make_rflow(f)
    i <- 0

    expect_equal(f(1), collect(rf(1)))
    expect_equal(f(1, 2), collect(rf(1, 2)))
    expect_equal(i, 4)
    rf(1)
    expect_equal(i, 4) # i doesn't increment, which is ok

    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("make_rflow() keeps function visibility", {

    vis <- function() NULL
    invis <- function() invisible()

    rf_vis <- make_rflow(vis)
    rf_invis <- make_rflow(invis)

    expect_true(withVisible(collect(rf_vis()))$visible)
    expect_false(withVisible(collect(rf_invis()))$visible)

    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("make_rflow() evaluates arguments in proper environment", {

    e <- new.env(parent = baseenv())
    e$a <- 5
    f <- function(x, y = a) { x + y }
    environment(f) <- e

    rf <- make_rflow(f)
    expect_equal(f(1), collect(rf(1)))
    expect_equal(f(10), collect(rf(10)))

    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("make_rflow() function's arguments are evaluated before hashing", {

    i <- 1

    rf <- make_rflow(function(x, y, z = 3) { x + y + z })
    rf2 <- function(x, y) rf(x, y)

    expect_equal(collect(rf2(1, 1)), 5)

    expect_equal(collect(rf2(1, 1)), 5)

    expect_equal(collect(rf2(2, 2)), 7)

    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("interface of wrapper matches interface of cached function", {

    fn <- function(j) { i <<- i + 1; i }
    i <- 0

    expect_equal(formals(fn), formals(make_rflow(fn)))
    expect_equal(formals(runif), formals(make_rflow(runif)))
    expect_equal(formals(paste), formals(make_rflow(paste)))

    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("interface of wrapper matches interface of cached function", {

    fn <- function(j) { i <<- i + 1; i }
    i <- 0

    expect_equal(formals(fn), formals(make_rflow(fn)))
    expect_equal(formals(runif), formals(make_rflow(runif)))
    expect_equal(formals(paste), formals(make_rflow(paste)))
    
    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("make_rflow() states work", {

    w <- 3
    q <- 4
    f <- function(x, y = 2, z = w) { x + y + z + q }
    rf <- make_rflow(f)
    rflow <- environment(rf)$self

    expect_false(rflow$is_valid)
    
    expect_equal(f(1), 10)
    expect_equal(f(2), 11)
    expect_equal(collect(rf(1)), f(1))
    expect_equal(collect(rf(2)), f(2))
    expect_identical(rf(1), rflow)

    expect_true(rflow$is_valid)
    
    # detects a change in the default values
    w <- 5
    expect_equal(f(1), 12)
    expect_equal(collect(rf(1)), f(1))

    # does not detect a change in one the vars in the env (f not pure)
    q <- 9
    expect_equal(f(1), 17)
    expect_equal(collect(rf(1)), 12)

    expect_equal(collect(rf(1)), 12)
    expect_equal(collect(rf(1)), 12)
    expect_equal(rflow$state_index, 3L)
    expect_equal(nrow(rflow$state), 3L)
    expect_equal(nrow(rflow$output_state), 0L)

    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


context("Caching")


test_that("rflow caching works", {

    x0 <- 10
    x1 <- 0.5
    x2 <- 2

    f <- function(a, b, c = 1) { a * b + c }
    rf <- make_rflow(f)
    rflow <- environment(rf)$self

    f1 <- f(x0, x1)
    r1 <- rf(x0, x1)
    expect_equal(f1, 6)
    expect_equal(collect(r1), 6)

    f2 <- f(f1, x2)
    r2 <- rf(r1, x2)
    expect_equal(f2, 13)
    expect_equal(collect(r2), 13)

    expect_equal(rflow$state_index, 2L)
    expect_equal(nrow(rflow$state), 2L)

    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


context("R6Flow functions")


test_that("new() handles missing fn_key case", {
    
    rf <- R6Flow$new(fn = diff)
    expect_true(!is.null(rf$fn_key))
})


test_that("new() checks if self was already stored in memory or disk", {
    
    rf <- make_rflow(diff)
    rflow <- environment(rf)$self
    
    expect_error(R6Flow$new(fn = diff, fn_key = rflow$fn_key))
})


test_that("new() checks if self was already stored in memory or disk", {

    rf <- make_rflow(diff)
    rflow <- environment(rf)$self

    expect_error(R6Flow$new(fn = diff, fn_key = rflow$fn_key))
    
    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("get_element() checks for state", {

    f <- function(a, b, c = 1) { a * b + c }

    rf <- make_rflow(f)
    rflow <- environment(rf)$self

    rf(1, 2)
    
    result <- rflow$get_element()

    expect_equal(result$elem_hash, rflow$state$out_hash)

    tmp_state <- rflow$state

    rflow$state <- data.frame()
    rflow$state_index <- NA_integer_

    result <- rflow$get_element()
    expect_equal(result$is_valid, FALSE)

    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("collect() works", {

    f <- function(b, c = 1) { b * c }

    rf <- make_rflow(f)
    rflow <- environment(rf)$self

    rf(1, 2)

    result <- rflow$collect_data()

    expect_equal(result, 2)

    rflow$state <- data.frame()
    rflow$state_index <- NA_integer_

    result <- rflow$collect_data()
    expect_equal(result$vis_out_lst$value, NULL)

    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("rflow works with split_output_fn parameter", {
    
    f <- function(b, c = 2) { list(b = b, c = c, bc = b * c) }
    # since the output is already a list, extract/calc items of interest
    so_f <- function(l) list(bc = l$bc, cc = l$c ^ 2)
    
    rf <- make_rflow(f, split_output_fn = so_f)
    rflow <- environment(rf)$self
    
    rf(2, 3)
    result <- rflow$collect_data()
    result_bc <- rflow$collect_data(name = "bc")
    result_cc <- rflow$collect_data(name = "cc")
    expect_error(rflow$collect_data(name = "dd"))

    expect_equal(result, list(b = 2, c = 3, bc = 6))
    expect_equal(result_bc, 6)
    expect_equal(result_cc, 9)
    
    elem <- rflow$get_element()
    expect_equal(elem$is_valid, TRUE)
    expect_equal(elem$elem_name, NULL)
    expect_equal(elem$elem_hash, rflow$state$out_hash)
    expect_identical(elem$self, rflow)
    expect_equal(rflow$output_state$out_hash, rep(rflow$state$out_hash, 2))
    
    expect_error(rflow$get_element(name = "dd"))
    elem_bc <- rflow$get_element(name = "bc")
    expect_equal(elem_bc$is_valid, TRUE)
    expect_equal(elem_bc$elem_name, "bc")
    tmp_hash <- rflow$output_state$elem_hash[
        rflow$output_state$elem_name == "bc"]
    expect_equal(elem_bc$elem_hash, tmp_hash)
    expect_identical(elem$self, rflow)
    
    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("split_output_fn is valid", {
    
    f <- function(b, c = 2) { list(b = b, c = c, bc = b * c) }
    # since the output is already a list, extract/calc items of interest
    
    so_f <- function(l) NA
    
    rf <- make_rflow(f, split_output_fn = so_f)
    
    expect_error(rf(3, 5))
    
    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
    
    so_f <- function(l) list(l$bc, l$c ^ 2)
    
    rf <- make_rflow(f, split_output_fn = so_f)
    
    expect_error(rf(2, 3))
    
    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
    
    so_f <- function(l) list(l$bc, b = l$c ^ 2, l$bc)
    
    rf <- make_rflow(f, split_output_fn = so_f)
    
    expect_error(rf(0, 1))
    
    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
    
    so_f <- function() { matrix(l$bc, l$c ^ 2) }
    
    rf <- make_rflow(f, split_output_fn = so_f)
    
    expect_error(rf(2, 9))
    
    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})


test_that("check_state() works", {
    
    rf <- make_rflow(sum)
    rflow <- environment(rf)$self
    
    expect_equal(rflow$check_state(), TRUE) # no state yet
    expect_equal(rflow$check_state(5), TRUE) # invalid index, pre having a state
    
    rf(1, 2)
    
    expect_equal(rflow$check_state(), TRUE) # we have out_hash in eddy
    expect_equal(rflow$check_state(5), TRUE) # invalid index after state
    
    delete_eddy(eddy_name = .EDDY_DEFAULT_NAME)
})
