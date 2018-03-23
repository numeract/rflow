context("make_rflow() special cases")


test_that("make_rflow() works", {
    
    # no arguments, just a side effect to determine when f ran
    i <- 0
    f <- function() { i <<- i + 1; i }
    rf <- make_rflow(f)
    
    expect_warning(rf <- make_rflow(f), NA)
    expect_equal(f(), 1)
    expect_equal(f(), 2)
    # +1 due to f running one more time
    expect_equal(collect(rf()), 3)
    # +0 due to remembering previous value
    expect_equal(collect(rf()), 3)
    
    delete_eddy()
})


test_that("make_rflow() works with function with one argument", {
    
    # no arguments, just a side effect to determine when f ran
    i <- 0
    f <- function(j) { i <<- i + 1; i }
    rf <- make_rflow(f)
    
    expect_warning(rf <- make_rflow(f), NA)
    expect_equal(f(), 1)
    expect_equal(f(), 2)
    # +1 due to f running one more time
    expect_equal(collect(rf()), 3)
    # +0 due to remembering previous value
    expect_equal(collect(rf()), 3)
    
    delete_eddy()
})

test_that("make_rflow() works with function with variadic arguments", {

    # no arguments, just a side effect to determine when f ran
    i <- 0
    f <- function(...) { i <<- i + 1; i }
    rf <- make_rflow(f)
    
    expect_warning(rf <- make_rflow(f), NA)
    expect_equal(f(), 1)
    expect_equal(f(), 2)
    # +1 due to f running one more time
    expect_equal(collect(rf()), 3)
    # +0 due to remembering previous value
    expect_equal(collect(rf()), 3)
    
    delete_eddy()
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
    
    delete_eddy()
})


test_that("make_rflow() works with function with one default function argument", {
    
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
    
    delete_eddy()
})


test_that("make_rflow() works with anonymous function", {
    
    expect_warning(rf <- make_rflow(function(a = 1) a), NA)
    expect_equal(names(formals(rf))[[1]], "a")

    expect_equal(collect(rf(1)), 1)
    expect_equal(collect(rf(2)), 2)
    expect_equal(collect(rf(1)), 1)
    
    delete_eddy()
})


test_that("make_rflow() works with primitive function", {
    
    expect_warning(rf <- make_rflow(`+`), NA)
    expect_equal(names(formals(rf)), names(formals(args(`+`))))

    expect_equal(collect(rf(1, 2)), 1 + 2)
    expect_equal(collect(rf(2, 3)), 2 + 3)
    expect_equal(collect(rf(1, 2)), 1 + 2)
    
    delete_eddy()
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
    
    delete_eddy()
})


test_that("make_rflow() keeps function visibility", {

    vis <- function() NULL
    invis <- function() invisible()

    rf_vis <- make_rflow(vis)
    rf_invis <- make_rflow(invis)

    expect_true(withVisible(collect(rf_vis()))$visible)
    expect_false(withVisible(collect(rf_invis()))$visible)
    
    delete_eddy()
})


test_that("make_rflow() evaluates arguments in proper environment", {
    
    e <- new.env(parent = baseenv())
    e$a <- 5
    f <- function(x, y = a) { x + y }
    environment(f) <- e
    
    rf <- make_rflow(f)
    expect_equal(f(1), collect(rf(1)))
    expect_equal(f(10), collect(rf(10)))
    
    delete_eddy()
})


test_that("make_rflow() function's arguments are evaluated before hashing", {
    
    i <- 1
    
    rf <- make_rflow(function(x, y, z = 3) { x + y + z })
    rf2 <- function(x, y) rf(x, y)
    
    expect_equal(collect(rf2(1, 1)), 5)
    
    expect_equal(collect(rf2(1, 1)), 5)
    
    expect_equal(collect(rf2(2, 2)), 7)
    
    delete_eddy()
})


test_that("interface of wrapper matches interface of cached function", {

    fn <- function(j) { i <<- i + 1; i }
    i <- 0

    expect_equal(formals(fn), formals(make_rflow(fn)))
    expect_equal(formals(runif), formals(make_rflow(runif)))
    expect_equal(formals(paste), formals(make_rflow(paste)))
    
    delete_eddy()
})


test_that("make_rflow() states work", {
    
    w <- 3
    q <- 4
    f <- function(x, y = 2, z = w) { x + y + z + q }
    rf <- make_rflow(f)
    rflow <- environment(rf)$self
    
    expect_equal(f(1), 10)
    expect_equal(f(2), 11)
    expect_equal(collect(rf(1)), f(1))
    expect_equal(collect(rf(2)), f(2))
    expect_identical(rf(1), rflow)
    
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
    
    delete_eddy()
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
    
    delete_eddy()
})


context("R6Flow functions")

test_that("new() checks if self was already stored in memory or disk", {

    rf <- make_rflow(diff)
    rflow <- environment(rf)$self

    expect_error(R6Flow$new(fn = diff, fn_key = rflow$fn_key))

    # TODO: Remove from memory and expect "already found on" disk error
    
    delete_eddy()
})


test_that("getElement() checks for state", {

    f <- function(a, b, c = 1) { a * b + c }

    rf <- make_rflow(f)
    rflow <- environment(rf)$self

    rf(1, 2)
    
    # TODO: test for split output
    # result <- rflow$getElement(name = "foo")
    #
    # expect_equal(is_valid, true)
    # expect_equal(result$element_hash, rflow$output_state$elem_hash[found_state_idx])

    result <- rflow$getElement()

    expect_equal(result$elem_hash, rflow$state$out_hash)

    tmp_state <- rflow$state

    rflow$state = data.frame()
    rflow$state_index = NA_integer_

    result <- rflow$getElement()
    expect_equal(result$is_valid, FALSE)
    
    delete_eddy()
})


test_that("collect() works", {

    f <- function(b, c = 1) { b * c }

    rf <- make_rflow(f)
    rflow <- environment(rf)$self

    rf(1, 2)
    
    # TODO: test for split output
    # result <- rflow$getElement(name = "foo")
    #
    # expect_equal(is_valid, true)
    # expect_equal(result$element_hash, rflow$output_state$elem_hash[found_state_idx])

    result <- rflow$collect()
    expect_equal(result, 2)

    rflow$state = data.frame()
    rflow$state_index = NA_integer_

    result <- rflow$collect()
    expect_equal(result$vis_out_lst$value, NULL)
    
    delete_eddy()
})
