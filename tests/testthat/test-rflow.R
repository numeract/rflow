context("rflow")


test_that("memoisation works", {
    
    # no arguments, just a side effect to determine when f ran
    i <- 0
    f <- function() { i <<- i + 1; i }
    rf <- make_rflow(f)
    
    expect_equal(f(), 1)
    expect_equal(f(), 2)
    # +1 due to f running one more time
    expect_equal(collect(rf()), 3)
    # +0 due to remembering previous value
    expect_equal(collect(rf()), 3)
    
})


test_that("rflow works", {
    
    w <- 3
    q <- 4
    f <- function(x, y = 2, z = w) {x + y + z + q}
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
})


test_that("rflow chaining works", {
    
    x0 <- 10
    x1 <- 0.5
    x2 <- 2
    
    f <- function(a, b, c = 1) {a * b + c}
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
})
