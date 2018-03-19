context("rflow")


test_that("make_rflow works", {
    
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
    
})


test_that("make_rflow works with function with one argument", {
    
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
    
})


test_that("make_rflow works with function with one default argument", {
    f <- function(j = 1) { i <<- i + 1; i }
    i <- 0
    
    expect_warning(rf <- make_rflow(f), NA)
    expect_equal(f(), 1)
    expect_equal(f(), 2)
    # +1 due to f running one more time
    expect_equal(collect(rf()), 3)
    # +0 due to remembering previous value
    expect_equal(collect(rf()), 3)
})


test_that("make_rflow works with function with one default function argument", {
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
})


# test_that("symbol collision", {
#     f <- function(j = 1) { i <<- i + 1; i }
#     i <- 0
#     rf <- make_rflow(f)
#     
#     expect_equal(f(), 1)
#     expect_equal(f(), 2)
#     expect_equal(collect(rf()), 3)
#     expect_equal(collect(rf()), 3)
#     expect_equal(f(), 4)
#     expect_equal(collect(rf()), 3)
#     
#     expect_true(rf$eddy$forget_rflow(rf$fn_key))
#     expect_equal(collect(rf()), 5)
# })

# test_that("is.memoised", {
#     i <- 0
#     expect_false(is.memoised(i))
#     expect_false(is.memoised(is.memoised))
#     expect_true(is.memoised(memoise(identical)))
# })

test_that("make_rflow works with anonymous function", {
    expect_warning(rf <- make_rflow(function(a = 1) a), NA)
    expect_equal(names(formals(rf))[[1]], "a")

    expect_equal(collect(rf(1)), 1)
    expect_equal(collect(rf(2)), 2)
    expect_equal(collect(rf(1)), 1)
})

test_that("make_rflow works with primitive function", {
    expect_warning(rf <- make_rflow(`+`), NA)
    expect_equal(names(formals(rf)), names(formals(args(`+`))))

    expect_equal(collect(rf(1, 2)), 1 + 2)
    expect_equal(collect(rf(2, 3)), 2 + 3)
    expect_equal(collect(rf(1, 2)), 1 + 2)
})

test_that("make_rflow works with missing arguments", {
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
})

# test_that("printing a memoised function prints the original definition", {
#     
#     browser()
#     f <- function(j) { i <<- i + 1; i }
#     
#     rf <- make_rflow(f)
#     
#     f_output <- capture.output(f)
#     rf_output <- capture.output(rf)
#     
#     expect_equal(rf_output[1], "Memoised Function:")
#     
#     expect_equal(rf_output[-1], fn_output)
# })

# test_that("make_rflow work with already cached function", {
#     browser()
#     rf_sum <- make_rflow(sum)
#     expect_error(make_rflow(rf_sum), "`rf_sum` must not be memoised.")
# })


test_that("make_rflow evaluates arguments in proper environment", {
    e <- new.env(parent = baseenv())
    e$a <- 5
    f <- function(x, y = a) { x + y }
    environment(f) <- e
    
    rf <- make_rflow(f)
    expect_equal(f(1), collect(rf(1)))
    expect_equal(f(10), collect(rf(10)))
})


test_that("make_rflow function's arguments are evaluated before hashing", {
    i <- 1
    
    rf <- make_rflow(function(x, y, z = 3) { x + y + z })
    rf2 <- function(x, y) rf(x, y)
    
    expect_equal(collect(rf2(1, 1)), 5)
    
    expect_equal(collect(rf2(1, 1)), 5)
    
    expect_equal(collect(rf2(2, 2)), 7)
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


context("Caching")
# test_that("has_data works", {
#     
#     rf <- make_rflow(sum)
#     rflow <- environment(rf)$self
#     
#     browser()
#     
#     eddy <- rflow$eddy
#     
#     eddy$has_data(rflow$fn_key)
#     
#     expect_equal(f(1), 17)
#     
# })

test_that("rflow caching works", {
    
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


test_that("interface of wrapper matches interface of memoised function", {
    fn <- function(j) { i <<- i + 1; i }
    i <- 0
    
    expect_equal(formals(fn), formals(make_rflow(fn)))
    expect_equal(formals(runif), formals(make_rflow(runif)))
    expect_equal(formals(paste), formals(make_rflow(paste)))
})
