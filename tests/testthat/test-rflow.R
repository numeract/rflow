context("rflow")

test_that("rflow works", {
    
    w <- 3
    q <- 4
    f <- function(x, y = 2, z = w) {x + y + z + q}
    rf <- make_rflow(f)
    rflow <- environment(rf)$self
    
    expect_equal(f(1), 10)
    expect_equal(f(2), 11)
    expect_equal(rf(1)$collect(), f(1))
    expect_equal(rf(2)$collect(), f(2))
    expect_identical(rf(1), rflow)
    
    w <- 5
    expect_equal(f(1), 12)
    expect_equal(rf(1)$collect(), f(1))

    q <- 9
    expect_equal(f(1), 17)
    expect_equal(rf(1)$collect(), f(1))
    
    expect_equal(f(1), 17)
    expect_equal(f(1), 17)
    expect_equal(rflow$state_index, 4L)
    expect_equal(nrow(rflow$state), 4L)
    expect_equal(nrow(rflow$output_state), 0L)
})
