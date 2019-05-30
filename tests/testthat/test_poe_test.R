context("Test that the poe_test() works - An example for learning")

test_that("poe_test() returns a single numeric value", {
    x <- runif(10)
    y <- runif(10)
    expect_true(is.numeric(poe_test(x, y)))
})