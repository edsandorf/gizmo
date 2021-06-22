context("Test last")

test_that("Last correctly returns the last element", {
  expect_equal(
    last(c("my", "name", "is", "buttons")),
    "buttons"
  )
  
  expect_equal(
    last(LETTERS[seq_len(10)]),
    "J"
  )
  
  expect_equal(
    last(list(1:5, 6:10)),
    list(6:10)
  )
})
