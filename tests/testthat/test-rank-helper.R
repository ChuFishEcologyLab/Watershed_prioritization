test_that("rank helper works", {
  expect_equal(scale_rank(c(1, 2)), c(1,100))
  expect_equal(scale_rank(c(1, 2, 3, 4)), c(1, 34, 67, 100))
  expect_error(scale_rank(1))
})

test_that("rank helper works", {
  expect_equal(scale_min_max(c(1, 2)), c(0, 100))
  expect_equal(scale_min_max(c(1, 2, 3, 4)), c(0, 100/3, 200/3, 100))
  expect_error(scale_min_max(1))
})
