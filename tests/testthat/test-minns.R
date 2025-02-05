pa1 <- matrix(c(1, 1, 0, 0, 0, 1, 1, 1, 0), nrow = 3)
pa2 <- pa1 * 2
res1 <- compute_minns_Q_I(pa1)
res2 <- compute_minns_Q_I(pa2)
# generate Nan because of a division by 0
res3 <- rbind(pa2, c(0, 0, 0)) |>
    compute_minns_Q_I()
test_that("minns computation works", {
    expect_equal(res1$Ii, rep(0.5, 3))
    expect_equal(res1$Qi, 1 / 3 * c(1, 1, 2))
    expect_equal(scale_rank(c(1, 2, 3, 4)), c(1, 34, 67, 100))
    expect_identical(res1, res2)
    expect_true(is.nan(res3$Qi[4]))
})
