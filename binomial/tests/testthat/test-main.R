context("check binomial")

test_that("bin_choose", {
  expect_equal(bin_choose(15,5), 3003)
  expect_error(bin_choose(3,5))
  expect_equal(bin_choose(5, 1:5), c(5,10,10,5,1))
})

test_that("bin_probability", {
  expect_equal(bin_probability(2,5,0.5), 0.3125)
  expect_error(bin_probability(10,5,0.5))
  expect_equal(bin_probability(0:2, 5, 0.3), c(0.16807,0.36015,0.30870))
})

test_that("bin_distribution", {
  expect_is(bin_distribution(5,0.5), "bindis")
  expect_length(bin_distribution(5,0.5), 2)
})

test_that("bin_cumulative", {
  expect_is(bin_cumulative(5,0.5), "bincum")
  expect_length(bin_cumulative(5,0.5), 3)
})
