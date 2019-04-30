context("check main functions")

test_that("check bin_choose with ok vectors",{
  expect_true(bin_choose(trials=12,success=4))
  expect_true(bin_choose(trials=15,success=6))
})

test_that("check bin_choose with invalid vectors",{
  expect_error(bin_choose(trials=10,success=14))
  expect_error(bin_choose(trials=15,success=16))
})

test_that("check bin_probability with ok vectors",{
  expect_true(bin_probability(success=4,trials=12,prob=0.5))
  expect_true(bin_probability(success=5,trials=15,prob=0.6))
})

test_that("check bin_probability with invalid lengths or vectors",{
  expect_error(bin_probability(success=12,trials=8,prob=0.5))
  expect_error(bin_probability(success=5,trials=12,prob=1.5))
  expect_error(bin_probability(success=-5,trials=12,prob=1.5))
  expect_error(bin_probability(success=5,trials=-2,prob=1.5))
  expect_error(bin_probability(success=5,trials=12,prob=c(0.5,0.6)))
})

test_that("check bin_distribution with ok vectors",{
  expect_true(bin_distribution(trials=12,prob=0.5))
  expect_true(bin_distribution(trials=30,prob=0.4))
})

test_that("check bin_distribution with invalid vectors",{
  expect_error(bin_distribution(trials=12,prob=1.5))
  expect_error(bin_distribution(trials=12,prob=c(0.8,0.4)))
  expect_error(bin_distribution(trials=-8,prob=0.5))
})

test_that("check bin_cumulative with ok vectors",{
  expect_true(bin_cumulative(trials=16,prob=0.5))
  expect_true(bin_cumulative(trials=18,prob=0.2))
})

test_that("check bin_cumulative with invalid vectors",{
  expect_error(bin_cumulative(trials=16,prob=1.5))
  expect_error(bin_cumulative(trials=12,prob=c(0.8,0.4)))
  expect_error(bin_cumulative(trials=-6,prob=0.2))
})
