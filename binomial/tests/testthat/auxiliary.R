context("check auxiliary functions")

test_that("check aux_mean with ok vectors",{
  expect_true(aux_mean(trials=20,prob=0.6))
  expect_true(aux_mean(trials=15,prob=0.3))
})

test_that("check aux_mean with invalid vectors",{
  expect_error(aux_mean(trials=-15,prob=0.4))
  expect_error(aux_mean(trials=15,prob=1.2))
  expect_error(aux_mean(trials=-15,prob=1.6))
})

test_that("check aux_variance with ok vectors",{
  expect_true(aux_variance(trials=10,prob=0.6))
  expect_true(aux_variance(trials=15,prob=0.24))
})

test_that("check aux_variance with invalid vectors",{
  expect_error(aux_variance(trials=-15,prob=0.5))
  expect_error(aux_variance(trials=15,prob=1.6))
  expect_error(aux_variance(trials=-15,prob=1.6))
})

test_that("check aux_mode with ok vectors",{
  expect_true(aux_mode(trials=10,prob=0.5))
  expect_true(aux_mode(trials=15,prob=0.34))
})

test_that("check aux_mode with invalid vectors",{
  expect_error(aux_mode(trials=-15,prob=0.5))
  expect_error(aux_mode(trials=16,prob=1.4))
  expect_error(aux_mode(trials=-16,prob=1.6))
})

test_that("check aux_skewness with ok vectors",{
  expect_true(aux_skewness(trials=10,prob=0.9))
  expect_true(aux_skewness(trials=15,prob=0.04))
})

test_that("check aux_skewness with invalid vectors",{
  expect_error(aux_skewness(trials=-15,prob=0.5))
  expect_error(aux_skewness(trials=14,prob=1.5))
  expect_error(aux_skewness(trials=-16,prob=1.05))
})

test_that("check aux_kurtosis with ok vectors",{
  expect_true(aux_kurtosis(trials=10,prob=0.9))
  expect_true(aux_kurtosis(trials=15,prob=0.04))
})

test_that("check aux_kurtosis with invalid vectors",{
  expect_error(aux_kurtosis(trials=-15,prob=0.5))
  expect_error(aux_kurtosis(trials=16,prob=1.5))
  expect_error(aux_kurtosis(trials=-8,prob=1.05))
})
