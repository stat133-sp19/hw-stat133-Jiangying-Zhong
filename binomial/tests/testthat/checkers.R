context("check checker functions")

test_that("check check_prob with ok vectors",{

  expect_true(check_prob(trials=10,prob=0.3))
  expect_true(check_prob(trials=5,prob=0.6))
})

test_that("check check_prob with invalid lengths or vectors",{
  expect_error(check_prob(trials=10,prob=c(0.5,06)))
  expect_error(check_prob(trials=10,prob=1.2))
  expect_error(check_prob(trials=10,prob=c(0.3,1.5,0.8)))
})

test_that("check check_trials with ok vectors",{
  expect_true(check_trials(trials=10))
  expect_true(check_trials(trials=8))
})

test_that("check check_trials with invalid vectors",{
  expect_error(check_trials(trials=-2))
  expect_error(check_trials(trials=-5))
})

test_that("check check_success with ok vectors",{
  expect_true(check_success(success=2,trials=10))
  expect_true(check_success(success=4,trials=15))
})

test_that("check check_success with invalid vectors",{
  expect_error(check_success(success=-2,trials=10))
  expect_error(check_success(success=12,trials=10))
})

