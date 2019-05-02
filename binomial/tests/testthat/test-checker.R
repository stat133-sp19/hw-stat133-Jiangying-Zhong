context("check function")

test_that("check_prob", {
  expect_true(check_prob(prob = 0.3))
  expect_true(check_prob(prob = 1))
  expect_error(check_prob(prob = -2))
})

test_that("check_trials", {
  expect_true(check_trials(trials = 15))
  expect_error(check_trials(trials = -20))
  expect_error(check_trials(trials = -3))
})

test_that("check_success", {
  expect_true(check_success(success = c(1,2,3),trials = 12))
  expect_error(check_success(success = -5, trials = 15))
  expect_error(check_success(success = 30, trials = 20))
})
