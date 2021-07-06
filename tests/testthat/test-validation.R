context("tests constructor validation functions")
library(testthat)
library(cofad)

test_that("constructor checks work", {
  expect_error(
    calc_contrast(dv = NULL, lambda_between = c(1)),
    "variable must be numeric"
  )

  expect_error(
    calc_contrast(dv = 1:8,
                  between = as.factor(rep(1:2,4)),
                  lambda_between = NULL,
                  within = NULL,
                  lambda_within = NULL, id = NULL,
                  data = NULL),
    "lambda is missing"
  )

  expect_error(
    calc_contrast(dv = 1:8,
                  lambda_between = 1:2,
                  within = NULL,
                  lambda_within = NULL, id = NULL,
                  data = NULL),
    "lambda must be a named numeric"
  )

  expect_error(
    calc_contrast(dv = 1:8,
                  lambda_between = c("1" = 1, "foo" = 2),
                  within = NULL,
                  lambda_within = NULL, id = NULL,
                  data = NULL),
    "Missing arguments"
  )

  expect_error(
    calc_contrast(dv = 1:8,
                  lambda_between =  c("1" = 1, "foo" = 2),
                  between = 1:2,
                  within = NULL,
                  lambda_within = NULL,
                  id = NULL,
                  data = NULL),
    "between must be a factor"
  )

  expect_warning(
    calc_contrast(dv = 1:8,
                  between = as.factor(rep(1:2,4)),
                  lambda_between =  c("1" = 1, "2" = 2),
                  within = as.factor(rep(1:4,2)),
                  lambda_within = NULL,
                  id = NULL,
                  data = NULL),
    "lambdas are centered and rounded to 3 digits"
  )

  expect_warning(
    calc_contrast(dv = c(1:7, NA),
                  between = as.factor(rep(1:2,4)),
                  lambda_between =  c("1" = -1, "2" = 1),
                  within = as.factor(rep(1:4,2)),
                  id = NULL,
                  data = NULL),
    "SD of group means is zero"
  )
})
