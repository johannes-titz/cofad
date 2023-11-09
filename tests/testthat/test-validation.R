#context("tests constructor validation functions")
library(testthat)
library(cofad)

test_that("constructor checks work", {
  expect_error(
    calc_contrast(dv = NULL, lambda_between = c(1)),
    "variable must be numeric"
  )

  expect_error(
    calc_contrast(dv = 1:8,
                  between = as.factor(rep(1:2, 4)),
                  lambda_between = NULL,
                  within = NULL,
                  lambda_within = NULL, id = NULL,
                  data = NULL),
    "Lambdas are missing, either set lambda_between or lambda_within or both."
  )

  expect_error(
    calc_contrast(dv = 1:8,
                  between = as.factor(rep(1:2, 4)),
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
                  lambda_within = NULL,
                  id = NULL,
                  data = NULL),
    "Independent Variable is missing, either set between or within or both."
  )

  expect_error(
    calc_contrast(dv = 1:8,
                  lambda_between =  c("1" = -0.5, "foo" = 0.5),
                  between = as.factor(rep(1:2, 4)),
                  within = NULL,
                  lambda_within = NULL,
                  id = NULL,
                  data = NULL),
    "lambda names doesn't match all between labels"
  )

  expect_warning(
    calc_contrast(dv = 1:8,
                  lambda_between =  c("1" = -0.5, "2" = 0.5),
                  between = rep(1:2, 4),
                  within = NULL,
                  lambda_within = NULL,
                  id = NULL,
                  data = NULL),
    "between is not a factor. I will try to convert it to a factor."
  )

  expect_message(
    calc_contrast(dv = 1:8,
                  between = as.factor(rep(1:2, 4)),
                  lambda_between =  c("1" = 1, "2" = 2),
                  within = as.factor(rep(1:4, 2)),
                  lambda_within = NULL,
                  id = NULL,
                  data = NULL),
    "lambdas are centered and rounded to 3 digits"
  )

  expect_warning(
    calc_contrast(dv = c(1:4, 1:4),
                  between = as.factor(rep(c("1" = 1, "2" = 2), each = 4)),
                  lambda_between =  c("1" = -1, "2" = 1),
                  within = as.factor(rep(1:4, 2)),
                  id = NULL,
                  data = NULL),
    "SD of group means is zero"
  )
})
