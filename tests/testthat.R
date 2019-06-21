library(testthat)
library(cofad)

test_check("cofad")

expect_error(
  calc_contrast(variable = NULL, lambda_between = c(1)),
  "variable must be numeric"
)
expect_error(
  calc_contrast(variable = 1:8,
                between = as.factor(rep(1:2,4)),
                between_levels = NULL,
                lambda_between = NULL,
                within = NULL, within_levels = NULL,
                lambda_within = NULL, ID = NULL,
                data = NULL),
  "lambda is missing"
)
expect_error(
  calc_contrast(variable = 1:8,
                between = as.factor(rep(1:2,4)),
                between_levels = NULL,
                lambda_between = 1:2,
                within = NULL, within_levels = NULL,
                lambda_within = NULL, ID = NULL,
                data = NULL),
  "Missing arguments"
)
expect_error(
  calc_contrast(variable = 1:8,
                between = as.factor(rep(1:2,4)),
                between_levels = NULL,
                lambda_between = 1:2,
                within = as.factor(rep(1:4,2)),
                within_levels = NULL,
                lambda_within = NULL,
                ID = NULL,
                data = NULL),
  "Missing arguments"
)
expect_error(
  calc_contrast(variable = 1:8,
                between = as.factor(rep(1:2,4)),
                between_levels = c("1", "2"),
                lambda_between = 1:2,
                within = as.factor(rep(1:4,2)),
                within_levels = as.character(1:4),
                lambda_within = 1:4,
                ID = NULL,
                data = NULL),
  "Missing arguments"
)






expect_error(
  calc_contrast(variable = c(1:7, NA),
                between = as.factor(rep(1:2, 4)),
                between_levels = c("1", "2"),
                lambda_between = 1:2,
                within = as.factor(rep(1:4,2)),
                within_levels = NULL,
                lambda_within = NULL,
                ID = NULL,
                data = NULL),
  "Missing arguments"
)

