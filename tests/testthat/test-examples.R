library(testthat)
library(cofad)
# between -----------

## Table 3.1 from Rosenthal Chapter 3 -------
data(rosenthal_tbl31)
t31 <- calc_contrast(
  dv = dv,
  between = between,
  lambda_between = sample(
    c(
      "A" = -3, "B" = -1,
      "C" = 1, "D" = 3
    ), 4, F
  ),
  data = rosenthal_tbl31
)

test_that("Rosenthal Chapter 3 F-value is correct", {
  expect_equal(t31$sig["f_contrast"], c("f_contrast" = 28.9))
})

## furr -----
data("furr_p4")
ca <- calc_contrast(
  dv = empathy, between = major,
  lambda_between = c(
    "psychology" = 1, "education" = -1,
    "business" = 0, "chemistry" = 0
  ),
  data = furr_p4
)
test_that("furr p4 works", {
          expect_equal(round(ca$sig["f_contrast"], 3), c("f_contrast" = 6.154))
          # test effect sizes!
          expect_equal(as.numeric(round(ca$effects, 2)),
                       c(-0.28, -0.53, -0.31))
          }
)
# test convert functions
test_that(
  "converting effect sizes works for furr", {
    expect_equal(calc_r_alerting(ca$effects[2], ca$effects[1]), ca$effects[3])
    expect_equal(calc_r_contrast(ca$effects[3], ca$effects[1]), ca$effects[2])
    expect_equal(calc_r_effectsize(ca$effects[3], ca$effects[2]), ca$effects[1])
  }
)

## Table 16.2 from Sedlmeier 525----
data("sedlmeier_p525")
sedlmeier525 <- sedlmeier_p525[sample(1:15, 15, F), ]
t16_2 <- calc_contrast(
  dv = lsg,
  between = between,
  lambda_between = sample(
    c("KT" = -2, "JT" = 3, "MT" = -1), 3, F
  ),
  data = sedlmeier_p525
)

test_that("sedlmeier 525 works", {
  expect_equal(round(t16_2$sig["f_contrast"], 3),
               c("f_contrast" = 6.519))
  expect_equal(as.numeric(round(t16_2$effects[2], 2)), 0.59)
}
)
# within (no between)------
#
# Example for within-subjects-design calculation 16.6 from
## Sedlmeier and Renkewitz (2018, p. 537) -----
data("sedlmeier_p537")

# random row order
sedlmeier_p537 <- sedlmeier_p537[sample(1:32, 32, F), ]
# change order of levels
sedlmeier_p537$music <- relevel(sedlmeier_p537$music, "white noise")

# analysis
contr_wi <- calc_contrast(
  dv = reading_test, within = music,
  lambda_within = sample(c(
    "without music" = 1.25, "white noise" = 0.25, "classic" = -0.75,
    "jazz" = -0.75), 4, F
  ),
  id = participant, data = sedlmeier_p537
)

test_that("sedlmeier 537 works", {
  expect_equal(contr_wi$desc[1], 5.875)
  expect_equal(
    round(contr_wi$sig, 3), c(5.269, .001, 7)
  )
  expect_equal(round(contr_wi$effects[2], 2), 1.86)
  }
)
# mixed ----
## unequal sample sizes for between -----
data("rosenthal_p141")

rosenthal_p141 <- rosenthal_p141[sample(1:14, 14, F), ]
ca <- calc_contrast(
  dv = dv,
  within = med,
  between = bw,
  id = id,
  lambda_within = c("treatment" = -1, "placebo" = +1),
  data = rosenthal_p141
)
test_that("rosenthal works", {
  expect_setequal(round(ca$sig, 2), c(7.41, 0.00, 4.00))
  }
)
## (within_lambda & between_lambda) -----

# Table 5.3. from Rosenthal, Chapter 5 (raw data)
data(rosenthal_tbl53)
rosenthal_tbl53 <- rosenthal_tbl53[sample(1:36, 36, F), ]
t_53 <- calc_contrast(
  dv = dv, between = between,
  within = within,
  id = id,
  lambda_within = sample(c(
    "1" = -3, "2" = -1,
    "3" = 1, "4" = 3
  ), 4, F),
  lambda_between = sample(c(
    "age8" = -1, "age10" = 0,
    "age12" = 1
  ), 3, F),
  data = rosenthal_tbl53
)

test_that("rosenthal 53 works", {
  expect_equal(round(t_53$sig[c(1, 3, 4)], 3), c(20.211, 1, 6))
  expect_equal(summary(t_53)$Effects[1], 0.871)
  }
)
# Rosenthal chap 5 exercise 2
data(rosenthal_chap5_q2)

c5_e2 <- calc_contrast(
  dv = dv,
  within = within,
  id = id,
  lambda_within = c("low" = -1, "medium" = 0, "high" = 1),
  between = between,
  data = rosenthal_chap5_q2,
  lambda_between = c("high" = 1, "low" = -1)
)

test_that("rosenthal q2 works", {
  expect_equal(c5_e2$sig[1], c(28.125))
  }
)

# Rosenthal table 5.9
data(rosenthal_tbl59)
tbl59 <- rosenthal_tbl59[sample(1:12, 12, F), ]
t59 <- calc_contrast(
  dv = dv,
  within = med,
  between = pt,
  id = id,
  lambda_within = c("treatment" = 1, "placebo" = -1),
  data = tbl59
)
test_that("rosenthal 59 works", {
  expect_equal(round(t59$sig, 3)[1], 2.449)
  }
)

# comparison between two contrasts, sedlmeier 2013 16.8, data is the same as
# sedlmeier_p525

lambda1 <- c(-2, 3, -1)
lambda2 <- c(-2, 1, 1)
lambda_diff <- lambda_diff(lambda1, lambda2, labels = c("KT", "JT", "MT"))

data("sedlmeier_p525")
sedlmeier525 <- sedlmeier_p525[sample(1:15, 15, F), ]
t16_2B <- calc_contrast(
  dv = lsg,
  between = between,
  lambda_between = round(lambda_diff, 2),
  data = sedlmeier_p525
)

test_that("comparison between two contrasts works, sedlmeier 525", {
  # actual value in Sedlmeier (2013) p. 533 is 1.137, which seems to be due to
  # rounding errors (lambda_diff is identical)
  expect_equal(round(sqrt(t16_2B$sig["f_contrast"]), 3), c("f_contrast" = 1.136))
  expect_equal(as.numeric(round(t16_2B$effects[1], 2)), 0.26)
  }
)

# comparison between two contrasts within, Sedlmeier 2013 p. 534
data("sedlmeier_p537")

# random row order
sedlmeier_p537 <- sedlmeier_p537[sample(1:32, 32, F), ]

lambda1 <- c(1.25, 0.25, -0.75, -0.75)
lambda2 <- c(3, -1, -1, -1)
lambda_diff <- lambda_diff(lambda2, lambda1,
                           labels = c("without music", "white noise", "classic",
                                      "jazz"))
# analysis
contr_wi <- calc_contrast(
  dv = reading_test, within = music,
  lambda_within = round(lambda_diff, 2),
  id = participant, data = sedlmeier_p537
)

test_that("comparison between 2 contrasts for within (sedlmeier 537) works", {
  # actual value in Sedlmeier 2013 p. 536 is -3.75
  expect_equal(round(contr_wi$sig[1], 2), c(-3.77))
  expect_equal(round(contr_wi$effects[2], 2), -1.33)
  }
)


# comparison between two contrasts between, Rosenthal table 6.4, 6.5, 6.6
lambda2 <- c(-3, -1, 1, 3)
lambda1 <- c(-1, -1, -1, 3)
names(lambda1) <- letters[1:4]
names(lambda2) <- letters[1:4]
lambda_diff <- lambda_diff(lambda2, lambda1)
# rosenthal is rounding upto 1.03, whereas the correct rounding would be 1.02
lambda_rosenthal <- c("a" = -.76, "b" = .13, "c" = 1.02, "d" = -.39)
expect_equal(round(lambda_diff, 2), lambda_rosenthal)

# <- calc_contrast(dv = dv, between = between, data = ...,
#                  lambda_between = lambda_diff)
# expect_equal(round($sig, 3)[1], ...)

# comparison between two contrasts within, it seems this table is not correct
# in rosenthal, the values for L1 in Rosenthal are all -0.01 of the correct ones...
# checked it manually with some examples
data("rosenthal_tbl68")
lambda2 <- round(cofad:::zscale(c(-3, -1, 1, 3)), 10)
lambda1 <- round(cofad:::zscale(c(-1, 0, 0, 1)), 10)
names(lambda1) <- c("t1", "t2", "t3", "t4")
names(lambda2) <- names(lambda1)

lambda_diff <- round(lambda_diff(lambda1, lambda2), 2)
# test_that("diff of two contrasts works",
#           expect_setequal(lambda_diff, c())
# ca_tbl68 <- calc_contrast(dv = dv, id = id, within = within,
#                           lambda_within = lambda_diff, data = rosenthal_tbl68)
# ca_tbl68
#
# librarian::shelf(dplyr)
# r <- rosenthal_tbl68
# r$lambda <- lambda_diff
# r$lambda1 <- lambda1
# r$lambda2 <- lambda2
# r %>%
#   group_by(id) %>%
#   summarize(sum(dv * lambda), sum(dv * lambda1), sum(dv * lambda2))

# comparison between two contrasts mixed...rosenthal example, probably incorrect

# lambda1 <- c(-1, 0, 1)
# lambda2 <- c(-2, 1, 1)
# names(lambda1) <- names(lambda2) <- letters[1:3]
# lambda_diff <- lambda_diff(lambda1, lambda2)

# helper lambda_diff

test_that("error handling for lambda_diff works", {
  expect_error(lambda_diff(1:3, 3:1), "Please provide group labels for your lambdas")
  expect_error(lambda_diff(c("A" = 1, "B" = 2, "C" = 3), 3:1), "Please provide group labels for your lambdas")
  expect_error(lambda_diff(3:1, c("A" = 1, "B" = 2, "C" = 3)), "Please provide group labels for your lambdas")
  expect_error(lambda_diff(c("A" = 3, "B" = 2, "C" = 3), c("A" = 1, "B" = 2, "C" = 3),
                           labels = letters[1:3]), "Use either a named")
  expect_error(lambda_diff(3:1, c("A" = 1, "B" = 2, "C" = 3),
                           labels = letters[1:3]), "Use either a named")
  expect_error(lambda_diff(c("A" = 3, "B" = 2, "C" = 3), 1:3,
                           labels = letters[1:3]), "Use either a named")
  expect_error(lambda_diff(1:3, 1:3), "Your lambdas are perfectly correlated")
  expect_error(lambda_diff(c("a" = 1, "b" = 2), c("A" = 1, "b" = 2)),
               "Please provide the same labels for your")
  }
)


test_that("2 ways of lambda specification for lambda_diff are identical", {
  expect_equal(lambda_diff(3:1, 1:3, labels = letters[1:3]),
               lambda_diff(c("a" = 3, "b" = 2, "c" = 1),
                           c("a" = 1, "b" = 2, "c" = 3)))
  }
)
