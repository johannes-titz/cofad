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

test_that("Rosenthal Chapter 3 is correct", {
  expect_equal(t31$sig[1], 28.9)
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
expect_equal(round(ca$sig[1], 3), 6.154)
# test effect sizes!
expect_equal(round(ca$effects, 2), c(-0.28, -0.53, -0.31))
expect_equal(calc_r_alerting(ca$effects[2], ca$effects[1]), ca$effects[3])
expect_equal(calc_r_contrast(ca$effects[3], ca$effects[1]), ca$effects[2])
expect_equal(calc_r_effectsize(ca$effects[3], ca$effects[2]), ca$effects[1])

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
expect_equal(round(t16_2$sig[1], 3), 6.519)
expect_equal(round(t16_2$effects[2], 2), 0.59)

# within (no between)------
#
# Example for within-subjects-design calculation 16.6 from
## Sedlmeier and Renkewitz (2018, p. 537) -----
data("sedlmeier_p537")

# random row order
sedlmeier_p537 <- sedlmeier_p537[sample(1:32, 32, F), ]

# analysis
contr_wi <- calc_contrast(
  dv = reading_test, within = music,
  lambda_within = c(
    "without music" = 1.25, "white noise" = 0.25, "classic" = -0.75,
    "jazz" = -0.75
  ),
  id = participant, data = sedlmeier_p537
)

expect_equal(contr_wi$desc[1], 5.875)
expect_equal(
  round(contr_wi$sig, 3), c(5.269, .001, 7)
)
expect_equal(round(contr_wi$effects[2], 2), 1.86)

# mixed ----
## (no between Lambda) -----
data("tbl59b")

tbl59b <- tbl59b[sample(1:14, 14, F), ]
t_59b <- calc_contrast(
  dv = dv,
  within = med,
  between = bw,
  id = id,
  lambda_within = c("T" = -1, "P" = +1),
  data = tbl59b
)
expect_setequal(
  round(t_59b$sig, 2),
  c(7.41, 0.00, 4.00)
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

expect_equal(round(t_53$sig[c(1, 3, 4)], 3), c(20.211, 1, 6))
expect_equal(summary(t_53)$Effects[1], 0.871)

# unclear -----
## chap 5 exercise 2----
data(chap5_Exercise2)

c5_e2 <- calc_contrast(
  dv = dv,
  within = within,
  id = id,
  lambda_within = c("L" = -1, "M" = 0, "H" = 1),
  between = between,
  data = chap5_Exercise2,
  lambda_between = c("ch" = 1, "cl" = -1)
)
expect_equal(c5_e2$sig[1], 28.125)

## this is what?----
data(rosenthal_tbl59)
tbl59 <- rosenthal_tbl59[sample(1:12, 12, F), ]
t59 <- calc_contrast(
  dv = dv,
  within = med,
  between = pt,
  id = id,
  lambda_within = c("T" = 1, "P" = -1),
  data = tbl59
)
expect_equal(round(t59$sig, 3)[1], 2.449)
