library(testthat)
library(cofad)
###### Test between
# Table 3.1 from Rosenthal Chapter 3
data(tab31)
t31 <- calc_contrast(
  dv = Val,
  between = Let,
  lambda_between = sample(
    c(
      "A" = -3, "B" = -1,
      "C" = 1, "D" = 3
    ), 4, F
  ),
  data = tab31
)
expect_equal(t31$sig[1], 28.9)

# furr
data(furr)
ca <- calc_contrast(
  dv = empathy, between = major,
  lambda_between = c(
    "psychology" = 1, "education" = -1,
    "business" = 0, "chemistry" = 0
  ),
  data = furr
)
expect_equal(round(ca$sig[1], 3), 6.154)

# Table 16.2 from Sedlmeier 525
data(sedlmeier525)
sedlmeier525 <- sedlmeier525[sample(1:15, 15, F), ]
t16_2 <- calc_contrast(
  dv = lsg,
  between = between,
  lambda_between = sample(
    c("KT" = -2, "JT" = 3, "MT" = -1), 3, F
  ),
  data = sedlmeier525
)
expect_equal(round(t16_2$sig[1], 3), 6.519)

###### Test within (no between)
# Example for within-subjects-design calculation 16.6 from
# Sedlmeier and Renkewitz (2018, p. 537)
data("sedlmeier537")

# random row order
sedlmeier537 <- sedlmeier537[sample(1:32, 32, F), ]

# analysis
contr_wi <- calc_contrast(
  dv = reading_test, within = music,
  lambda_within = c(
    "without music" = 1.25, "white noise" = 0.25, "classic" = -0.75,
    "jazz" = -0.75
  ),
  ID = participant, data = sedlmeier537
)

expect_equal(contr_wi$desc[1], 5.875)
expect_equal(
  round(contr_wi$sig, 3), c(5.269, .001, 7)
)

######## Test within + between (no between Lambda)
data("tab59b")

tab59b <- tab59b[sample(1:14, 14, F), ]
t_59b <- calc_contrast(
  dv = var,
  within = med,
  between = bw,
  ID = ID,
  lambda_within = c("T" = -1, "P" = +1),
  data = tab59b
)
expect_setequal(
  round(t_59b$sig, 2),
  c(7.41, 0.00, 4.00)
)

# Test mixed Design (within_lambda & between_lambda)
#
# Table 5.3. from Rosenthal, Chapter 5 (raw data)
data(rosenthal_tbl53)
rosenthal_tbl53 <- rosenthal_tbl53[sample(1:36, 36, F), ]
t_53 <- calc_contrast(
  dv = var, between = between,
  within = within,
  ID = ID,
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

# chap 5 exercise 2
data(chap5_Exercise2)

c5_e2 <- calc_contrast(
  dv = var,
  within = within,
  ID = ID,
  lambda_within = c("L" = -1, "M" = 0, "H" = 1),
  between = between,
  data = chap5_Exercise2,
  lambda_between = c("ch" = 1, "cl" = -1)
)
expect_equal(c5_e2$sig[1], 28.125)
##

data(tab59)
tab59 <- tab59[sample(1:12, 12, F), ]
t59 <- calc_contrast(
  dv = var,
  within = med,
  between = pt,
  ID = ID,
  lambda_within = c("T" = 1, "P" = -1),
  data = tab59
)
expect_equal(round(t59$sig, 3)[1], 2.449)
