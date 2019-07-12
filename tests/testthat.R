library(testthat)
library(cofad)
expect_error(
  calc_contrast(dv = NULL, lambda_between = c(1)),
  "variable must be numeric"
)
expect_error(
  calc_contrast(dv = 1:8,
                between = as.factor(rep(1:2,4)),
                lambda_between = NULL,
                within = NULL,
                lambda_within = NULL, ID = NULL,
                data = NULL),
  "lambda is missing"
)
expect_error(
  calc_contrast(dv = 1:8,
                lambda_between = 1:2,
                within = NULL,
                lambda_within = NULL, ID = NULL,
                data = NULL),
  "lambda must be a named numeric"
)
expect_error(
  calc_contrast(dv = 1:8,
                lambda_between = c("1" = 1, "foo" = 2),
                within = NULL,
                lambda_within = NULL, ID = NULL,
                data = NULL),
  "Missing arguments"
)
expect_error(
  calc_contrast(dv = 1:8,
                lambda_between =  c("1" = 1, "foo" = 2),
                between = 1:2,
                within = NULL,
                lambda_within = NULL,
                ID = NULL,
                data = NULL),
  "between must be a factor"
)
expect_warning(
  calc_contrast(dv = 1:8,
                between = as.factor(rep(1:2,4)),
                lambda_between =  c("1" = 1, "2" = 2),
                within = as.factor(rep(1:4,2)),
                lambda_within = NULL,
                ID = NULL,
                data = NULL),
  "lambdas are centered"
)
expect_warning(
  calc_contrast(dv = c(1:7, NA),
                between = as.factor(rep(1:2,4)),
                lambda_between =  c("1" = 1, "2" = 2),
                within = as.factor(rep(1:4,2)),
                ID = NULL,
                data = NULL),
  "SD of groupmeans is zero"
)
###### Test between
# Table 3.1 from Rosenthal Chapter 3
tab3_1 <- data.frame(
  Val = c(2, 6, 8, 4, 10, 6, 8, 10, 4, 12, 8,
          16, 10, 14, 12, 12, 18, 14, 20, 16),
  Let = rep(c("A", "B", "C", "D"), c(5, 5, 5, 5)))
tab3_1 <- tab3_1[sample(1:20, 20, F ),]
t31 <- calc_contrast(dv = Val,
                    between = Let,
                    lambda_between = sample(
                      c("A" = -3, "B" = -1,
                        "C" = 1, "D" = 3), 4, F),
                    data = tab3_1)
expect_equal(t31$sig[1], 28.9)
# Sedlmeier p. 525 table 16.2
tab16_2 <- data.frame(
  lsg = c(1,2,2,2,3,4,2,3,4,3,2,3,3,1,2),
  between = as.factor(rep(c("KT", "JT", "MT"), c(5, 5, 5))),
  lambda = rep(c(-2, 3, -1 ), c(5, 5, 5))
)
tab16_2 <- tab16_2[sample(1:15, 15, F), ]
t16_2 <- calc_contrast(dv = lsg,
              between = between,
              lambda_between = sample(
                c("KT" = -2, "JT" = 3, "MT" = -1), 3, F),
              data = tab16_2
              )
expect_equal(round(t16_2$sig[1], 3), 6.519)

###### Test within (no between)
# Exampel for within-subjects-design Calculation 16.6 from
# Sedlmeier and Renkewitz (2018, p. 537)
tab16_6 <- data.frame(
  Var = c(27, 25, 30, 29, 30, 33, 31, 35,
          25, 26, 32, 29, 28, 30, 32, 34,
          21, 25, 23, 26, 27, 26, 29, 31,
          23, 24, 24, 28, 24, 26, 27, 32),
  within = as.factor(
    rep(c("om", "wr", "kl", "ja"),c(8,8,8,8))),
  ID = as.factor(rep(1:8,4))
)
tab16_6 <- tab16_6[sample(1:32, 32, F), ]

t16_6<- calc_contrast(
  dv = Var,
  within = within,
  ID = ID,
  lambda_within = sample(c("wr" = 0.25, "kl" = -.75,
                    "om" = 1.25, "ja" = -.75), 4, F),
  data=tab16_6
)
expect_equal(t16_6$desc[1], 5.875)
expect_equal(
  round(t16_6$sig,3), c(5.269,.001, 7)
)

######## Test wtihin + between (no between Lambda)
tab59b <- data.frame(
  ID=as.factor(rep(1:7, 2)),
  var = c(0,1,0,0,0,0,1,
          3,6,6,1,1,3,4),
  med = as.factor(rep(c("T", "P"), c(7,7))),
  bw = as.factor(rep(rep(c("A", "B", "C"), c(2,1,4)),2))
)
tab59b <- tab59b[sample(1:14, 14, F),]
t_59b <- calc_contrast(dv = var,
              within = med,
              between = bw,
              ID = ID,
              lambda_within = c("T" = -1, "P" = +1),
              data=tab59b)
expect_setequal(
  round(t_59b$sig, 2),
  c(7.41, 0.00, 4.00)
)

# Test mixed Design (within_lambda & between_lambda)
#
# Table 5.3. from Rosenthal, Chapter 5 (raw data)
tab53_raw <- data.frame(
  var = c(3, 1, 4, 4, 5, 5, 6, 5, 7, 2, 2, 5,
          5, 6, 7, 6, 6, 8, 3, 1, 5, 4, 5, 6,
          7, 6, 8, 3, 2, 5, 6, 6, 7, 8, 8, 9),
  between = as.factor(
    rep(
      rep(c("age8" , "age10", "age12"), c(3, 3, 3)),
      4)),
  ID = as.factor(rep(1:9, 4)),
  within = as.factor(rep(1:4, c(9, 9, 9, 9)))
)
tab53_raw <- tab53_raw[sample(1:36, 36, F), ]
t_53 <- calc_contrast(dv = var, between = between,
              within = within,
              ID = ID,
              lambda_within = sample(c("1" = -3, "2" = -1,
                                "3" = 1, "4" = 3), 4, F),
              lambda_between = sample(c("age8" = -1, "age10" = 0,
                                 "age12" = 1), 3, F),
              data= tab53_raw
              )
expect_equal( round(t_53$sig[c(1, 3, 4)],3), c(20.211, 1, 6))
expect_equal(summary(t_53)$Effects[1], 0.871)

chap5_Exercise2 <- data.frame(
  var = c(8,7,8,4,4,3,11,10,12,5,4,5,13,14,16,5,6,5),
  ID = as.factor(c(1:6,1:6,1:6)),
  within = as.factor(rep(c("L", "M", "H"), c(6, 6, 6))),
  between = as.factor(
    rep(rep(c("ch", "cl"), c(3, 3)),3)
  )
)
c5_e2 <- calc_contrast(
  dv = var,
  within = within,
  ID = ID,
  lambda_within = c("L" = -1, "M" = 0,  "H" = 1),
  between = between,
  data=chap5_Exercise2,
  lambda_between = c("ch" = 1, "cl" = -1)
)
expect_equal(c5_e2$sig[1], 28.125)
##
# Table 5.9. from Rosenthal, Chapter 5
tab59 <- data.frame(
  ID = as.factor(rep(1:6, 2)),
  var = c(12,8,4,8,6,4,8,6,4,6,2,4),
  med = as.factor(rep(c("T", "P"), c(6,6))),
  pt = as.factor( rep(rep(c("PT", "PP"), c(3,3)),2))
)
tab59 <- tab59[sample(1:12, 12, F), ]
t59 <- calc_contrast(dv = var,
              within = med,
              between = pt,
              ID=ID,
              lambda_within = c("T"=1, "P"=-1),
              data=tab59
)
expect_equal(round(t59$sig, 3)[1], 2.449)
