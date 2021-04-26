# Example for between-subjects design Table 3.1 from
# Rosenthal, Rosnow and Rubin (2001)

tab31 <- data.frame(
  Val = c(2, 6, 8, 4,10, 6, 8, 10, 4, 12, 8,
    16, 10, 14, 12, 12,  18, 14, 20, 16),
  Let = as.factor(rep(c("A", "B", "C", "D"), c(5, 5, 5, 5)))
  )
contr_bw <- calc_contrast(
   dv = Val,
   between = Let,
   lambda_between = c("A" = -3, "B" = -1, "C" = 1, "D" = 3),
   data = tab31)
contr_bw
summary(contr_bw)

# Example for within-subjects design Calculation 16.6 from
# Sedlmeier and Renkewitz (2018, p. 537)

sedlmeier537 <- d <- data.frame(reading_test = c(27, 25, 30, 29, 30, 33, 31, 35,
                                                 25, 26, 32, 29, 28, 30, 32, 34,
                                                 21, 25, 23, 26, 27, 26, 29, 31,
                                                 23, 24, 24, 28, 24, 26, 27, 32),
                                participant = as.factor(rep(1:8, 4)),
                                music = as.factor(rep(c("without music",
                                                        "white noise",
                                                        "classic", "jazz"),
                                                      each = 8)))
contr_wi <- calc_contrast(
   dv = Var,
   within = within,
   ID = ID,
   lambda_within = c("1" = 0.25, "2" = -.75, "3" = 1.25, "4" = -.75),
   data=sedlmeier537
 )
contr_wi
summary(contr_wi, ci=.90)

# Exampel for mixed-designs Table 5.3 from
# Rosenthal, Rosnow and Rubin (2001)
tab53 <- data.frame(
   Var = c(3, 1, 4, 4, 5, 5, 6, 5, 7, 2, 2, 5,
           5, 6, 7, 6, 6, 8, 3, 1, 5, 4, 5, 6,
           7, 6, 8, 3, 2, 5, 6, 6, 7, 8, 8, 9),
           bw = as.factor(rep(rep(LETTERS[1:3], c(3, 3, 3)), 4)),
           wi = as.factor(rep(1:4, c(9, 9, 9, 9))),
           ID = as.factor(rep(1:9, 4 ))
   )
   lambda_within <- c("1" = -3, "2" = -1, "3" = 1, "4" = 3)
   lambda_between <-c("A" = -1, "B" = 0, "C" = 1)

contr_mx <- calc_contrast(dv = Var, between = bw,
              lambda_between = lambda_between,
              within = wi,
               lambda_within = lambda_within,
              ID = ID, data = tab53
              )
contr_mx
summary(contr_mx)

furr <- data.frame(empathy = c(51, 56, 61, 58, 54, 62, 67, 57, 65, 59, 50, 49, 47, 45,
                            44, 50, 45, 40, 49, 41),
                major = as.factor(
                  rep(c("psychology", "education", "business",
                              "chemistry"), each = 5)))
ca <- calc_contrast(dv = empathy, between = major,
                    lambda_between = c("psychology" = 1, "education" = -1,
                                       "business" = 0, "chemistry" = 0),
                    data = furr)
ca


write.csv(tab31, "data-raw/tab31.csv", row.names = F)
write.csv(sedlmeier537, "data-raw/sedlmeier537.csv", row.names = F)
write.csv(tab53, "data-raw/tab53.csv", row.names = F)
write.csv(furr, "data-raw/furr.csv", row.names = F)

usethis::use_data(tab31, overwrite=T)
usethis::use_data(sedlmeier537, overwrite=T)
usethis::use_data(tab53, overwrite=T)
usethis::use_data(furr, overwrite=T)
