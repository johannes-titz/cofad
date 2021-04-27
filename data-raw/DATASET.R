# Example for between-subjects design Table 3.1 from
# Rosenthal, Rosnow and Rubin (2001)

tab31 <- data.frame(
  Val = c(2, 6, 8, 4, 10, 6, 8, 10, 4, 12, 8, 16, 10, 14, 12, 12, 18, 14, 20,
          16),
  Let = as.factor(rep(c("A", "B", "C", "D"), c(5, 5, 5, 5)))
)

# between from furr
furr <- data.frame(
  empathy = c(51, 56, 61, 58, 54, 62, 67, 57, 65, 59, 50, 49, 47, 45, 44, 50,
              45, 40, 49, 41
  ),
  major = as.factor(rep(c("psychology", "education", "business", "chemistry"),
                        each = 5)
  )
)

# Sedlmeier p. 525 table 16.2
sedlmeier525 <- data.frame(
  lsg = c(1, 2, 2, 2, 3, 4, 2, 3, 4, 3, 2, 3, 3, 1, 2),
  between = as.factor(rep(c("KT", "JT", "MT"), c(5, 5, 5))),
  lambda = rep(c(-2, 3, -1), c(5, 5, 5))
)

# Example for within-subjects design Calculation 16.6 from
# Sedlmeier and Renkewitz (2018, p. 537)

sedlmeier537 <- d <- data.frame(
  reading_test = c(
     27, 25, 30, 29, 30, 33, 31, 35,25, 26, 32, 29, 28, 30, 32, 34, 21, 25, 23,
     26, 27, 26, 29, 31, 23, 24, 24, 28, 24, 26, 27, 32
  ),
  participant = as.factor(rep(1:8, 4)),
  music = as.factor(rep(c(
    "without music",
    "white noise",
    "classic", "jazz"
  ),
  each = 8
  ))
)

# Exampel for mixed-designs Table 5.3 from
# Rosenthal, Rosnow and Rubin (2001)
rosenthal_tbl53 <- data.frame(
  var = c(3, 1, 4, 4, 5, 5, 6, 5, 7, 2, 2, 5, 5, 6, 7, 6, 6, 8, 3, 1, 5, 4, 5,
          6, 7, 6, 8, 3, 2, 5, 6, 6, 7, 8, 8, 9),
  between = as.factor(rep(rep(c("age8", "age10", "age12"), c(3, 3, 3)), 4)),
  ID = as.factor(rep(1:9, 4)),
  within = as.factor(rep(1:4, c(9, 9, 9, 9)))
)

# Table 5.9. from Rosenthal, Chapter 5
tab59 <- data.frame(
  ID = as.factor(rep(1:6, 2)),
  var = c(12, 8, 4, 8, 6, 4, 8, 6, 4, 6, 2, 4),
  med = as.factor(rep(c("T", "P"), c(6, 6))),
  pt = as.factor(rep(rep(c("PT", "PP"), c(3, 3)), 2))
)

# where is this from?
chap5_Exercise2 <- data.frame(
  var = c(8, 7, 8, 4, 4, 3, 11, 10, 12, 5, 4, 5, 13, 14, 16, 5, 6, 5),
  ID = as.factor(c(1:6, 1:6, 1:6)),
  within = as.factor(rep(c("L", "M", "H"), c(6, 6, 6))),
  between = as.factor(rep(rep(c("ch", "cl"), c(3, 3)), 3))
)

# where is this from?
tab59b <- data.frame(
  ID = as.factor(rep(1:7, 2)),
  var = c(0, 1, 0, 0, 0, 0, 1,3, 6, 6, 1, 1, 3, 4),
  med = as.factor(rep(c("T", "P"), c(7, 7))),
  bw = as.factor(rep(rep(c("A", "B", "C"), c(2, 1, 4)), 2))
)

write.csv(tab31, "data-raw/tab31.csv", row.names = F)
write.csv(sedlmeier537, "data-raw/sedlmeier537.csv", row.names = F)
write.csv(tab53, "data-raw/tab53.csv", row.names = F)
write.csv(tab59, "data-raw/tab59.csv", row.names = F)
write.csv(tab59b, "data-raw/tab59b.csv", row.names = F)
write.csv(furr, "data-raw/furr.csv", row.names = F)
write.csv(sedlmeier525, "data-raw/sedlmeier525.csv", row.names = F)
write.csv(rosenthal_tbl53, "data-raw/rosenthal_tbl53.csv", row.names = F)
write.csv(chap5_Exercise2, "data-raw/chap5_Exercise2.csv", row.names = F)

usethis::use_data(tab31, overwrite = T)
usethis::use_data(sedlmeier537, overwrite = T)
usethis::use_data(tab53, overwrite = T)
usethis::use_data(tab59, overwrite = T)
usethis::use_data(tab59b, overwrite = T)
usethis::use_data(furr, overwrite = T)
usethis::use_data(sedlmeier525, overwrite = T)
usethis::use_data(rosenthal_tbl53, overwrite = T)
usethis::use_data(chap5_Exercise2, overwrite = T)
