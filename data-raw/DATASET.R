# between from furr
furr_p4 <- data.frame(
  empathy = c(51, 56, 61, 58, 54, 62, 67, 57, 65, 59, 50, 49, 47, 45, 44, 50,
              45, 40, 49, 41
  ),
  major = as.factor(rep(c("psychology", "education", "business", "chemistry"),
                        each = 5)
  )
)

# Sedlmeier p. 525 table 16.2------
sedlmeier_p525 <- data.frame(
  lsg = c(1, 2, 2, 2, 3, 4, 2, 3, 4, 3, 2, 3, 3, 1, 2),
  between = as.factor(rep(c("KT", "JT", "MT"), c(5, 5, 5))),
  lambda = rep(c(-2, 3, -1), c(5, 5, 5))
)

# Example for within-subjects design Calculation 16.6 from
# Sedlmeier and Renkewitz (2018, p. 537)

sedlmeier_p537 <- d <- data.frame(
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


# Example for between-subjects design Table 3.1 from
# Rosenthal, Rosnow and Rubin (2001)

rosenthal_tbl31 <- data.frame(
  dv = c(2, 6, 8, 4, 10, 6, 8, 10, 4, 12, 8, 16, 10, 14, 12, 12, 18, 14, 20,
          16),
  between = as.factor(rep(c("A", "B", "C", "D"), c(5, 5, 5, 5)))
)

# Example for mixed-designs Table 5.3 from
# Rosenthal, Rosnow and Rubin (2001)
rosenthal_tbl53 <- data.frame(
  dv = c(3, 1, 4, 4, 5, 5, 6, 5, 7, 2, 2, 5, 5, 6, 7, 6, 6, 8, 3, 1, 5, 4, 5,
          6, 7, 6, 8, 3, 2, 5, 6, 6, 7, 8, 8, 9),
  between = as.factor(rep(rep(c("age8", "age10", "age12"), c(3, 3, 3)), 4)),
  id = as.factor(rep(1:9, 4)),
  within = as.factor(rep(1:4, c(9, 9, 9, 9)))
)

# Table 5.9. from Rosenthal, Chapter 5
rosenthal_tbl59 <- data.frame(
  id = as.factor(rep(1:6, 2)),
  dv = c(12, 8, 4, 8, 6, 4, 8, 6, 4, 6, 2, 4),
  med = as.factor(rep(c("treatment", "placebo"), c(6, 6))),
  pt = as.factor(rep(rep(c("psychotherapy", "placebo"), c(3, 3)), 2))
)

# Rosenthal example for unequal sample sizes for between condition (mixed ca),
# p. 141
rosenthal_p141 <- data.frame(
  id = as.factor(rep(1:7, 2)),
  dv = c(0, 1, 0, 0, 0, 0, 1, 3, 6, 6, 1, 1, 3, 4),
  med = as.factor(rep(c("treatment", "placebo"), c(7, 7))),
  bw = as.factor(rep(rep(c("A", "B", "C"), c(2, 1, 4)), 2))
)

# rosenthal Chapter 5 Review Question 2
rosenthal_chap5_q2 <- data.frame(
  dv = c(8, 7, 8, 4, 4, 3, 11, 10, 12, 5, 4, 5, 13, 14, 16, 5, 6, 5),
  id = as.factor(c(1:6, 1:6, 1:6)),
  within = as.factor(rep(c("low", "medium", "high"), c(6, 6, 6))),
  between = as.factor(rep(rep(c("high", "low"), c(3, 3)), 3))
)

# rosenthal comparison of 2 contrasts between

# rosenthal comparison of 2 contrasts within
rosenthal_tbl68 <- data.frame(
  dv = c(2, 4, 4, 8, 1, 5, 6, 7, 3, 5, 4, 8, 2, 6, 6, 9,
         1, 5, 5, 6, 3, 5, 3, 7, 2, 4, 3, 7, 2, 6, 5, 8),
  within = as.factor(paste0("t", rep(1:4, 8))),
  id = as.factor(rep(1:8, each = 4)),
  between = as.factor(c("girl", "boy"))
)

write.csv(rosenthal_tbl31, "data-raw/rosenthal_tbl31.csv", row.names = F)
write.csv(sedlmeier_p537, "data-raw/sedlmeier_p537.csv", row.names = F)
write.csv(rosenthal_tbl59, "data-raw/rosenthal_tbl59.csv", row.names = F)
write.csv(rosenthal_p141, "data-raw/rosenthal_p141.csv", row.names = F)
write.csv(furr_p4, "data-raw/furr_p4.csv", row.names = F)
write.csv(sedlmeier_p525, "data-raw/sedlmeier_p525.csv", row.names = F)
write.csv(rosenthal_tbl53, "data-raw/rosenthal_tbl53.csv", row.names = F)
write.csv(rosenthal_chap5_q2, "data-raw/rosenthal_chap5_q2.csv", row.names = F)
write.csv(rosenthal_tbl68, "data-raw/rosenthal_tbl68.csv", row.names = F)

usethis::use_data(rosenthal_tbl31, overwrite = T)
usethis::use_data(sedlmeier_p537, overwrite = T)
usethis::use_data(rosenthal_tbl59, overwrite = T)
usethis::use_data(rosenthal_p141, overwrite = T)
usethis::use_data(furr_p4, overwrite = T)
usethis::use_data(sedlmeier_p525, overwrite = T)
usethis::use_data(rosenthal_tbl53, overwrite = T)
usethis::use_data(rosenthal_chap5_q2, overwrite = T)
usethis::use_data(rosenthal_tbl68, overwrite = T)

# new data, Mirka and Simone
#working_memory <- read.csv("data-raw/Akan_etal_2018_Exp2B.csv")
testing_effect <- read.csv("data-raw/dat_testing.csv")
usethis::use_data(testing_effect, overwrite = T)

# Haans 2018

d <- haven::read_stata("data-raw/within1by4data.dta")
haans_within1by4 <- tidyr::pivot_longer(d, 2:5)
usethis::use_data(haans_within1by4, overwrite = T)

# l <- c(3, 1, -1, -3)
# names(l) <- unique(haans_within1by4$name)
# res <- calc_contrast(value, id = person, within = name,
#                      lambda_within = l, data = haans_within1by4)
# summary(res)

# Schwöbel

# exclude the control condition, and relabel the conditions
dat_schwoebel <- readr::read_csv("data-raw/Schwoebel_etal_2018.csv")
dat_schwoebel <- dat_schwoebel[dat_schwoebel$condition!=1,]
dat_schwoebel$condition <- factor(dat_schwoebel$condition,
                                  labels = c("massed-same", "massed-different", "spaced-same", "spaced-different"))
schwoebel <- dat_schwoebel
usethis::use_data(schwoebel, overwrite = T)
