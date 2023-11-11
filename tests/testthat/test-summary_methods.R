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

ftable <- matrix(c(289, 160, 455, 1, 16, 19, 289, 10, NA, 28.9, NA, NA,
                   6.18e-5, NA, NA))

test_that("Rosenthal Chapter 3 F-value is correct", {
  expect_equal(t31$sig["f_contrast"], c("f_contrast" = 28.9))
})

data(furr_p4)
ca <- calc_contrast(
  dv = empathy, between = major,
  lambda_between = c(
    "psychology" = 1, "education" = -1,
    "business" = 0, "chemistry" = 0
  ),
  data = furr_p4
)
# the p value is so large because it is the wrong direction!
furr_ftable <- matrix(c(90, 234, 1179, 1, 16, 19, 90, 14.625, NA, 2.48^2, NA, NA, pt(2.48, 19, lower.tail = F)*2, NA, NA), nrow = 3)
rownames(furr_ftable) <- c("contrast", "within", "total")
colnames(furr_ftable) <- c("SS", "df", "MS", "F", "p")

# has to be rounded to two digits because F and p value are not accurate in
# Furr
furr_ftable <- round(furr_ftable, 2)
cofad_ftable <- round(summary(ca)$FTable, 2)

test_that("furr p4 summary works", {
          expect_equal(cofad_ftable, furr_ftable)
  }
)
