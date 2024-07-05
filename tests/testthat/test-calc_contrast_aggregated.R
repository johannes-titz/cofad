library(dplyr)
data(furr_p4)
furr_agg <- furr_p4 %>%
  group_by(major) %>%
  summarize(mean = mean(empathy), sd = sd(empathy), n = n())
lambdas = c("psychology" = 1, "education" = -1, "business" = 0, "chemistry" = 0)
res <- calc_contrast_aggregated(mean, sd, n, major, lambdas, furr_agg)

ca <- calc_contrast(
  dv = empathy, between = major,
  lambda_between = c(
    "psychology" = 1, "education" = -1,
    "business" = 0, "chemistry" = 0
  ),
  data = furr_p4
)

test_that("calc contrast for aggregated data works", {
  expect_equal(res, ca)
})
