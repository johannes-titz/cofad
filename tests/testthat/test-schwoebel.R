dat <- readr::read_csv("Schwoebel_etal_2018.csv")

dat$condition <- factor(dat$condition,
                        labels = c("control",
                                   "same context massed",
                                   "different context massed",
                                   "same context spaced",
                                   "different context spaced"))
# compute group means --> use later to estimate the contrast estimate
(group_means <- aggregate(percent_recalled ~ condition, dat, mean))

# spaced vs. massed
contrast1 <- calc_contrast(dv = percent_recalled,
                           between = condition,
                           lambda_between = c(
                             "control" = 0,
                             "same context massed" = -1,
                             "different context massed" = -1,
                             "same context spaced" = 1,
                             "different context spaced" = 1
                           ),
                           data = dat
)
summary <- summary(contrast1)
# contrast estimate by hand
expect_equal(summary$tTable[,"L"], round(sum(group_means[c(4,5),2]) - sum(group_means[c(2,3),2]), 3))
