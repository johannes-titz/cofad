dat <- readr::read_csv("Schwoebel_etal_2018.csv")

library(DescTools)
library(cofad)

# anova -------------------------------------------------------------------

mod <- aov(percent_recalled ~ factor(condition), data = dat)

# contrasts ---------------------------------------------------------------

# Die 5 Bedingungen lauten: control, same context massed, different context massed,
# same context spaced, different context spaced.

# Analysiert wurde sie mit einer One-way-ANOVA und Post-hoc-Tests (immer Vergleich zur KG)
# sowieso zusätzlich eine 2x2-ANOVA, die die KG ignoriert hat.
# Es wurden keine Kontraste getestet.
# control = same context massed < different context massed = same context spaced <
# different context spaced.

# Jetzt könnte man verschiedene Hypothesen testen:
#   (1) Ist spaced testing besser als massed testing?
#   (2) Ist different context besser als same context?
#   (3) Ist der Effekt von different context additiv oder nicht (Interaktion)?

dat$condition <- factor(dat$condition,
                        labels = c("control",
                                   "same context massed",
                                   "different context massed",
                                   "same context spaced",
                                   "different context spaced"))
# compute group means --> use later to estimate the contrast estimate
(group_means <- aggregate(percent_recalled ~ condition, dat, mean))
# --> increasing means by condition
# spaced > massed
# different > same
# interaction?

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
