library(testthat)
library(cofad)

test_check("cofad")

tab3_1 <- data.frame(
  c(2, 6, 8, 4,
    10, 6, 8, 10,
    4, 12, 8, 16,
    10, 14, 12, 12,
    18, 14,20,16),
  rep(c("A", "B", "C", "D"), c(5, 5, 5, 5)))
colnames(tab3_1) <- c("Val", "Let")
between <- tab3_1$Let
variable <- tab3_1$Val
t1 <- calc_contrast(variable = Val,
                    between = Let,
                    between_levels = c("A", "B", "C", "D"),
                    lambda_between = c(-3, -1, 1, 3),
                    data = tab3_1)
summary(t1)
t1


# Tab53
variable <- c(3,1,4,4,5,5,6,5,7,2,2,5,5,6,7,6,6,8,3,1,5,4,5,6,7,6,8,3,2,5,6,6,7,8,8,9)
between <- as.factor(rep(c(1,1,1,2,2,2,3,3,3),4))
between_levels <- as.character(1:3)
ID <- as.factor(rep(1:9,4 ))
within <- as.factor(rep(1:4, c(9,9,9,9)))
within_levels <- c("1","2","3","4")
lambda_within <- c(-3,-1,1,3)
lambda_between <-c(-1,0,1)

t3 <- calc_contrast(variable = variable, between = between,
                    between_levels = between_levels,
                    within = within, within_levels = within_levels,
                    ID = ID,
                    lambda_within = lambda_within)
t3
summary(t3)

