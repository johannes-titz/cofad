dat <- haven::read_sav("imagination_facilitation.sav")

# rename variables
dat$id <- dat$Nr.PP
dat$condition <- factor(dat$Condição)
levels(dat$condition) <- c("pay_attention", "imagine", "memorize")
dat$condition <- factor(dat$condition, levels = c("imagine", "memorize", "pay_attention"))

table(dat$condition, dat$Condição)
dat$prop_recalled <- dat$RC_Average

# subset of data that we use in the manuscript
maraver_2021 <- dat[, c("id", "condition", "prop_recalled")]
maraver <- maraver_2021

usethis::use_data(maraver, overwrite = T)
