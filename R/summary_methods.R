#' Summary of between subject design contrast analysis
#' @param object output of calc_contrast
#' @param ... further arguments
#' @return Displays ANOVA table of the contrastanalysis
#' and the typical effectsizes.
#' @export
summary.cofad_bw <- function(object, ...) {
  x <- object
  s <- x$sig
  # @Markus: please put names before calculations!
  # round everything to 3 digits, except p value, which shows 3 sig digits
  f_tab <- matrix(c(
    # first row, kontrast
    round(s[c("ss_kontrast", "df_contrast", "ss_kontrast", "f_contrast")], 3),
      signif(s["p_contrast"], 3),
    # second row within
    round(c(s[c("ss_within", "df_inn", "ms_within")], NA, NA,
    s[c("ss_total", "df_total")], NA, NA, NA), 3)),
    byrow = T, ncol = 5)
  rownames(f_tab) <- c("contrast", "within", "total")
  colnames(f_tab) <- c("SS", "df", "MS", "F", "p")
  r_tab <- as.matrix(round(x$effects, 3))
  colnames(r_tab) <- c("effects")
  out <- list(f_tab, r_tab)
  names(out) <- c("FTable", "Effects")
  warning <- ifelse(s["L"] < 0, "\n\nAttention! Your contrast is negative, meaning that it fits in the opposite direction of your lambdas!", "")
  cat(paste0("Contrast Analysis Between\n\nL=", s["L"], warning, "\n\n", collapse = ""))
  return(print(out, na.print = ""))
}
#' Summary of within subject design contrast analysis
#' @param object output of calc_contrast
#' @param ci confidence intervall for composite Score (L-Values)
#' @return Displays ANOVA table of the contrastanalysis
#' and the typical effectsizes.
#' @export
summary.cofad_wi <- function(object, ci = .95, ...) {
  x <- object
  # @Markus: please put names before calculation!
  l_mean <- x[[2]][[1]]
  l_se <- x[[2]][2]
  l_df <- x[[1]][3]
  l_p <- x[[1]][2]
  l_se_ci <- qt(p = (1 - ci) / 2, df = l_df, lower.tail = F) * l_se
  l_upper_bound <- l_mean + l_se_ci
  l_lower_bound <- l_mean - l_se_ci
  l_vals <- c(l_mean, l_se, l_df, x$sig[1], l_p, l_lower_bound, l_upper_bound)
  l_vals <- signif(matrix(l_vals, ncol = length(l_vals)), 4)
  l_eff <- signif(matrix(c(x[[4]][1], x[[4]][2])), 3)
  rownames(l_eff) <- c("r-contrast", "g-contrast")
  colnames(l_vals) <- c("mean of L", "SE", "df", "t", "p",
                     paste0(ci*100, "%", c("CI-lower", "CI-upper")))
  out <- list(l_vals, l_eff)
  names(out) <- c("L-Statistics", "Effects")
  return(out)
}
#' Summary of a mixed design contrast analysis
#' @param object output of calc_contrast
#' @param ... further arguments
#' @return Displays ANOVA table of the contrastanalysis
#' and the typical effectsizes.
#' @export
summary.cofad_mx <- function(object, ...) {
  x <- object
  # @Markus: please put names before calculation!
  all_l <- as.vector(x[[6]])
  all_l <- all_l[which(!is.na(all_l))]
  ss_total <- sum(
    (all_l - mean(all_l)) ** 2)
  df_total <- length(all_l) - 1
  f_tab <- matrix(c(
    ss_contrast <- x[[1]][5],
    df_contrast <- 1,
    ms_contrast <- x[[1]][5],
    f_contrast <- x[[1]][1],
    p_contrast <- x[[1]][2],
    ss_within  <- x[[1]][6] * x[[1]][4],
    df_within <- x[[1]][4],
    ms_within <-  x[[1]][6], NA, NA,
    ss_total, df_total, NA, NA, NA),
    ncol = 5, byrow = T)
  f_tab <- round(f_tab, 3)
  l_tab <- x[[2]]
  r_tab <- round(matrix(c(x[[5]]), ncol = 1), 3)
  colnames(r_tab) <- "effect"
  rownames(r_tab) <- c("r_effectsize", "r_contrast", "r_alerting")
  rownames(f_tab) <- c("contrast", "within", "total")
  colnames(f_tab) <- c("SS", "df", "MS", "F", "p")
  out <- list(f_tab, r_tab, l_tab)
  names(out) <- c("F_Table", "Effects", "Within_Groups")
  return(out)
}
