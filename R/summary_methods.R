#' Summary of between subject design contrast analysis
#'
#' @param object output of calc_contrast
#' @param ... further arguments
#' @return Displays type of contrast analysis, lambdas, t-table, ANOVA table and
#'   typical effect sizes. If you assign this to a variable, it will be a list
#'   with the elements Lambdas, tTable, FTable, Effects.
#'
#' @export
summary.cofad_bw <- function(object, ...) {
  x <- object
  type <- ifelse(class(object) == "cofad_bw", "Contrast Analysis Between",
                 "Contrast Analysis Mixed")
  s <- x$sig
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
  t <- sqrt(s["f_contrast"])*sign(x$effects[1])
  p_label <- paste0("p(t\u2265", round(t, 3), ")\u2460")
  t_tab <- matrix(
    c(s["L"], "df" = round(s["df_inn"]),
      "t" = t,
      "p" = pt(t, s["df_inn"], lower.tail = F)),
    nrow = 1)
  t_tab[1:3] <- round(t_tab[1:3], 3)
  t_tab[4] <- signif(t_tab[4], 3)
  colnames(t_tab) <- c("L", "df", "t", p_label)
  rownames(t_tab) <- ""
  out <- list(x$lambda_between, t_tab,
              f_tab, r_tab)
  names(out) <- c("Lambdas", "tTable", "FTable", "Effects")
  warning <- ifelse(s["L"] < 0, "\n\nYour contrast estimate is negative. This means that your data does not reflect the expected direction of your hypothesis specified by the contrast weights (lambdas).", "")
  cat(paste0(type, warning, "\n\n", collapse = ""))
  print(out[1:2], na.print = "")
  cat("\u2460The p-value refers to a one-tailed test.\n\n")
  print(out[3:4], na.print = "")
  invisible(out)
}
#' Summary of within subject design contrast analysis
#'
#' @param object output of calc_contrast
#' @param ci confidence intervall for composite Score (L-Values)
#' @param ... further arguments
#' @return Displays type of contrast analysis, lambdas, t-table and typical
#'   effect sizes. If you assign this to a variable, it will be a list with the
#'   elements Lambdas, tTable, Effects.
#' @export
summary.cofad_wi <- function(object, ci = .95, ...) {
  x <- object
  l_mean <- x[[2]][[1]]
  l_se <- x[[2]][2]
  l_df <- x[[1]][3]
  l_p <- x[[1]][2]
  l_se_ci <- qt(p = (1 - ci) / 2, df = l_df, lower.tail = F) * l_se
  l_upper_bound <- l_mean + l_se_ci
  l_lower_bound <- l_mean - l_se_ci
  l_vals <- c(round(c(l_mean, l_se, l_df, x$sig[1]), 3),
              signif(l_p, 3),
              round(c(l_lower_bound, l_upper_bound), 3))
  l_vals <- matrix(l_vals, ncol = length(l_vals))
  l_eff <- round(matrix(c(x[[4]][1], x[[4]][2])), 3)
  p_label <- paste0("p(t\u2265", round(x$sig[[1]], 3), ")\u2460")
  rownames(l_eff) <- c("r-contrast", "g-contrast")
  colnames(l_eff) <- ""
  colnames(l_vals) <- c("mean of L", "SE", "df", "t", p_label,
                        paste0(ci*100, "%", c("CI-lower", "CI-upper")))
  rownames(l_vals) <- ""
  out <- list(x$lambda_within, l_vals, l_eff)
  names(out) <- c("Lambdas", "tTable", "Effects")
  warning <- ifelse(l_mean < 0, "\n\nYour contrast estimate is negative. This means that your data does not reflect the expected direction of your hypothesis specified by the contrast weights (lambdas).", "")
  cat(paste0("Contrast Analysis Within", warning, "\n\n", collapse = ""))
  print(out[1:2], na.print = "")
  cat("\u2460The p-value refers to a one-tailed test.\n\n")
  print(out[3], na.print = "")
  invisible(out)
}

#' Summary of a mixed design contrast analysis
#'
#' @param object output of calc_contrast
#' @param ... further arguments
#' @return Displays type of contrast analysis, lambdas, t-table, ANOVA table and
#'   typical effect sizes. If you assign this to a variable, it will be a list
#'   with the elements Lambdas, tTable, FTable, Effects.
#' @export
summary.cofad_mx <- function(object, ...) {
  summary.cofad_bw(object, ...)
}
