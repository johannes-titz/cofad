#' Output of between-subject design contrast analysis
#'
#' @param x output of calc_contrast
#' @param ... further arguments
#' @return Displays the significance of the contrast analysis. The contrast
#' weights, the corresponding group and an effectsize are given.
#' @export
print.cofad_bw <- function(x, ...) {
  p_value <- format.pval(x[[1]][2], 4)
  p_value <- paste("; p = ", p_value, sep = "")
  p <- paste("F(1,", x[[1]][4], ") = ", signif(x[[1]][1], 4), p_value, sep = "")
  lambda <- round(x[[3]], 3)
  contr_1 <-  paste(
    paste(names(lambda), "=", lambda, collapse = "; "),
    collapse = NULL
  )
  r_effect <- round(x[[4]][1], 3)
  opposite <- ifelse(
    r_effect < 0,
    "Since the effect is negative, the contrast actually fits in the opposite direction.",
    ""
  )
  cat("\nWe ran a contrast analysis for the following between contrasts: ",
      contr_1, ". ")
  cat("This resulted in statistics of ", p,
      " and an effect magnitude of r_effectsize = ", r_effect, ". ", opposite)
}
#' Output of a within subject design contrast analysis
#'
#' @param x output of calc_contrast
#' @param ... further arguments
#' @return Displays the significance of the contrast analysis. The
#' contrastweights, the corresponding group and an effectsize are given.
#' @export
print.cofad_wi <- function(x, ...) {
  l_val <- paste(
    "L-Values: Mean = ", signif(x[[2]][1], 4), "; SD = ", signif(x[[2]][3], 4)
  )
  p_value <- format.pval(x[[1]][2], 4)
  p_value <- paste("; p = ", p_value, sep = "")
  p <- paste("t(", x[[1]][3], ") = ", signif(x[[1]][1], 4), p_value, sep = "")
  lambda <- signif(x[[3]], 4)
  contr_1 <-  paste(
    paste(names(lambda), "=", lambda, collapse = "; "),
    collapse = NULL
  )
  g_effect <- signif(x[[4]][2], 4)
  opposite <- ifelse(
    g_effect < 0,
    "Since the effect is negative, the contrast actually fits in the opposite direction.",
    ""
  )
  cat("\nWe ran a contrast analysis for the following within contrasts: ",
      contr_1, ". ")
  cat("This resulted in statistics of ", p,
      " and an effect magnitude of g_effectsize = ", g_effect, ". ", opposite)
}
#' Output of a mixed design contrast analysis
#'
#' @param x output of calc_contrast
#' @param ... further arguments
#' @return Displays the significance of the contrast analysis. The
#' contrastweights, the corresponding group and an effectsize are given.
#' @export
print.cofad_mx <- function(x, ...) {
  lambda <- signif(x[[3]], 4)
  contr_1 <-  paste(
    paste(names(lambda), "=", lambda, collapse = "; "),
    collapse = NULL
  )
  p_value <- signif(x[[1]][2], 4)
  p_value <- paste("; p = ", p_value, sep = "")
  p <- paste("F(1,", x[[1]][4], ") = ", signif(x[[1]][1], 4), p_value, sep = "")
  # within lambdas
  lambda <- signif(x[[4]], 4)
  contr_2 <-  paste(
    paste(names(lambda), "=", lambda, collapse = "; "),
    collapse = NULL
  )
  r_effect <- signif(x[[5]][1], 4)
  opposite <- ifelse(
    r_effect < 0,
    " Attention: Contrast fits in the opposite direction!",
    ""
  )
  cat("\nWe ran a contrast analysis for the following between contrasts: ",
      contr_1, " and within contrasts: ", contr_2)
  cat(". This resulted in statistics of ", p,
      " and an effect magnitude of r_effectsize = ", r_effect, ". ", opposite)
}
