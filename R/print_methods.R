#' Output of between-subject design contrast analysis
#'
#' @param x output of calc_contrast
#' @param ... further arguments
#' @return Displays the significance of the contrast analysis. The contrast
#' weights, the corresponding group and an effectsize are given.
#' @export
print.cofad_bw <- function(x, ...) {
  p_value <- signif(x[[1]][2], 4)
  p_value <- paste("; p = ", p_value, sep = "")
  p <- paste("F(1,", x[[1]][4], ") = ", round(x[[1]][1], 3), p_value, sep = "")
  lambda <- signif(x[[3]], 4)
  contr_1 <-  paste(
    paste(names(lambda), "=", lambda, collapse = "; "),
    collapse = NULL
  )
  r_effect <- round(x[[4]][1], 3)
  opposite <- ifelse(
    r_effect < 0,
    "Attention: Contrast fits in the opposite direction!",
    ""
  )
  cat("\nWe ran a contrast analysis for the following between contrasts: ",
      contr_1, ". ", sep = "")
  cat("This resulted in statistics of ", p,
      " and an effect magnitude of r_effectsize = ", r_effect, ". ",
      opposite, sep = "")
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
  p <- paste("t(", x[[1]][3], ") = ", round(x[[1]][1], 3), p_value, sep = "")
  lambda <- signif(x[[3]], 4)
  contr_1 <-  paste(
    paste(names(lambda), "=", lambda, collapse = "; "),
    collapse = NULL
  )
  g_effect <- round(x[[4]][2], 3)
  opposite <- ifelse(
    g_effect < 0,
    "Attention: Contrast fits in the opposite direction!",
    ""
  )
  cat("\nWe ran a contrast analysis for the following within contrasts: ",
      contr_1, ". ", sep = "")
  cat("This resulted in statistics of ", p,
      " and an effect magnitude of g_effectsize = ", g_effect, ". ",
      opposite, sep = "")
}
#' Output of a mixed design contrast analysis
#'
#' @param x output of calc_contrast
#' @param ... further arguments
#' @return Displays the significance of the contrast analysis. The
#' contrastweights, the corresponding group and an effectsize are given.
#' @export
print.cofad_mx <- function(x, ...) {
  print.cofad_bw(x)
}
