#' Output of between-subject design contrast analysis
#'
#' @param x output of calc_contrast
#' @param ... further arguments
#' @return Displays the significance of the contrast analysis. The contrast
#'   weights, the corresponding group and an effectsize are given.
#' @export
print.cofad_bw <- function(x, ...) {
  p_value <- round(x[[1]][2], 8)
  p_value <- ifelse(
    p_value < .00000001,
    paste("; p < .00000001", sep = ""),
    paste("; p = ", p_value, sep = "")
  )
  p <- paste("F(1,", x[[1]][4], ") = ", round(x[[1]][1], 3), p_value, sep = "")
  lambda <- round(x[[3]], 3)
  contr_1 <-  paste(
    "Contrast: ",
    paste(names(lambda), "=", lambda, collapse = "; "),
    collapse = NULL
  )
  r_effect <- round(x[[4]][1], 3)
  r_effect_1 <- ifelse(
    r_effect < 0,
    paste("r_effectsize = ", r_effect, "  CAVE: F-Value for opposite contrast",
          sep = ""),
    paste("r_effectsize = ", r_effect, sep = "")
  )
  cat("\nContrast Analysis for between factor design\n\n")
  cat(p, contr_1, r_effect_1, sep = "\n")
}
#' Output of a within subject design contrast analysis
#'
#' @param x output of calc_contrast
#' @param ... further arguments
#' @return Displays the significance of the contrast analysis. The
#'   contrastweights, the corresponding group and an effectsize are given.
#' @export
print.cofad_wi <- function(x, ...) {
  l_val <- paste(
    "L-Values: Mean = ", round(x[[2]][1], 3), "; SD = ", round(x[[2]][3], 3)
  )
  p_value <- round(x[[1]][2], 8)
  p_value <- ifelse(
    p_value < .00000001,
    paste("; p < .00000001", sep = ""),
    paste("; p = ", p_value, sep = "")
  )
  p <- paste("t(", x[[1]][3], ") = ", round(x[[1]][1], 3), p_value, sep = "")
  lambda <- round(x[[3]], 3)
  contr_1 <-  paste(
    "Contrast: ",
    paste(names(lambda), "=", lambda, collapse = "; "),
    collapse = NULL
  )
  g_effect <- round(x[[4]][2], 3)
  g_effect_1 <- paste("g_contrast = ", g_effect, sep = "")
  cat("\nContrast Analysis for within factor design\n\n")
  cat(l_val, p, contr_1, g_effect_1, sep = "\n")
}
#' Output of a mixed design contrast analysis
#'
#' @param x output of calc_contrast
#' @param ... further arguments
#' @return Displays the significance of the contrast analysis. The
#'   contrastweights, the corresponding group and an effectsize are given.
#' @export
print.cofad_mx <- function(x, ...) {
  p_value <- round(x[[1]][2], 8)
  p_value <- ifelse(
    p_value < .00000001,
    paste("; p < .00000001", sep = ""),
    paste("; p = ", p_value, sep = "")
  )
  p <- paste("t(", x[[1]][3], ") = ", round(x[[1]][1], 3), p_value, sep = "")
  # between lambdas
  lambda <- round(x[[3]], 3)
  contr_1 <-  paste(
    "Contrast between: ",
    paste(names(lambda), "=", lambda, collapse = "; "),
    collapse = NULL
  )
  p_value <- round(x[[1]][2], 3)
  p_value <- ifelse(
    p_value < .001,
    paste("; p < .001", sep = ""),
    paste("; p = ", p_value, sep = "")
  )
  p <- paste("F(1,", x[[1]][4], ") = ", round(x[[1]][1], 3), p_value, sep = "")
  # within lambdas
  lambda <- round(x[[4]], 3)
  contr_2 <-  paste(
    "Contrast within: ",
    paste(names(lambda), "=", lambda, collapse = "; "),
    collapse = NULL
  )
  r_effect <- round(x[[5]][1], 3)
  r_effect_1 <- ifelse(
    r_effect < 0,
    paste("r_effectsize = ", r_effect, "  CAVE: F-Value for opposite contrast",
          sep = ""),
    paste("r_effectsize = ", r_effect, sep = "")
  )
  cat("\nContrast Analysis for Mixed-Design:\n\n")
  cat(p, contr_1, contr_2, r_effect_1, sep = "\n")
}
