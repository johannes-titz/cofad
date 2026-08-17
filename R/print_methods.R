format_report_p <- function(p, digits = 4, scientific_cutoff = 0.001) {
  if (is.na(p)) return("NA")
  if (p != 0 && p < scientific_cutoff) {
    return(formatC(p, format = "e", digits = digits - 1))
  }
  as.character(signif(p, digits))
}

#' Output of between-subject design contrast analysis
#'
#' @param x output of calc_contrast
#' @param ... further arguments
#' @return Displays the significance of the contrast analysis. The contrast
#' weights, the corresponding group and an effectsize are given.
#' @export
print.cofad_bw <- function(x, ...) {
  t_value <- sqrt(x[[1]][1]) * sign(x[[4]][1])
  p_value <- format_report_p(stats::pt(
    t_value, x[[1]][4], lower.tail = FALSE
  ))
  p_value <- paste("; p = ", p_value, sep = "")
  p <- paste("t(", x[[1]][4], ") = ", round(t_value, 3), p_value, sep = "")
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
      " and an effect magnitude of r\u2091\u209b = ", r_effect, ". ",
      opposite, sep = "")
}
#' Output of a within subject design contrast analysis
#'
#' @param x output of calc_contrast
#' @param ... further arguments
#' @return Displays the significance of the contrast analysis. The
#' contrast weights, the corresponding group, and an effect size are given.
#' @export
print.cofad_wi <- function(x, ...) {
  score_name <- if (identical(x$within_score, "r")) "r" else "L"
  score_explanation <- if (score_name == "r") {
    "correlations measuring agreement with the predicted pattern"
  } else {
    "weighted sums retaining response magnitude"
  }
  p_value <- format_report_p(x[[1]][2])
  p_value <- paste("; p = ", p_value, sep = "")
  p <- paste("t(", x[[1]][3], ") = ", round(x[[1]][1], 3), p_value, sep = "")
  lambda <- signif(x[[3]], 4)
  contr_1 <-  paste(
    paste(names(lambda), "=", lambda, collapse = "; "),
    collapse = NULL
  )
  r_contrast <- round(x[[4]][1], 3)
  opposite <- ifelse(
    r_contrast < 0,
    "Attention: Contrast fits in the opposite direction!",
    ""
  )
  cat("\nWe ran a within-subjects contrast analysis using participant-level ",
      score_name, " scores (", score_explanation,
      ") for the following contrast weights: ", contr_1, ". ", sep = "")
  cat("This resulted in ", p,
      " and an effect magnitude of r_contrast = ", r_contrast, ". ",
      opposite, sep = "")
}
#' Output of a mixed design contrast analysis
#'
#' @param x output of calc_contrast
#' @param ... further arguments
#' @return Displays the significance of the contrast analysis. The
#' contrast weights, the corresponding group, and an effect size are given.
#' @export
print.cofad_mx <- function(x, ...) {
  score_name <- if (identical(x$within_score, "r")) "r" else "L"
  score_explanation <- if (score_name == "r") {
    "correlations measuring agreement with the predicted pattern"
  } else {
    "weighted sums retaining response magnitude"
  }
  t_value <- sqrt(x$sig[["f_contrast"]]) *
    sign(x$effects[["r_effectsize"]])
  p_value <- stats::pt(
    t_value, x$sig[["df_inn"]], lower.tail = FALSE
  )
  statistic <- paste0("t(", x$sig[["df_inn"]], ") = ",
                      round(t_value, 3), "; p = ", format_report_p(p_value))
  lambda <- signif(x$lambda_between, 4)
  weights <- paste(names(lambda), "=", lambda, collapse = "; ")
  r_effect <- round(x$effects[["r_effectsize"]], 3)
  opposite <- if (r_effect < 0) {
    " Attention: The contrast fits in the opposite direction."
  } else {
    ""
  }
  cat(
    "\nWe ran a mixed contrast analysis using participant-level ", score_name,
    " scores (", score_explanation, ") for the within-subjects contrast and ",
    "the following between-subjects contrast weights: ", weights, ". This ",
    "resulted in ", statistic, " and an effect magnitude of r\u2091\u209b = ",
    r_effect, ".", opposite,
    sep = ""
  )
}
