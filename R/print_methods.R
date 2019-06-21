# print_methods
print.cofad_bw <- function(x, ...) {
  p_value <- round(x[[1]][2], 3)
  p_value <- ifelse (p_value < .001,
                     paste("; p < .001", sep = ""),
                     paste("; p = ", p_value, sep = "")
  )
  p <- paste("F(1,", x[[1]][4], ") = ", round(x[[1]][1], 3),
             p_value, sep = "")
  lambda <- x[[3]]
  contr_1 <-  paste("Contrasts: ",
                    paste(names(lambda), "=", lambda, collapse = "; "),
                    collapse = NULL)
  r_effect <- round(x[[4]][1], 3)
  r_effect_1 <- ifelse (r_effect < 0,
                        paste("r_effectsize = ", r_effect,
                              "  CAVE: F-Value for opposite contrast",
                              sep = ""),
                        paste("r_effectsize = ", r_effect, sep = "")
  )
  cat("\nContrast Analysis for between factor design\n\n")
  cat(p, contr_1, r_effect_1, sep = "\n" )
}
print.cofad_wi <- function(x, ...) {
  L_val <- paste("L-Values: Mean = ", round(x[[2]][1], 3),
                 " ; SD = ", round(x[[2]][3], 3) )
  p_value <- round(x[[1]][2], 3)
  p_value <- ifelse (p_value < .001,
                     paste("; p < .001", sep = ""),
                     paste("; p = ", p_value, sep = "")
  )
  p <- paste("t(", x[[1]][3], ") = ", round(x[[1]][1], 3),
             p_value, sep = "")
  lambda <- x[[3]]
  contr_1 <-  paste("Contrasts: ",
                    paste(names(lambda), "=", lambda, collapse = "; "),
                    collapse = NULL)
  g_effect <- round(x[[4]][2], 3)
  g_effect_1 <- paste("g_contrast = ", g_effect, sep = "")
  cat("\nContrast Analysis for within factor design\n\n")
  cat(L_val, p, contr_1, g_effect_1, sep = "\n" )
}
print.cofad_mx <- function(x, ...) {
  p_value <- round(x[[1]][2], 3)
  p_value <- ifelse (p_value < .001,
                     paste("; p < .001", sep = ""),
                     paste("; p = ", p_value, sep = "")
  )

  p <- paste("t(", x[[1]][3], ") = ", round(x[[1]][1], 3),
             p_value, sep = "")
  lambda <- x[[3]]
  contr_1 <-  paste("Contrasts: ",
                    paste(names(lambda), "=", lambda, collapse = "; "),
                    collapse = NULL)
  p_value <- round(x[[1]][2], 3)
  p_value <- ifelse (p_value < .001,
                     paste("; p < .001", sep = ""),
                     paste("; p = ", p_value, sep = "")
  )
  p <- paste("F(1,", x[[1]][4], ") = ", round(x[[1]][1], 3),
             p_value, sep = "")
  lambda <- x[[3]]
  contr_1 <-  paste("Contrasts: ",
                    paste(names(lambda), "=", lambda, collapse = " "),
                    collapse = NULL)
  r_effect <- round(x[[5]][1], 3)
  r_effect_1 <- ifelse (r_effect < 0,
                        paste("r_effectsize = ", r_effect,
                              "  CAVE: F-Value for opposite contrast",
                              sep = ""),
                        paste("r_effectsize = ", r_effect, sep = "")
  )
  cat("\nContrast Analysis for Mixed-Design:\n\n")
  cat(p, contr_1, r_effect_1, sep = "\n" )
}
