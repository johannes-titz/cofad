#' Calculate contrast analysis for factorial designs
#'
#' @param variable The dependent variable. This must be numeric.
#' @param between The independent variable that divides the data into
#' independent groups. This must be a factor.
#' @param between_levels The names between the groups. The order of
#' between levels must match the order of between_lambda.
#' This must be a character
#' @param lambda_between The contrast weights must be numeric and
#' correspond to the order between  between_levels. If lambda_between
#' does not sum to zero, this will be done automatically.
#' @param within The independent variable which divides the data into
#' dependent groups. This must be a factor.
#' @param within_levels The names of the within groups. The
#' order of within_levels must match the order of  within_lambda.
#' This must be a character
#' @param lambda_within The contrast weights must be numeric and
#' correspond to the order between within_levels. If lambda_within
#' does not sum to zero, this will be done automatically.
#' @param ID The indentifire for cases or subjects. This is needed
#' for within-analysis.
#' @param data Optional argument for a data.frame containing variable
#' and groups.
#' @details For multi-factorial designs, the lambdaweights of
#' the factors must be connected.
#' @return Calculates the significance of the contrast analysis.
#  The contrastweights, the corresponding group and an effectsize are
#' given.
#' @references Rosenthal, R., Rosnow, R.L., & Rubin, D.B. (2000).
#' Contrasts and effect sizes in behavioral research:
#' A correlational approach. New York: Cambridge University Press.
#' @examples
#' # Exampel for between-subjects-design Table 3.1 from
#' # Rosenthal, Rosnow and Rubin (2001)
#'
#' tab31 <- data.frame(
#'   Val = c(2, 6, 8, 4,10, 6, 8, 10, 4, 12, 8,
#'     16, 10, 14, 12, 12,  18, 14, 20, 16),
#'   Let = rep(c("A", "B", "C", "D"), c(5, 5, 5, 5))
#'   )
#' contr_bw <- calc_contrast(variable = Val,
#'                  between = Let,
#'                  between_levels = c("A", "B", "C", "D"),
#'                  lambda_between = c(-3, -1, 1, 3),
#'                  data = tab31)


summary.cofad_bw <- function(object, ...){
  x <- object
  f_tab <- matrix(c(
    round(x[[1]][1] * x[[1]][5], 3), round(x[[1]][3]),
    round(x[[1]][1] * x[[1]][5], 3), round(x[[1]][1], 3),
    round(x[[1]][2], 3),
    round(x[[1]][5] * x[[1]][4], 3), round(x[[1]][4]),
    round(x[[1]][5], 3), NA, NA,
    round(x[[1]][7], 3), round(x[[1]][8]),
    NA, NA, NA),
    byrow = T, ncol = 5)
  rownames(f_tab) <- c("contrast", "within", "total")
  colnames(f_tab) <- c("SS", "df", "MS", "F", "p")
  r_tab <- round(matrix(c(x[[4]]), ncol = 1), 3)
  rownames(r_tab) <- c("r_effectsize", "r_contrast", "r_alerting")
  colnames(r_tab) <- c("effects")
  out <- list(f_tab, r_tab)
  names(out) <- c("F-Table", "Effects")
  return(out)
}
summary.cofad_wi <- function(object, ci = .95, ...){
  x <- object
  L_M <- x[[2]][[1]]
  L_SE <- x[[2]][2]
  L_df <- x[[1]][3]
  L_p <- x[[1]][2]
  L_SE_ci <- qt(p = (1 - ci) / 2, df = L_df, lower.tail = F) * L_SE
  L_UB <- L_M + L_SE_ci
  L_LB <- L_M - L_SE_ci
  L_vals <- matrix(c(
    L_M, L_SE, L_df, L_p, L_LB, L_UB), ncol = 6)
  L_eff <- matrix(c(x[[4]][1], x[[4]][2]))
  rownames(L_eff) <- c("r-contrast", "g-contrast")
  colnames(L_vals) <- c("Mean", "SE", "df", "p",
                        "CI-lower", "CI-upper")
  out <- list(L_vals, L_eff)
  names(out) <- c("L-Statistics", "Effects")
  return(out)
}
summary.cofad_mx <- function(object, ...){
  x <- object
  all_L <- as.vector(x[[6]])
  all_L <- all_L[which(!is.na(all_L))]
  SS_total <- sum(
    (all_L - mean(all_L)) ** 2)
  df_total <- length(all_L) - 1
  f_tab <- matrix(c(
     SS_contrast <- x[[1]][5],
     df_contrast <- 1,
     MS_contrast <- x[[1]][5],
     F_contrast <- x[[1]][1],
     p_contrast <- x[[1]][2],
     SS_within  <- x[[1]][6] * x[[1]][4],
     df_within <- x[[1]][4],
     MS_within <-  x[[1]][6], NA, NA,
     SS_total, df_total, NA, NA, NA),
     ncol = 5, byrow = T)
  f_tab <- round(f_tab, 3)
  L_mean <- apply(x[[6]], 2, mean, na.rm = T)
  L_sd <- apply(x[[6]], 2, sd, na.rm = T)
  l_tab <- round(matrix(c(L_mean, L_sd),
                        nrow = 2, byrow = T), 3)
  colnames(l_tab) <- names(L_mean)
  rownames(l_tab) <- c("mean", "sd")
  r_tab <- round(matrix(c(x[[5]]), ncol = 1), 3)
  colnames(r_tab) <- "effect"
  rownames(r_tab) <- c("r_effectsize", "r_contrast", "r_alerting")
  rownames(f_tab) <- c("contrast", "within", "total")
  colnames(f_tab) <- c("SS", "df", "MS", "F", "p" )
  out <- list(f_tab, r_tab, l_tab)
  names(out) <- c("F_Table", "Effects", "Within_Groups")
  return(out)
}
