#' Calculate between contrast analysis from aggregated data (means, sds and ns)
#'
#' @param means vector of mean values for every condition
#' @param sds vector of standard deviation values for every condition
#' @param ns vector of sample size values for every condition
#' @param lambdas vector of lambda values for every condition
#'
#' @return Calculates the significance of the contrast analysis.
#  The contrastweights, the corresponding group and an effectsize are
#' given.
#'
#' @references Rosenthal, R., Rosnow, R.L., & Rubin, D.B. (2000).
#' Contrasts and effect sizes in behmeansioral research:
#' A correlational approach. New York: Cambridge University Press.
#' @examples
#'
#' data from Betsch et al. (2010), experiment 1
#'
#'
calc_contrast_aggregated <- function(means, sds, ns, lambdas){
  df_between <- length(means) - 1
  df_within <- sum(ns) - length(means)
  sigma_within <- sum(sds^2*ns) / (sum(ns))
  ss_between <- ss(means, ns)
  sigma_between <- ss_between / df_between
  F_between <- sigma_between / sigma_within
  # contrast
  kov <- sum(lambdas * means)
  ss_contrast <- kov^2 / (sum((lambdas^2) / ns))
  # ss contrast is the same as sigma contrast
  F_contrast <- ss_contrast / sigma_within
  # which direction?
  sign <- ifelse(kov > 0, 1, -1)
  r_effectsize <- sign * sqrt(F_contrast / (F_between * df_between + df_within))
  r_contrast <- sqrt(F_contrast/(F_contrast + df_within))
  r_alerting <- sqrt(F_contrast/(F_contrast * df_between))
  N <- sum(ns)
  p_contrast <- pf(F_contrast, df_between, df_within)
  #data.frame(r_es = r_effect_size, N = sum(ns), n_cell_mean = mean(ns))
  # Markus object creation
  sig <- c(F_contrast, p_contrast, df_contrast = 1, df_within, sigma_within,
           ss_between, ss_total = NA, N - 1)
    r <- c(r_effectsize, r_contrast, r_alerting)
    desc <- matrix(c(means, sds / sqrt(ns)), ncol = 2, byrow = F)
    colnames(desc) <- c("M", "SE")
    out_l <- list(sig, desc, lambdas, r)
    names(out_l) <- c("sig", "desc", "lambda_between", "effects")
    class(out_l) <- c("cofad_bw")
    structure(out_l)
    return(out_l)
}

ss <- function(x, n) {
  sum(n*(x-mean(x))^2)
}
