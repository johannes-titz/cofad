#' Calculate between contrast analysis from aggregated data (means, sds and ns)
#'
#' @param means named character, vector of mean values for every condition in
#'   data
#' @param sds named character, vector of standard deviation values for every
#'   condition in data
#' @param ns named character, vector of sample size values for every condition
#'   in data
#' @param lambdas contrast weights must be a named numeric.
#' Names must match the levels of \code{between}. If
#' \code{lambda_between}
#' does not sum up to zero, this will be done automatically.
#' @param between independent variable that divides the data into
#' independent groups (a factor).
#' @param data optional argument for the \code{data.frame} containing all variables
#' except for lambdas
#' @return Calculates the significance of the contrast analysis. The
#contrastweights, the corresponding group and an effectsize are ' given.
#'
#' @references Rosenthal, R., Rosnow, R.L., & Rubin, D.B. (2000). Contrasts and
#'   effect sizes in behmeansioral research: A correlational approach. New York:
#'   Cambridge University Press.
#'
#' @examples
#'
#' @export
calc_contrast_aggregated <- function(means, sds, ns, lambdas, between, data){
  if (!is.null(data) & (is.data.frame(data))) {
    arguments <- as.list(match.call())
    means <- eval(arguments$means, data)
    sds <- eval(arguments$sds, data)
    ns <- eval(arguments$ns, data)
    #lambdas <- eval(arguments$lambdas, data)
    between <- eval(arguments$between, data)
  } else if (!is.null(data) & !is.data.frame(data)){
    stop("data is not a data.frame")
  }
  lambdas <- check_lambda_between(lambdas)
  check_labels(between, lambdas)
  # correctly sort lambdas
  lambdas_pos <- sapply(between, function(x) which(x == names(lambdas)))
  lambdas <- lambdas[lambdas_pos]

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
  p_contrast <- 1 - pf(F_contrast, 1, df_within)
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

#' helper function to calculate between sum of squares
#'
#' @param x vector of mean values
#' @param n vector of sample sizes values
#' @noRd
ss <- function(x, n) {
  sum(n*(x-mean(x))^2)
}
