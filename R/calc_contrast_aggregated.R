#' Calculate between contrast analysis from aggregated data (means, sds and ns)
#'
#' @param means numeric vector of mean values for every condition
#' @param sds numeric vector of standard deviation values for every condition
#' @param ns numeric vector of sample size values for every condition
#' @param between factor for the independent variable that divides the data into
#'   independent groups
#' @param lambda_between numeric vector for contrast weights. Names must match
#'   the levels of \code{between}. If \code{lambda_between} does not sum up to
#'   zero, this will be done automatically (centering).
#' @param data optional argument for the \code{data.frame} containing all
#'   variables except for lambda_between
#' @return an object of type cofad_bw, including p-value, F-value, contrast
#'   weights, different effect sizes
#'
#' @references Rosenthal, R., Rosnow, R.L., & Rubin, D.B. (2000). Contrasts and
#'   effect sizes in behavioral research: A correlational approach. New York:
#'   Cambridge University Press.
#'
#' @examples
#' library(dplyr)
#' furr_agg <- furr_p4 %>%
#'   group_by(major) %>%
#'   summarize(mean = mean(empathy), sd = sd(empathy), n = n())
#' lambdas = c("psychology" = 1, "education" = -1, "business" = 0, "chemistry" = 0)
#' calc_contrast_aggregated(mean, sd, n, major, lambdas, furr_agg)
#'
#' @export
calc_contrast_aggregated <- function(means, sds, ns, between, lambda_between,
                                     data) {
  if (!is.null(data) & (is.data.frame(data))) {
    arguments <- as.list(match.call())
    means <- eval(arguments$means, data)
    sds <- eval(arguments$sds, data)
    ns <- eval(arguments$ns, data)
    between <- eval(arguments$between, data)
  } else if (!is.null(data) & !is.data.frame(data)) {
    stop("data is not a data.frame")
  }
  lambda_between <- check_lambda(lambda_between)
  check_labels(between, lambda_between)
  # correctly sort lambda_between
  lambda_between_pos <- sapply(
    between,
    function(x) which(x == names(lambda_between))
  )
  lambda_between <- lambda_between[lambda_between_pos]

  df_between <- length(means) - 1
  df_within <- sum(ns) - length(means)
  sigma_within <- sum(sds^2 * ns) / (sum(ns))
  ss_between <- ss(means, ns)
  sigma_between <- ss_between / df_between
  f_between <- sigma_between / sigma_within
  # contrast
  kov <- sum(lambda_between * means)
  ss_contrast <- kov^2 / (sum((lambda_between^2) / ns))
  # ss contrast is the same as sigma contrast
  f_contrast <- ss_contrast / sigma_within
  # which direction?
  sign <- ifelse(kov > 0, 1, -1)
  r_effectsize <- sign * sqrt(f_contrast / (f_between * df_between + df_within))
  r_contrast <- sign * sqrt(f_contrast / (f_contrast + df_within))
  r_alerting <- sign * sqrt(f_contrast / (f_contrast * df_between))
  n_total <- sum(ns)
  p_contrast <- 1 - pf(f_contrast, 1, df_within)
  sig <- c(f_contrast, p_contrast, df_contrast = 1, df_within, sigma_within,
           ss_between, ss_total = NA, n_total - 1)
  r <- c(r_effectsize, r_contrast, r_alerting)
  desc <- matrix(c(means, sds / sqrt(ns)), ncol = 2, byrow = F)
  colnames(desc) <- c("M", "SE")
  out_l <- list(sig, desc, lambda_between, r)
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
  sum(n * (x - mean(x))^2)
}
