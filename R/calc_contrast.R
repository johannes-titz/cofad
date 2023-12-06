#' Calculate contrast analysis for factorial designs
#'
#' @param dv dependent variable. Values must be numeric.
#' @param between independent variable that divides the data into
#' independent groups. Vector must be a factor.
#' @param lambda_between contrast weights must be a named numeric.
#' Names must match the levels of \code{between}. If
#' \code{lambda_between}
#' does not sum up to zero, this will be done automatically.
#' @param within independent variable which divides the data into
#' dependent groups. This must be a factor.
#' @param lambda_within contrast must be a named numeric.
#' Names must match the levels of \code{between}. If
#' \code{lambda_between}
#' does not sum up to zero, this will be done automatically.
#' @param id identifier for cases or subjects is needed
#' for within- and mixed contrast analysis.
#' @param data optional argument for the \code{data.frame}
#' containing \code{dv} and groups.
#' @param ID deprecated, use id instead
#' @details For multi-factorial designs, the lambda weights of
#' the factors must be connected.
#'
#' Note that cofad returns one-sided p-values for t-tests.
#'
#' @return an object of type cofad_bw or cofad_wi or cofad_mx, including
#'   p-value, F-value, contrast weights, different effect sizes. Call summary
#'   on this object to get a nice overview of all relevant statistics. Call
#'   print to get a short text that can be used for a report.
#' @references Rosenthal, R., Rosnow, R.L., & Rubin, D.B. (2000). Contrasts and
#'   effect sizes in behavioral research: A correlational approach. New York:
#'   Cambridge University Press.
#' @examples
#' # Example for between-subjects design Table 3.1 from
#' # Rosenthal, Rosnow and Rubin (2001)
#'
#' data(rosenthal_tbl31)
#' contr_bw <- calc_contrast(
#'    dv = dv,
#'    between = between,
#'    lambda_between = c("A" = -3, "B" = -1, "C" = 1, "D" = 3),
#'    data = rosenthal_tbl31)
#' contr_bw
#' summary(contr_bw)
#'
#' # Example for within-subjects design Calculation 16.6 from
#' # Sedlmeier and Renkewitz (2018, p. 537)
#'
#' data(sedlmeier_p537)
#' contr_wi <- calc_contrast(
#'    dv = reading_test,
#'    within = music,
#'    id = participant,
#'    lambda_within = c(
#'      "without music" = 1.25,
#'      "white noise" = 0.25,
#'      "classic" = -0.75,
#'      "jazz" = -0.75
#'    ),
#'    data = sedlmeier_p537
#'  )
#' contr_wi
#' summary(contr_wi, ci = .90)
#'
#' # Example for mixed-design Table 5.3 from
#' # Rosenthal, Rosnow and Rubin (2001)
#'
#' data(rosenthal_tbl53)
#'
#' contr_mx <- calc_contrast(dv = dv, between = between,
#'               lambda_between = c("age8" = -1, "age10" = 0, "age12" = 1),
#'               within = within,
#'               lambda_within = c("1" = -3, "2" = -1,"3" = 1, "4" = 3),
#'               id = id, data = rosenthal_tbl53
#'               )
#' contr_mx
#' summary(contr_mx)
#'
#' @export
#' @import stats
calc_contrast <- function(dv,
                          between = NULL,
                          lambda_between = NULL,
                          within = NULL,
                          lambda_within = NULL,
                          ID = NULL,
                          id = NULL,
                          data = NULL) {
  if (!is.null(data) & is.data.frame(data)) {
    arguments <- as.list(match.call())
    if (is.character(arguments$dv)) dv <- data[, arguments$dv] else dv <- eval(arguments$dv, data)
    if (is.character(arguments$between)) between <- data[, arguments$between] else between <- eval(arguments$between, data)
    if (is.character(arguments$within)) within <- data[, arguments$within] else within <- eval(arguments$within, data)
    if (!missing(ID)) {
      warning("argument ID is deprecated; please use id instead.",
              call. = FALSE)
      id <- eval(arguments$ID, data)
    } else {
      id <- eval(arguments$id, data)
    }
  } else if (!is.null(data) & !is.data.frame(data)) {
    stop("data is not a data.frame")
  }

  # check types
  if (!is.numeric(dv)) {
    stop("dependent variable must be numeric")
  }

  between <- check_if_factor(between)
  within <- check_if_factor(within)
  id <- check_if_factor(id)

  if (is.null(between) & is.null(within)) {
    stop(
      "Independent Variable is missing, either set between or within or both."
    )
  }

  if (is.null(lambda_between) & is.null(lambda_within)) {
    stop(
      "Lambdas are missing, either set lambda_between or lambda_within or both."
    )
  }

  lambda_between <- check_lambda(lambda_between)
  lambda_within <- check_lambda(lambda_within)
  check_labels(between, lambda_between)

  if (!is.null(within) & !is.null(lambda_within)) {
    if (anyNA(match(levels(within), names(lambda_within)))) {
      stop("lambda names doesn't match all within labels")
    }
  }
  if (!is.null(lambda_between) & is.null(lambda_within)) {
    case <- "Analysis between groups"
  }
  if (is.null(lambda_between) & !is.null(lambda_within)) {
    case <- "Analysis within cases"
  }
  if (!is.null(lambda_between) & !is.null(lambda_within)) {
    case <- "mixed-Analysis: between and within factors"
  }
  if (any(is.null(between), is.null(lambda_between)) &
      case == "Analysis between groups") {
    stop("Missing arguments")
  }
  if (any(is.null(within), is.null(lambda_within),
          is.null(id)) & case == "Analysis within cases") {
    stop("Missing arguments")
  }
  if (any(
    is.null(between), is.null(lambda_between),
    is.null(within), is.null(lambda_within),
    is.null(id)) &
    case == "mixed-Analysis: between and within factors") {
    stop("Missing arguments")
  }

  if (anyNA(dv)) {
    index_na <- which(is.na(dv))
    dv <- dv[-index_na]
    between <- between[-index_na]
    within <- within[-index_na]
    id <- id[-index_na]
    warning("NAs in dependent variable are omitted")
  }
  if (anyNA(between)) {
    index_na <- which(is.na(between))
    dv <- dv[-index_na]
    between <- between[-index_na]
    within <- within[-index_na]
    id <- id[-index_na]
    warning("NAs in between are omitted")
  }
  if (anyNA(within)) {
    index_na <- which(is.na(within))
    dv <- dv[-index_na]
    between <- between[-index_na]
    within <- within[-index_na]
    id <- id[-index_na]
    warning("NAs in within are omitted")

  }
  if (anyNA(id)) {
    index_na <- which(is.na(id))
    dv <- dv[-index_na]
    between <- between[-index_na]
    within <- within[-index_na]
    id <- id[-index_na]
    warning("NAs in id are omitted")
  }

  if (case == "Analysis between groups") {
    return(run_between_analysis(dv, between, lambda_between))
  }

  if (case == "Analysis within cases") {
    return(run_within_analysis(dv, within, between, lambda_within, id))
  }

  if (case == "mixed-Analysis: between and within factors") {
    lambda_within <- lambda_within[levels(within)]
    lambda_between <- lambda_between[levels(between)]
    prodsum <- lambda_within[within] * dv
    data_new <- aggregate(prodsum, by = list(id, between), FUN = "sum")
    obj <- run_between_analysis(dv = data_new$x,
                                between = data_new$Group.2,
                                lambda_between = lambda_between)
    class(obj) <- "cofad_mx"
    obj
  }
}

#' Between contrast analysis
#'
#' internal function
#'
#' @inheritParams calc_contrast
#' @noRd
run_between_analysis <- function(dv, between, lambda_between) {
  lambda_between <- lambda_between[levels(between)]
  ni <- table(between)
  n_total <- sum(table(dv))
  k <- length(ni)
  df_inn <- n_total - k
  df_contrast <- 1
  lambda_between_row <- rep(NA, sum(ni))
  for (i in seq(lambda_between)) {
    lambda_between_row <- replace(
      x = lambda_between_row,
      list = which(between == names(lambda_between)[i]),
      lambda_between[i]
    )
  }
  var_within <- tapply(X = dv, INDEX = between, FUN = var)
  ms_within <- mean(var_within, na.rm = T)
  mean_i <- tapply(X = dv, INDEX = between, FUN = mean)
  se_i <- tapply(X = dv, INDEX = between, FUN = sd) / sqrt(ni)

  # bring lambda_between in same order as between variable
  lambda_between <- lambda_between[levels(between)]
  L <- sum(mean_i * lambda_between)
  ss_kontrast <- L^2/(sum(lambda_between^2 / ni))
  ss_total <- sum((dv - mean(dv)) ** 2)
  ss_between <- sum(ni * (mean_i - mean(mean_i))^2)
  ss_within <- ss_total - ss_between
  f_contrast <- ((L ** 2) / (ms_within)) *
    (1 / sum((lambda_between ** 2) / ni))
  p_contrast <- pf(f_contrast, 1, df_inn, lower.tail = F)
  r_effectsize <- cor(lambda_between_row, dv)
  if (sd(mean_i) == 0) {
    r_alerting <- NA
    r_contrast <- NA
    warning("SD of group means is zero")
  } else {
    r_alerting <- cor(lambda_between, mean_i)
    sign_r_contrast <- sign(r_effectsize)
    r_contrast <- sign_r_contrast * (r_effectsize * r_alerting) /
      (sqrt(
        r_effectsize ** 2 * r_alerting ** 2 - r_effectsize ** 2
        + r_alerting ** 2))
  }
  sig <- cn(f_contrast, p_contrast, df_contrast, df_inn, ms_within,
            ss_between, ss_kontrast, ss_total, ss_within,
            df_total = n_total - 1, L)
  effects <- cn(r_effectsize, r_contrast, r_alerting)
  desc <- matrix(c(mean_i, se_i), ncol = 2, byrow = F)
  colnames(desc) <- c("mean_i", "se_i")
  out_l <- tibble::lst(sig, desc, lambda_between, effects)
  class(out_l) <- c("cofad_bw")
  structure(out_l)
  return(out_l)
}

#' Within contrast analysis
#'
#' internal function
#'
#' @inheritParams calc_contrast
#' @noRd
run_within_analysis <- function(dv, within, between, lambda_within, id) {
  lambda_within <- lambda_within[levels(within)]
  ni_within <- table(within)
  n_total <- sum(ni_within)
  l_value <- NULL
  for (i in seq(table(id))) {
    var_i <- dv[which(id == levels(id)[i])]
    l_value[i] <- sum(
      var_i[order(within[which(id == levels(id)[i])])] * lambda_within
    )
  }
  if (!is.null(between)) {
    ni_bw <- table(between)
    bw_wide <- matrix(NA, ncol = 2, nrow = length(levels(id)))
    for (i in seq(levels(id))) {
      id_bw <- as.character(unique(
        between[which(id == levels(id)[i])]
      ))
      if (length(id_bw) > 1) {
        stop("some id's are in more than one between group")
      } else {
        bw_wide[i, 1] <- levels(id)[i]
        bw_wide[i, 2] <- id_bw
      }
    }
    ni_l_value <- table(bw_wide[, 2])
    s2i <- tapply(l_value, as.factor(bw_wide[, 2]), var)
    if (anyNA(s2i)) {
      s2i[which(is.na(s2i))] <- 0
    }
    s2 <- sum((ni_l_value - 1) * s2i) / sum(ni_l_value - 1)
    k_bw <- length(ni_bw)
    df_within <- n_total - k_bw
  } else {
    s2 <- var(l_value)
    k_bw <- 1
    df_id <- length(l_value) - 1
    df_within <- n_total - k_bw
  }
  #df_contrast <- 1
  if (!is.null(between)) {
    df_id <- sum(ni_l_value) - k_bw
    ni_cell <- table(within, between)
  } else {
    ni_cell <- ni_within
  }
  harm_n <- 1 / mean(1 / ni_cell)
  if (!is.null(between)) {
    l_value <- tapply(l_value, bw_wide[, 2], mean)
  }
  t_value <- mean(l_value) / sqrt((1 / (k_bw * harm_n)) * s2)
  f_contrast <- t_value ** 2
  #ss_total <- sum(((dv - mean(dv)) ** 2))
  p_contrast <- pt(t_value, df_id, lower.tail = F)
  g_effect <- mean(l_value) / (sqrt(s2))
  sign_r_contrast <- sign(g_effect)
  r_contrast <- sign_r_contrast * sqrt(f_contrast / (f_contrast + df_within))
  sig <- c(t_value, p_contrast, df_id)
  desc <- c(mean(l_value), sqrt(s2) / sqrt(sum(table(l_value))), sqrt(s2))
  r <- c(r_contrast, g_effect)
  out_l <- list(sig, desc, lambda_within, r)
  names(out_l) <- c("sig", "desc", "lambda_within", "effects")
  class(out_l) <- c("cofad_wi")
  structure(out_l)
  return(out_l)
}

#' Validates that lambda is correct
#'
#' internal function
#'
#' @noRd
check_lambda <- function(lambda) {
  if (!is.null(lambda)) {
    if (!is.numeric(lambda)) {
      stop("lambda must be a named numeric")
    }
    if (is.null(names(lambda))) {
      stop("lambda must be a named numeric")
    }
    if (anyNA(lambda)) {
      stop("NA in lambda is not allowed")
    }
  }
  if (sum(lambda) != 0) {
    lambda <- round(lambda - mean(lambda), 3)
    message("lambdas are centered and rounded to 3 digits")
  }
  lambda
}

#' Validates that every condition gets a lambda
#'
#' internal function
#'
#' @noRd
check_labels <- function(between, lambda_between) {
  if (!is.null(between) & !is.null(lambda_between)) {
    if (anyNA(match(levels(between), names(lambda_between)))) {
      stop("lambda names doesn't match all between labels")
    }
  }
}

#' Validates that variable is a factor
#'
#' internal function
#'
#' @noRd
check_if_factor <- function(variable) {
  if (!is.factor(variable) & !is.null(variable)) {
    warning(
      deparse(substitute(variable)),
      " is not a factor. I will try to convert it to a factor."
    )
    variable <- as.factor(variable)
  }
  return(variable)
}

#' Calculate r_alerting from r_contrast and r_effectsize
#'
#' Convenience function to transform effect sizes in contrast analyses.
#'
#' @param r_contrast what it says
#' @param r_effectsize what it says
#'
#' @export
calc_r_alerting <- function(r_contrast, r_effectsize) {
  numerator <- - r_effectsize * r_contrast
  denominator <- sqrt((1 + r_effectsize^2) * r_contrast^2 - r_effectsize^2)
  r_alerting <- as.numeric(numerator / denominator)
  return(cn(r_alerting))
}

#' Calculate r_alerting from F-values
#'
#' Convenience function to calculate effect sizes in contrast analyses.
#'
#' @param f_contrast F value from contrast analysis
#' @param f_between F value from ANOVA (one between variable!)
#' @param df_between degrees of freedom of ANOVA
#' @export
calc_r_alerting_from_f <- function(f_contrast, f_between, df_between) {
  return(sqrt(f_contrast / (f_between * df_between)))
}

#' Calculate r_contrast from r_alerting and r_effectsize
#'
#' Convenience function to transform effect sizes in contrast analyses.
#'
#' @param r_alerting what it says
#' @param r_effectsize what it says
#'
#' @export
calc_r_contrast <- function(r_alerting, r_effectsize) {
  sign_r_contrast <- sign(r_effectsize)
  numerator <- r_contrast <- sign_r_contrast * (r_effectsize * r_alerting)
  denominator <- sqrt(
    r_effectsize^2 * r_alerting^2 - r_effectsize^2 + r_alerting^2
  )
  r_contrast <- as.numeric(numerator / denominator)
  return(cn(r_contrast))
}

#' Calculate r_effectsize from r_contrast and r_alerting
#'
#' Convenience function to transform effect sizes in contrast analyses.
#'
#' @param r_alerting what it says
#' @param r_contrast what it says
#'
#' @export
calc_r_effectsize <- function(r_alerting, r_contrast) {
  numerator <- - r_contrast * r_alerting
  denominator <- sqrt(- (r_contrast^2) * r_alerting^2 + r_contrast^2 +
                        r_alerting^2)
  r_effectsize <- as.numeric(numerator / denominator)
  return(cn(r_effectsize))
}

