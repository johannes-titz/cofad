#' Calculate contrast analysis for factorial designs
#'
#' @param variable The dependent variable. This must be numeric.
#' @param between The independent variable which cut yout data into
#' independent groups. This must be a factor.
#' @param between_levels The names of the between groups. The
#' sqequence of between_levels must correspondent to the sequence
#' of  between_lambda. This must be a character
#' @param lambda_between The Contrastweights. This must be numeric and
#' correspondent to the sequence of between_levels. If
#' lambda_between not sum up to zero, this will be done automatically
#' @param within The independent variable which cut yout data into
#' dependent groups. This must be a factor.
#' @param within_levels The names of the within groups. The
#' sqequence of within_levels must correspondent to the sequence
#' of  within_lambda. This must be a character
#' @param lambda_within The Contrastweights. This must be numeric and
#' correspondent to the sequence of within_levels. If
#' lambda_within not sum up to zero, this will be done automatically
#' @param ID The cases / subjects connectet to within.
#' @param data Optional argument for a data.frame containing variable
#' and group.
#' @details For more than one wihtin or between variable,
#' contrastweights have to be connected See example 4 or 5
#' correspondent to the expected multifactorial effect.
#' @return Calculates the significance of the contrast analysis.
#  The contrastweights corresponding group and an effectsize are
#' given to evaluate the analysis.
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
#' contr_bw
#' summary(contr_bw)
#'
#' # Exampel for within-subjects-design Calculation 16.6 from
#' # Sedlmeier and Renkewitz (2018, p. 537)
#'
#' sedlmeier537 <- data.frame(
#'    Var = c(27, 25, 30, 29, 30, 33, 31, 35,
#'             25, 26, 32, 29, 28, 30, 32, 34,
#'             21, 25, 23, 26, 27, 26, 29, 31,
#'             23, 24, 24, 28, 24, 26, 27, 32),
#'    within = as.factor(rep(1:4,c(8,8,8,8))),
#'    ID = as.factor(rep(1:8,4))
#'  )
#' contr_wi <- calc_contrast(
#'    variable = Var,
#'    within = within,
#'    within_levels = as.character(c(2,3,1,4)),
#'    ID = ID,
#'    lambda_within = c(0.25,-.75,1.25,-.75),
#'    data=sedlmeier537
#'  )
#' contr_wi
#' summary(conr_wi, ci=.90)
#'
#' # Exampel for mixed-designs Table 5.3 from
#' # Rosenthal, Rosnow and Rubin (2001)
#'
calc_contrast <- function(variable,
                          between = NULL, between_levels = NULL,
                          lambda_between = NULL,
                          within = NULL, within_levels = NULL,
                          lambda_within = NULL, ID = NULL,
                          data = NULL){
  if (!is.null(data) & is.data.frame(data)) {
    arguments <- as.list(match.call())
    variable <- eval(arguments$variable, data)
    between <- eval(arguments$between, data)
    within <- eval(arguments$within, data)
    ID <- eval(arguments$ID, data)
  } else if (!is.null(data) & !is.data.frame(data)){
    stop("data is not a data.frame")
  }
  if (is.null(lambda_between) & is.null(lambda_within)) {
    stop("lambda is missing")
  }
  if (!is.numeric(variable)){
    stop("variable must be numeric")
  }
  if (!is.factor(between) & !is.null(between)){
    stop("between must be a factor")
  }
    if (!is.character(between_levels) & !is.null(between_levels)){
    stop("between_levels must be a character")
  }
  if (!is.factor(within) & !is.null(within)){
    stop("within must be a factor")
  }
  if (!is.character(within_levels) & !is.null(within_levels)){
    stop("within_levels must be a character")
  }
  if (sum(lambda_between) != 0) {
    lambda_between <- lambda_between - mean(lambda_between)
  }
  if (sum(lambda_within) != 0) {
    lambda_within <- lambda_within - mean(lambda_within)
  }
    if (!all(names(table(between)) == sort(between_levels))) {
    stop("between_levels doesn't match all between labels")
    }
  if (!all(names(table(within)) == sort(within_levels))) {
    stop("within_levels doesn't match all within labels")
  }
  if (!is.null(lambda_between) & is.null(lambda_within)) {
    case <- "Analysis between groups"
  }
  if (is.null(lambda_between) & !is.null(lambda_within)) {
    case <- "Analysis within cases"
  }
  if (!is.null(lambda_between) & !is.null(lambda_within)) {
    case <- "Mixed-Analysis: between and within factors"
  }
  if (any(is.null(between), is.null(lambda_between),
          is.null(between_levels)) &
      case == "Analysis between groups") {
    stop("Missing arguments")
  }
  if (any(is.null(within), is.null(lambda_within),
          is.null(within_levels), is.null(ID)) &
    case == "Analysis within cases") {
    stop("Missing arguments")
  }
  if (any(
    is.null(between), is.null(lambda_between),
    is.null(between_levels),
    is.null(within), is.null(lambda_within),
          is.null(within_levels), is.null(ID)) &
      case == "Mixed-Analysis: between and within factors") {
    stop("Missing arguments")
  }
  if (case == "Analysis between groups"){
    lambda_between <- (lambda_between[order(between_levels)])
    ni <- table(between)
    N <- sum(table(variable))
    k <- length(ni)
    df_inn <- N - k
    df_contrast <- 1
    lambda_between_row <- rep(NA, sum(ni))
    for (i in 1:length(between)){
      lambda_between_row <- replace(
        x = lambda_between_row,
        list = which(between == between_levels[i]),
        lambda_between[i]
        )
    }
    var_within <- tapply(X = variable, INDEX = between,
                         FUN = var)
    MS_within <- mean(var_within, na.rm = T)
    Mi <- tapply(X = variable, INDEX = between, FUN = mean)
    SEi <- tapply(X = variable, INDEX = between, FUN = sd) / sqrt(ni)
    L <- sum(Mi * lambda_between)
    SS_total <- sum(
      (variable - mean(variable)) ** 2)
    SS_between <- sum(ni * (Mi - mean(Mi) ** 2))
    F_contrast <- ( (L ** 2) / (MS_within)) *
      (1 / sum( (lambda_between ** 2) / ni))
    p_contrast <- pf(F_contrast, 1, df_inn, lower.tail = F)
    r_effectsize <- cor(lambda_between_row, variable)
    r_alerting <- cor(lambda_between, Mi)
    r_contrast <- (r_effectsize * r_alerting) /
      (sqrt(
        r_effectsize ** 2 * r_alerting ** 2 - r_effectsize ** 2
        + r_alerting ** 2))
    sig <- c(F_contrast, p_contrast, df_contrast, df_inn, MS_within,
             SS_between, SS_total, N - 1)
    r <- c(r_effectsize, r_contrast, r_alerting)
    names(lambda_between) <- between_levels
    desc <- matrix(c(Mi, SEi), ncol = 2, byrow = F)
    colnames(desc) <- c("M", "SE")
    out_l <- list(sig, desc, lambda_between, r)
    names(out_l) <- c("sig", "desc", "lambda_between", "effects")
    class(out_l) <- c("cofad_bw")
    structure(out_l)
    return(out_l)
  }
  if (case == "Analysis within cases"){
    lambda_within <- lambda_within[order(within_levels)]
    ni_within <- table(within)
    N <- sum(ni_within)
    k_within <- 1
    df_contrast <- 1
    df_id <- N - 1
    names(lambda_within) <- within_levels
    L <- tapply(variable, ID, FUN = function(x) sum(x * lambda_within))
    S2 <- var(L)
    harm_mean <- k_within / sum(1 / ni_within)
    t_value <- mean(L) / sqrt( (1 / (k_group * harm_mean)) * S2)
    F_contrast <- t_value ** 2
    SS_total <- sum( ( (variable - mean(variable)) ** 2))
    p_contrast <- pt(t_value, df_id, lower.tail = F)
    g_effect <- mean(L) / (sqrt(S2))
    r_contrast <- sqrt(F_contrast / (F_contrast + df_within))
    sig <- c(t_value, p_contrast, df_id)
    desc <- c(mean(L), sd(L) / sqrt(sum(table(L))), sd(L))
    r <- c(r_contrast, g_effect)
    out_l <- list(sig, desc, lambda_within, r)
    names(out_l) <- c("sig", "desc", "lambda_between", "effects")
    class(out_l) <- c("cofad_wi")
    structure(out_l)
    return(out_l)
  }
  if (case == "Mixed-Analysis: between and within factors"){
    lambda_within <- sort(lambda_within[order(within_levels)])
    ni_within <- table(within)
    ni_between <- table(between)
    N <- sum(ni_within)
    k_within <- length(ni_within)
    k_between <- length(ni_between)
    df_contrast <- 1
    L <- tapply(variable, ID,
                FUN = function(x) sum(x * lambda_within))
    df_id <- length(L) - 1
    L_group <- tapply(variable, list(ID, between),
                      FUN = function(x) sum(x * lambda_within))
    L_group_mean <- apply(L_group, 2, mean, na.rm = T)
    L_group_var <- apply(L_group, 2, var, na.rm = T)
    L_group_n <- apply(L_group, 2, function(x) sum(table(x)))
    MS_contrast <- k_between *
      sum(L_group_mean * lambda_between) ** 2 /
      sum(lambda_between ** 2)
    S2_pooled <- sum(
      ( (L_group_n - 1) * L_group_var)
      ) /
      sum(L_group_n - 1)
    F_contrast <- MS_contrast / S2_pooled
    df_within <- length(L) - k_between
    p_contrast <- pf(F_contrast, df1 = 1, df2 = df_within,
                     lower.tail = F)
    r_contrast <- sqrt(F_contrast / (F_contrast + df_within))
    r_alerting <- cor(L_group_mean, lambda_between)
    lambda_between_row <- rep(NA, length(L))
      for (i in 1:ncol(L_group)) {
            lambda_between_row[which(!is.na(L_group[, i]))] <-
              lambda_between[i]
      }
    r_effectsize <- cor(L, lambda_between_row)
    sig <- c(F_contrast, p_contrast, df_contrast, df_within,
             MS_contrast, S2_pooled)
    r <- c(r_effectsize, r_contrast, r_alerting)
    names(lambda_between) <- between_levels
    names(lambda_within) <- within_levels
    desc_wi <- L_group
    desc <- matrix(c(L_group_mean, sqrt(L_group_var / L_group_n)),
                   ncol = 2, byrow = F)
    colnames(desc) <- c("M", "SE")
    out_l <- list(sig, desc, lambda_between, lambda_within, r, desc_wi)
    names(out_l) <- c("sig", "desc", "lambda_between",
                      "lambda_within", "effects", "desc_wi")
    class(out_l) <- c("cofad_mx")
    structure(out_l)
    return(out_l)
  }
}
