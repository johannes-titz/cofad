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
#' @param ID identifier for cases or subjects is needed
#' for within- and mixed contrastanalysis.
#' @param data optional argument for the \code{data.frame}
#' containing \code{dv} and groups.
#' @details For multi-factorial designs, the lambda weights of
#' the factors must be connected.
#' @return Calculates the significance of the contrast analysis.
#  The contrastweights, the corresponding group and an effectsize are
#' given.
#' @references Rosenthal, R., Rosnow, R.L., & Rubin, D.B. (2000).
#' Contrasts and effect sizes in behavioral research:
#' A correlational approach. New York: Cambridge University Press.
#' @examples
#' # Example for between-subjects design Table 3.1 from
#' # Rosenthal, Rosnow and Rubin (2001)
#'
#' tab31 <- data.frame(
#'   Val = c(2, 6, 8, 4,10, 6, 8, 10, 4, 12, 8,
#'     16, 10, 14, 12, 12,  18, 14, 20, 16),
#'   Let = rep(c("A", "B", "C", "D"), c(5, 5, 5, 5))
#'   )
#' contr_bw <- calc_contrast(
#'    dv = Val,
#'    between = Let,
#'    lambda_between = c("A" = -3, "B" = -1, "C" = 1, "D" = 3),
#'    data = tab31)
#' contr_bw
#' summary(contr_bw)
#'
#' # Example for within-subjects design Calculation 16.6 from
#' # Sedlmeier and Renkewitz (2018, p. 537)
#'
#' sedlmeier537 <- data.frame(
#'    Var = c(27, 25, 30, 29, 30, 33, 31, 35,
#'            25, 26, 32, 29, 28, 30, 32, 34,
#'            21, 25, 23, 26, 27, 26, 29, 31,
#'            23, 24, 24, 28, 24, 26, 27, 32),
#'    within = as.factor(rep(1:4,c(8,8,8,8))),
#'    ID = as.factor(rep(1:8,4)))
#' contr_wi <- calc_contrast(
#'    dv = Var,
#'    within = within,
#'    ID = ID,
#'    lambda_within = c("1" = 0.25, "2" = -.75, "3" = 1.25, "4" = -.75),
#'    data=sedlmeier537
#'  )
#' contr_wi
#' summary(contr_wi, ci=.90)
#'
#' # Exampel for mixed-designs Table 5.3 from
#' # Rosenthal, Rosnow and Rubin (2001)
#' tab53 <- data.frame(
#'    Var = c(3, 1, 4, 4, 5, 5, 6, 5, 7, 2, 2, 5,
#'            5, 6, 7, 6, 6, 8, 3, 1, 5, 4, 5, 6,
#'            7, 6, 8, 3, 2, 5, 6, 6, 7, 8, 8, 9),
#'            bw = as.factor(rep(rep(LETTERS[1:3], c(3, 3, 3)), 4)),
#'            wi = as.factor(rep(1:4, c(9, 9, 9, 9))),
#'            ID = as.factor(rep(1:9, 4 ))
#'    )
#'    lambda_within <- c("1" = -3, "2" = -1, "3" = 1, "4" = 3)
#'    lambda_between <-c("A" = -1, "B" = 0, "C" = 1)
#'
#' contr_mx <- calc_contrast(dv = Var, between = bw,
#'               lambda_between = lambda_between,
#'               within = wi,
#'                lambda_within = lambda_within,
#'               ID = ID, data = tab53
#'               )
#' contr_mx
#' summary(contr_mx)
calc_contrast <- function(dv,
                          between = NULL,
                          lambda_between = NULL,
                          within = NULL,
                          lambda_within = NULL, ID = NULL,
                          data = NULL){
  if (!is.null(data) & is.data.frame(data)) {
    arguments <- as.list(match.call())
    dv <- eval(arguments$dv, data)
    between <- eval(arguments$between, data)
    within <- eval(arguments$within, data)
    ID <- eval(arguments$ID, data)
  } else if (!is.null(data) & !is.data.frame(data)){
    stop("data is not a data.frame")
  }
  if (is.null(lambda_between) & is.null(lambda_within)) {
    stop("lambda is missing")
  }
  if (!is.numeric(dv)){
    stop("variable must be numeric")
  }
  if (!is.factor(between) & !is.null(between)){
    stop("between must be a factor")
  }
  if (!is.null(lambda_between)){
    if (!is.numeric(lambda_between)){
      stop("lambda must be a named numeric")
    }
    if (is.null(names(lambda_between))){
      stop("lambda must be a named numeric")
    }
    if (anyNA(lambda_between)){
      stop("NA in lambda is not allowed")
    }
  }
  if (!is.null(lambda_within)){
    if (!is.numeric(lambda_within)){
      stop("lambda must be a named numeric")
    }
    if (is.null(names(lambda_within))){
      stop("lambda must be a named numeric")
    }
    if (anyNA(lambda_within)){
      stop("NA in lambda is not allowed")
    }
  }
  if (!is.factor(within) & !is.null(within)){
    stop("within must be a factor")
  }

    if (!is.null(between) & !is.null(lambda_between)) {
      if (anyNA(match(levels(between),names(lambda_between)))) {
        stop("lambda names doesn't match all between labels")
      }
    }
  if (!is.null(within) & !is.null(lambda_within)){
    if (anyNA(match(levels(within),names(lambda_within)))) {
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
    case <- "Mixed-Analysis: between and within factors"
  }
  if (any(is.null(between), is.null(lambda_between)) &
      case == "Analysis between groups") {
    stop("Missing arguments")
  }
  if (any(is.null(within), is.null(lambda_within),
          is.null(ID)) & case == "Analysis within cases") {
    stop("Missing arguments")
  }
  if (any(
    is.null(between), is.null(lambda_between),
    is.null(within), is.null(lambda_within),
    is.null(ID)) &
    case == "Mixed-Analysis: between and within factors") {
    stop("Missing arguments")
  }
  if (sum(lambda_between) != 0) {
    lambda_between <- lambda_between - mean(lambda_between)
    warning("lambdas are centered and rounded to 3 digits")
  }
  if (sum(lambda_within) != 0) {
    lambda_within <- lambda_within - mean(lambda_within)
    warning("lambdas are centered and rounded to 3 digits")
  }
  if (anyNA(dv)) {
    indexNA <- which(is.na(dv))
    dv <- dv[-indexNA]
    between <- between[-indexNA]
    within <- within[-indexNA]
    ID <- ID[-indexNA]
    warning("NAs in dependent variable are omitted")
  }
  if (anyNA(between)) {
    indexNA <- which(is.na(between))
    dv <- dv[-indexNA]
    between <- between[-indexNA]
    within <- within[-indexNA]
    ID <- ID[-indexNA]
    warning("NAs in between are omitted")
  }
  if (anyNA(within)) {
    indexNA <- which(is.na(within))
    dv <- dv[-indexNA]
    between <- between[-indexNA]
    within <- within[-indexNA]
    ID <- ID[-indexNA]
    warning("NAs in within are omitted")

  }
  if (anyNA(ID)) {
    indexNA <- which(is.na(ID))
    dv <- dv[-indexNA]
    between <- between[-indexNA]
    within <- within[-indexNA]
    ID <- ID[-indexNA]
    warning("NAs in ID are omitted")
  }
  if (case == "Analysis between groups"){
    lambda_between <- lambda_between[order(names(lambda_between))]
    ni <- table(between)
    N <- sum(table(dv))
    k <- length(ni)
    df_inn <- N - k
    df_contrast <- 1
    lambda_between_row <- rep(NA, sum(ni))
    for (i in 1:length(names(lambda_between))) {
      lambda_between_row <- replace(
        x = lambda_between_row,
        list = which(between == names(lambda_between)[i]),
        lambda_between[i]
      )
    }
    var_within <- tapply(X = dv, INDEX = between,
                         FUN = var)
    MS_within <- mean(var_within, na.rm = T)
    Mi <- tapply(X = dv, INDEX = between, FUN = mean)
    SEi <- tapply(X = dv, INDEX = between, FUN = sd) / sqrt(ni)
    L <- sum(Mi * lambda_between)
    SS_total <- sum(
      (dv - mean(dv)) ** 2)
    SS_between <- sum(ni * (Mi - mean(Mi) ** 2))
    F_contrast <- ( (L ** 2) / (MS_within)) *
      (1 / sum( (lambda_between ** 2) / ni))
    p_contrast <- pf(F_contrast, 1, df_inn, lower.tail = F)
    r_effectsize <- cor(lambda_between_row, dv)
    if (sd(Mi)==0) {
      r_alerting <- NA
      r_contrast <- NA
      warning("SD of groupmeans is zero")
    } else {
      r_alerting <- cor(lambda_between, Mi)
      r_contrast <- (r_effectsize * r_alerting) /
      (sqrt(
        r_effectsize ** 2 * r_alerting ** 2 - r_effectsize ** 2
        + r_alerting ** 2))
    }
    sig <- c(F_contrast, p_contrast, df_contrast, df_inn, MS_within,
             SS_between, SS_total, N - 1)
    r <- c(r_effectsize, r_contrast, r_alerting)
    desc <- matrix(c(Mi, SEi), ncol = 2, byrow = F)
    colnames(desc) <- c("M", "SE")
    out_l <- list(sig, desc, lambda_between, r)
    names(out_l) <- c("sig", "desc", "lambda_between", "effects")
    class(out_l) <- c("cofad_bw")
    structure(out_l)
    return(out_l)
  }
  if (case == "Analysis within cases"){
    lambda_within <- lambda_within[order(names(lambda_within))]
    ni_within <- table(within)
    N <- sum(ni_within)
    L <- NULL
    for (i in 1:length(table(ID))){
     var_i <- dv[which(ID==levels(ID)[i])]
    L[i] <- sum(var_i[order(within[which(ID==levels(ID)[i])])] * lambda_within)
    }
    if (!is.null(between)){
      ni_bw <- table(between)
      bw_wide <- matrix(NA, ncol = 2, nrow = length(levels(ID)))
      for(i in 1:length(levels(ID))){
        id_bw <- as.character(unique(
          between[which(ID==levels(ID)[i])]
        ))
        if (length(id_bw) > 1) {
          stop("some ID's are in more than one between group")
        } else {
          bw_wide[i, 1] <- levels(ID)[i]
          bw_wide[i, 2] <- id_bw
        }
      }
     ni_L <- table(bw_wide[,2])
     S2i <- tapply(L,as.factor(bw_wide[,2]), var)
     if (anyNA(S2i)) {
       S2i[which(is.na(S2i))] <- 0
     }
     S2 <- sum((ni_L-1) * S2i) / sum(ni_L-1)
     k_bw <- length(ni_bw)
     df_within <- N - k_bw
    } else {
      S2 <- var(L)
      k_bw <- 1
      df_id <- length(L)-1
      df_within <- N - k_bw
    }
    df_contrast <- 1
    if (!is.null(between)){
      df_id <- sum(ni_L) - k_bw
      ni_cell <- table(within, between)
    } else {
      ni_cell <- ni_within
    }
    harm_n <- 1 / mean(1/ni_cell)
    if (!is.null(between)){
      L <- tapply(L, bw_wide[,2] ,mean)
    }
    t_value <- mean(L) / sqrt( (1 / (k_bw * harm_n)) * S2)
    F_contrast <- t_value ** 2
    SS_total <- sum( ( (dv - mean(dv)) ** 2))
    p_contrast <- pt(t_value, df_id, lower.tail = F)
    g_effect <- mean(L) / (sqrt(S2))
    r_contrast <- sqrt(F_contrast / (F_contrast + df_within))
    sig <- c(t_value, p_contrast, df_id)
    desc <- c(mean(L), sqrt(S2) / sqrt(sum(table(L))), sqrt(S2))
    r <- c(r_contrast, g_effect)
    out_l <- list(sig, desc, lambda_within, r)
    names(out_l) <- c("sig", "desc", "lambda_between", "effects")
    class(out_l) <- c("cofad_wi")
    structure(out_l)
    return(out_l)
  }
  if (case == "Mixed-Analysis: between and within factors"){
    lambda_within <- lambda_within[order(names(lambda_within))]
    lambda_between <- lambda_between[order(names(lambda_between))]
    ni_within <- table(within)
    ni_between <- table(between)
    N <- sum(ni_within)
    k_within <- length(ni_within)
    k_between <- length(ni_between)
    df_contrast <- 1
    L <- NULL
    for (i in 1:length(table(ID))){
      var_i <- dv[which(ID==levels(ID)[i])]
      L[i] <- sum(var_i[order(within[which(ID==levels(ID)[i])])] * lambda_within)
    }
    bw_wide <- matrix(NA, ncol = 2, nrow = length(levels(ID)))
    for(i in 1:length(levels(ID))){
      id_bw <- as.character(unique(
        between[which(ID==levels(ID)[i])]
      ))
      if (length(id_bw) > 1) {
        stop("some ID's are in more than one between group")
      } else {
        bw_wide[i, 1] <- levels(ID)[i]
        bw_wide[i, 2] <- id_bw
        }
    }
    df_id <- length(L) - 1
    L_group_mean <- tapply(L, bw_wide[,2], mean)
    L_group_var <- tapply(L, bw_wide[,2], var)
    L_group_n <- tapply(L,  bw_wide[,2],
                        function(x) sum(table(x)))
    ni_cell <- table(within, between)
    harm_n <- 1 / mean(1/ni_cell)
    MS_contrast <- harm_n *
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
    for (i in 1:length(lambda_between)) {
    lambda_between_row  <- replace(lambda_between_row,
               which(bw_wide[,2]==names(lambda_between)[i]),
               lambda_between[i])
    }
    r_effectsize <- cor(L, lambda_between_row)
    sig <- c(F_contrast, p_contrast, df_contrast, df_within,
             MS_contrast, S2_pooled)
    r <- c(r_effectsize, r_contrast, r_alerting)
    desc_wi <- L
    desc <- matrix(c(L_group_mean, sqrt(L_group_var / L_group_n)),
                   ncol = 2, byrow = F)
    rownames(desc) <- names(L_group_mean)
    colnames(desc) <- c("M", "SE")
    out_l <- list(sig, desc, lambda_between, lambda_within, r, desc_wi)
    names(out_l) <- c("sig", "desc", "lambda_between",
                      "lambda_within", "effects", "desc_wi")
    class(out_l) <- c("cofad_mx")
    structure(out_l)
    return(out_l)
  }
}

