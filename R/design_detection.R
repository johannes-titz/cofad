#' Suggest roles for a contrast-analysis data set
#'
#' Uses the replication and nesting structure of a long-format data frame to
#' suggest a dependent variable, participant identifier, and within- and
#' between-subjects factors. Suggestions are intended as editable defaults;
#' they cannot recover design intent that is not represented in the data.
#'
#' @param data A data frame in long format.
#' @return A list containing the suggested design and variable names, a
#'   confidence score, and role diagnostics.
#' @export
detect_design <- function(data) {
  if (!is.data.frame(data) || nrow(data) < 2 || ncol(data) < 2) {
    stop("data must be a data frame with at least two rows and two columns")
  }

  n <- nrow(data)
  unique_n <- vapply(data, function(x) length(unique(x[!is.na(x)])), integer(1))
  discrete <- vapply(data, is_design_discrete, logical(1), n = n)
  names(discrete) <- names(data)

  id_diagnostics <- do.call(rbind, lapply(names(data), function(variable) {
    score_id_candidate(data, variable, discrete)
  }))
  id_diagnostics <- id_diagnostics[order(-id_diagnostics$score), , drop = FALSE]
  id_name <- ""
  if (nrow(id_diagnostics) && id_diagnostics$score[1] >= 0.62 &&
      id_diagnostics$crossing[1] >= 0.75) {
    id_name <- id_diagnostics$variable[1]
  }

  factor_diagnostics <- do.call(rbind, lapply(names(data), function(variable) {
    score_factor_candidate(data, variable, id_name, discrete)
  }))
  factor_diagnostics <- factor_diagnostics[
    order(-factor_diagnostics$within_score, -factor_diagnostics$between_score),
    , drop = FALSE
  ]

  within_name <- ""
  if (nzchar(id_name)) {
    within_rows <- factor_diagnostics$within_score >= 0.70
    if (any(within_rows)) {
      within_name <- factor_diagnostics$variable[which(within_rows)[1]]
    }
  }

  between_name <- ""
  between_rows <- factor_diagnostics$between_score >= 0.55 &
    factor_diagnostics$variable != within_name &
    factor_diagnostics$variable != id_name
  if (any(between_rows)) {
    between_name <- factor_diagnostics$variable[which(between_rows)[1]]
  }

  dv_diagnostics <- do.call(rbind, lapply(names(data), function(variable) {
    score_dv_candidate(
      data, variable, excluded = c(id_name, within_name, between_name)
    )
  }))
  dv_diagnostics <- dv_diagnostics[order(-dv_diagnostics$score), , drop = FALSE]
  dv_name <- if (nrow(dv_diagnostics) && is.finite(dv_diagnostics$score[1])) {
    dv_diagnostics$variable[1]
  } else {
    ""
  }

  design <- if (nzchar(within_name) && nzchar(between_name)) {
    "mixed"
  } else if (nzchar(within_name)) {
    "within"
  } else if (nzchar(between_name)) {
    "between"
  } else {
    "undetermined"
  }
  selected_scores <- c(
    if (nzchar(dv_name)) dv_diagnostics$score[1] else 0,
    if (nzchar(id_name)) id_diagnostics$score[1] else NULL,
    if (nzchar(within_name)) {
      factor_diagnostics$within_score[
        match(within_name, factor_diagnostics$variable)
      ]
    } else NULL,
    if (nzchar(between_name)) {
      factor_diagnostics$between_score[
        match(between_name, factor_diagnostics$variable)
      ]
    } else NULL
  )

  structure(
    list(
      design = design,
      dv_name = dv_name,
      between_name = between_name,
      within_name = within_name,
      id_name = id_name,
      confidence = if (length(selected_scores)) {
        max(0, min(1, mean(pmin(selected_scores, 1))))
      } else 0,
      diagnostics = list(
        id = id_diagnostics,
        factors = factor_diagnostics,
        dv = dv_diagnostics
      )
    ),
    class = "cofad_design_suggestion"
  )
}

is_design_discrete <- function(x, n) {
  values <- x[!is.na(x)]
  k <- length(unique(values))
  is.factor(x) || is.character(x) || is.logical(x) ||
    (is.numeric(x) && all(values == floor(values)) &&
       k <= max(12, floor(sqrt(n))))
}

name_score <- function(variable, pattern) {
  as.numeric(grepl(pattern, variable, ignore.case = TRUE))
}

score_id_candidate <- function(data, variable, discrete) {
  x <- data[[variable]]
  values <- x[!is.na(x)]
  id_like <- is.factor(x) || is.character(x) || is.logical(x) ||
    (is.numeric(x) && all(values == floor(values)))
  valid <- !is.na(x)
  sizes <- table(x[valid])
  groups <- length(sizes)
  repeated_rows <- if (sum(valid)) {
    sum(sizes[sizes > 1]) / sum(valid)
  } else 0
  regularity <- if (length(sizes) > 1 && mean(sizes) > 0) {
    1 / (1 + stats::sd(as.numeric(sizes)) / mean(sizes))
  } else 0
  possible_within <- names(data)[
    discrete & names(data) != variable &
      vapply(data, function(y) {
        k <- length(unique(y[!is.na(y)]))
        k >= 2 && k <= 12 && k < groups
      }, logical(1))
  ]
  crossing <- if (groups >= 2 && length(possible_within)) {
    max(vapply(possible_within, function(candidate) {
      crossing_score(x, data[[candidate]])
    }, numeric(1)))
  } else 0
  cardinality <- if (groups > 1 && groups < nrow(data)) {
    log1p(groups) / log1p(nrow(data))
  } else 0
  score <- if (id_like && groups >= 2 && groups < nrow(data)) {
    0.42 * crossing + 0.20 * repeated_rows + 0.13 * regularity +
      0.15 * cardinality + 0.10 * name_score(
        variable, "(^id$|id$|subject|participant|person|case|proband)"
      )
  } else 0
  data.frame(
    variable = variable, groups = groups, repeated_rows = repeated_rows,
    regularity = regularity, crossing = crossing, score = score,
    stringsAsFactors = FALSE
  )
}

crossing_score <- function(id, candidate) {
  keep <- !is.na(id) & !is.na(candidate)
  if (!any(keep)) return(0)
  id <- droplevels(as.factor(id[keep]))
  candidate <- droplevels(as.factor(candidate[keep]))
  if (nlevels(id) < 2 || nlevels(candidate) < 2) return(0)
  cells <- table(id, candidate)
  complete <- mean(rowSums(cells > 0) == ncol(cells))
  unique_cells <- mean(cells <= 1)
  balanced <- 1 / (1 + stats::sd(rowSums(cells)) / mean(rowSums(cells)))
  0.55 * complete + 0.30 * unique_cells + 0.15 * balanced
}

score_factor_candidate <- function(data, variable, id_name, discrete) {
  x <- data[[variable]]
  k <- length(unique(x[!is.na(x)]))
  type_score <- if (is.factor(x) || is.character(x) || is.logical(x)) 1 else 0.55
  factor_name <- name_score(
    variable, "condition|group|between|within|time|occasion|treatment|music|med|pt|bw"
  )
  plausible <- discrete[[variable]] && k >= 2 && k <= 12 && variable != id_name
  within_score <- 0
  between_score <- 0
  if (plausible && nzchar(id_name)) {
    cross <- crossing_score(data[[id_name]], x)
    groups <- split(x, data[[id_name]], drop = TRUE)
    constant <- mean(vapply(groups, function(z) {
      length(unique(z[!is.na(z)])) <= 1
    }, logical(1)))
    within_score <- 0.75 * cross + 0.15 * type_score + 0.10 * factor_name
    between_score <- 0.70 * constant + 0.20 * type_score + 0.10 * factor_name
    if (cross >= 0.70) between_score <- between_score * 0.25
  } else if (plausible) {
    replication <- mean(table(x[!is.na(x)]) >= 2)
    between_score <- 0.50 * replication + 0.30 * type_score +
      0.20 * factor_name
  }
  data.frame(
    variable = variable, levels = k, within_score = within_score,
    between_score = between_score, stringsAsFactors = FALSE
  )
}

score_dv_candidate <- function(data, variable, excluded) {
  x <- data[[variable]]
  if (!is.numeric(x) || variable %in% excluded) {
    return(data.frame(variable = variable, score = -Inf,
                      stringsAsFactors = FALSE))
  }
  k <- length(unique(x[!is.na(x)]))
  variation <- if (nrow(data) > 1) log1p(k) / log1p(nrow(data)) else 0
  outcome_name <- name_score(
    variable,
    "(^dv$|outcome|response|score|test|value|empathy|recall|context|lsg|ability)"
  )
  id_penalty <- name_score(
    variable, "(^id$|id$|subject|participant|person|case|lambda|weight)"
  )
  data.frame(
    variable = variable,
    score = 0.65 * variation + 0.45 * outcome_name - 0.80 * id_penalty,
    stringsAsFactors = FALSE
  )
}
