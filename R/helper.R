#' Load the data
#'
#' Depending on file ending, the data is loaded.
#'
#' @importFrom Hmisc spss.get
#' @importFrom utils read.csv read.csv2 count.fields
#' @importFrom readr guess_encoding
#' @importFrom stringr str_match
#' @param datafile yep, the data file to upload
#' @return data as an R object or an error
#' @noRd
load_data <- function(datafile) {
  fileending <- stringr::str_match(datafile$datapath, "(\\..+$)")[1, 1]
  data <- tryCatch({
    if (fileending == ".sav") {
      data <- Hmisc::spss.get(datafile$datapath, use.value.labels = F)
    } else if (fileending == ".csv") {
      encoding <- unlist(readr::guess_encoding(datafile$datapath)[1, 1])
      lines <- readLines(datafile$datapath, n = 1)
      numfields_semicolon <- count.fields(textConnection(lines), sep = ";")
      numfields_colon <- count.fields(textConnection(lines), sep = ",")
      if (numfields_semicolon == 1) {
        data <- utils::read.csv(datafile$datapath, fileEncoding = encoding)
      } else if (numfields_colon == 1) {
        data <- utils::read.csv2(datafile$datapath, fileEncoding = encoding)
      }
    }
    data},
    error = function(error_message) {
      msg <- paste(
        "Sorry, I could not read your data. Please check that it is in the ",
        "SPSS format .sav or a regular .csv file with a comma as a separator ",
        "(not a semicolon or any other delimiter).",
        sep = ""
      )
      shinyalert::shinyalert("Error", msg)
      message(error_message)
    }
  )
}

#' Make column names to html tags
#'
#' This is a sortable helper that covert column names of a data frame to
#' proper html tags for use with sortable_js
#'
#' @importFrom utils tail
#' @param df the data frame to convert
#' @return html object with column names of the df
#' @noRd
colnames_to_tags <- function(df) {
  lapply(
    colnames(df),
    function(co) {
      tag(
        "p",
        list(
          class = utils::tail(class(df[, co]), 1),
          tags$span(class = "glyphicon glyphicon-move"),
          tags$strong(co)
        )
      )
    }
  )
}

#' Creates default (linear) lambdas
#'
#' This is used in shiny input tables to have some default.
#'
#' @param levels levels to create default lambdas for
#' @return vector of lambdas
#' @noRd
create_default_lambdas <- function(levels) {
  lambdas <- 1:length(levels)
  lambdas <- lambdas - mean(lambdas)
  names(lambdas) <- levels
  lambdas
}

#' Creates rhandsontable for lambda input
#'
#' Used in shiny to create a UI for setting lambda values.
#'
#' @param levels levels to create default lambdas for
#' @return rhandsontable with lambda levels and default (linear) lambda values
#' @noRd
create_table <- function(levels) {
  df <- data.frame(
    levels = levels,
    lambda = create_default_lambdas(levels)
  )
  the_tab <- rhandsontable::rhandsontable(
    df,
    stretchH = "all",
    rowHeaders = NULL
  )
  # make first column read only
  rhandsontable::hot_col(the_tab, "levels", readOnly = T)
}

#' prepares lambda table
#'
#' Used in shiny to pepare a UI for setting lambda values.
#'
#' @param levels levels to create default lambdas for
#' @return data frame with levels, lambda values and n per group
#' @noRd
#' @importFrom rlang .data
prepare_table <- function(lambda, var) {
  df <- data.frame("level" = names(lambda), lambda = lambda)
  freq_between <- as.data.frame(table(var))
  freq_between <- dplyr::transmute(freq_between,
                                   level = as.character(.data$var),
                                   n = .data$Freq)
  df$level <- as.character(df$level)
  df <- dplyr::left_join(df, freq_between, by = "level")
  df
}

#' Cites useful references for cofad in html
#'
#' Used in shiny to list some references for contrast analysis.
#'
#' @return HTML character
#' @noRd
cite <- function() {
  paste(readLines(system.file("extdata", "citation.txt", package = "cofad")),
        collapse = "")
}

#' Calculate lambdas for two competing hypotheses
#'
#' If you want to test two competing hypotheses, you can use this helper
#' function to create the correct difference lambdas. There is no magic here.
#' The two contrasts are z-standardized first and then subtracted
#' (lambda_preferred - lambda_competing). You can use the new difference lambdas
#' as the input for calc_contrast.
#'
#' @param lambda_preferred Lambdas of the preferred hypothesis, has to be a
#'   named vector with the names corresponding with the groups in the analyzed
#'   data set. Alternatively, use the parameter labels.
#' @param lambda_competing Lambdas of the competing hypothesis, has to be a
#'   named vector with the names corresponding with the groups in the analyzed
#'   data set. Alternatively, use the parameter labels.
#' @param labels If you provide lambdas without names, you can set the labels
#'   for both groups here.
#'
#' @return Lambdas for difference between lambda_preferred and lambda 2
#'
#' @examples
#' lambda_diff <- lambda_diff(c("A" = 1, "B" = 2, "C" = 3),
#'                            c("A" = 1, "B" = 2, "C" = 6))
#' # same result
#' lambda_diff2 <- lambda_diff(c(1, 2, 3), c(1, 2, 6),
#'                             labels = c("A", "B", "C"))
#' @export
lambda_diff <- function(lambda_preferred, lambda_competing, labels = NULL) {
  if (cor(lambda_preferred, lambda_competing) == 1) {
    stop('Your lambdas are perfectly correlated. ',
         'It does not make sense to compare them.')
  }
  if ((is.null(names(lambda_preferred)) | is.null(names(lambda_competing))) &
      is.null(labels)) {
    stop('Please provide group labels for your lambdas. ',
         'For instance, c("A" = 1, "B" = 2, ...)')
  }
  if ((!is.null(names(lambda_preferred)) | !is.null(names(lambda_competing))) &
      !is.null(labels)) {
    stop('Use either a named vector for the lambdas',
         ' or the labels parameter to specify the group labels. ',
         'Do not use both.')
  }
  if (!is.null(labels)) {
    names(lambda_preferred) <- names(lambda_competing) <- labels
  }
  lambda_preferred <- lambda_preferred[sort(names(lambda_preferred))]
  lambda_competing <- lambda_competing[sort(names(lambda_competing))]
  if (!(identical(names(lambda_preferred), names(lambda_competing)))) {
    stop('Please provide the same labels for your lambdas\n',
         'current labels of preferred lambdas: ',
         paste(names(lambda_preferred), collapse = " "),
         '\ncurrent labels of competing lambdas: ',
         paste(names(lambda_competing), collapse = " "))
  }
  lambda_diff <- as.numeric(zscale(lambda_preferred) - zscale(lambda_competing))
  names(lambda_diff) <- names(lambda_preferred)
  return(lambda_diff)
}

zscale <- function(x) {
  n <- length(x)
  sqrt(n / (n - 1)) * (x - mean(x)) / sd(x)
}
