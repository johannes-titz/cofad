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

prepare_table <- function(lambda, var) {
  df <- data.frame("level" = names(lambda), lambda = lambda)
  freq_between <- as.data.frame(table(var))
  freq_between <- freq_between %>%
    dplyr::transmute(level = as.character(var), n = Freq)
  df$level <- as.character(df$level)
  df <- dplyr::left_join(df, freq_between, by = "level")
  df
}
