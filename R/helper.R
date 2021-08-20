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

cite <- function() {
  '<p>References for method: </p>
  <div class="csl-bib-body" style="line-height: 1.5; margin-left: 2em; text-indent:-2em;">
  <div class="csl-entry">Furr, R. M. (2004). Interpreting effect sizes in contrast analysis. <i>Understanding Statistics</i>, <i>3</i>, 1–25. <a href="https://doi.org/10.1207/s15328031us0301_1">https://doi.org/10.1207/s15328031us0301_1</a></div>
  <span class="Z3988" title="url_ver=Z39.88-2004&amp;ctx_ver=Z39.88-2004&amp;rfr_id=info%3Asid%2Fzotero.org%3A2&amp;rft_id=info%3Adoi%2F10.1207%2Fs15328031us0301_1&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Ajournal&amp;rft.genre=article&amp;rft.atitle=Interpreting%20effect%20sizes%20in%20contrast%20analysis&amp;rft.jtitle=Understanding%20Statistics&amp;rft.volume=3&amp;rft.aufirst=R.%20Michael&amp;rft.aulast=Furr&amp;rft.au=R.%20Michael%20Furr&amp;rft.date=2004&amp;rft.pages=1-25&amp;rft.spage=1&amp;rft.epage=25"></span>
  <div class="csl-entry">Rosenthal, R., Rosnow, R. L., &amp; Rubin, D. B. (1999). <i>Contrasts and effect sizes in behavioral research: A correlational approach</i>. Cambridge University Press. <a href="https://doi.org/10.1017/CBO9780511804403">https://doi.org/10.1017/CBO9780511804403</a></div>
  <span class="Z3988" title="url_ver=Z39.88-2004&amp;ctx_ver=Z39.88-2004&amp;rfr_id=info%3Asid%2Fzotero.org%3A2&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Abook&amp;rft.genre=book&amp;rft.btitle=Contrasts%20and%20effect%20sizes%20in%20behavioral%20research%3A%20A%20correlational%20approach&amp;rft.publisher=Cambridge%20University%20Press&amp;rft.aufirst=Robert&amp;rft.aulast=Rosenthal&amp;rft.au=Robert%20Rosenthal&amp;rft.au=Ralph%20L%20Rosnow&amp;rft.au=Donald%20B%20Rubin&amp;rft.date=1999"></span>
</div>'
}
