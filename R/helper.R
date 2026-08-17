#' Load the data
#'
#' Depending on file ending, the data is loaded.
#'
#' @importFrom foreign read.spss
#' @importFrom utils read.csv read.csv2 count.fields
#' @param datafile yep, the data file to upload
#' @return data as an R object or an error
#' @noRd
load_data <- function(datafile) {
  fileending <- tolower(sub("^.*(\\.[^.]+)$", "\\1", basename(datafile$name)))
  data <- tryCatch(
    {
      if (identical(fileending, ".sav")) {
        data <- foreign::read.spss(
          datafile$datapath, to.data.frame = TRUE, use.value.labels = FALSE,
          trim.factor.names = TRUE
        )
      } else if (identical(fileending, ".csv")) {
        encoding <- detect_text_encoding(datafile$datapath)
        lines <- readLines(datafile$datapath, n = 1)
        numfields_semicolon <- count.fields(textConnection(lines), sep = ";")
        numfields_comma <- count.fields(textConnection(lines), sep = ",")
        if (numfields_semicolon > numfields_comma) {
          data <- utils::read.csv2(datafile$datapath, fileEncoding = encoding)
        } else {
          data <- utils::read.csv(datafile$datapath, fileEncoding = encoding)
        }
      } else {
        stop("Unsupported file extension: ", fileending)
      }
      data
    },
    error = function(error_message) {
      msg <- paste(
        "Sorry, I could not read your data. Please check that it is in the ",
        "SPSS .sav format or a regular .csv file with a comma or semicolon ",
        "as the separator.",
        sep = ""
      )
      session <- shiny::getDefaultReactiveDomain()
      if (!is.null(session) && is.function(session$sendNotification)) {
        showNotification(msg, type = "error")
      }
      message(conditionMessage(error_message))
      NULL
    }
  )
}

detect_text_encoding <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  bytes <- readBin(connection, what = "raw", n = 3)
  if (length(bytes) == 3 && identical(as.integer(bytes), c(239L, 187L, 191L))) {
    "UTF-8-BOM"
  } else {
    ""
  }
}

#' Make column names to html tags
#'
#' This is a sortable helper that converts column names of a data frame to
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
#' Used in Shiny to prepare a UI for setting lambda values.
#'
#' @param lambda named primary contrast weights
#' @param var factor used to count observations per level
#' @param lambda_rival optional named rival contrast weights
#' @return data frame with levels, lambda values and n per group
#' @noRd
prepare_table <- function(lambda, var, lambda_rival = NULL) {
  if (is.null(lambda_rival)) {
    df <- data.frame("level" = names(lambda), lambda = lambda)
  } else {
    lambda_rival <- lambda_rival[names(lambda)]
    df <- data.frame(
      "level" = names(lambda), favored = lambda, rival = lambda_rival
    )
  }
  df$level <- as.character(df$level)
  frequencies <- table(as.character(var), useNA = "no")
  df$n <- as.integer(frequencies[df$level])
  df
}

cofad_resource <- function(...) {
  resource_root <- getOption("cofad.resource_dir", "")
  if (nzchar(resource_root)) {
    file.path(resource_root, ...)
  } else {
    system.file("extdata", ..., package = "cofad")
  }
}

load_cofad_example <- function(name) {
  data_root <- getOption("cofad.data_dir", "")
  data_env <- new.env(parent = emptyenv())
  if (nzchar(data_root)) {
    path <- file.path(data_root, paste0(name, ".rda"))
    if (!file.exists(path)) return(NULL)
    load(path, envir = data_env)
  } else {
    suppressWarnings(utils::data(
      list = name, package = "cofad", envir = data_env
    ))
  }
  if (!exists(name, envir = data_env, inherits = FALSE)) return(NULL)
  get(name, envir = data_env, inherits = FALSE)
}

#' Citation formats used by the cofad app
#'
#' Keep these strings in sync with `inst/CITATION`. The app deliberately shows
#' the two package citations rather than the broader methodological reading
#' list.
#'
#' @return A single character string.
#' @noRd
cofad_citation_plain <- function() {
  paste(
    paste(
      "Henninger, M., Malejka, S., & Titz, J. (2025). Contrast analysis for",
      "competing hypotheses: A tutorial using the R package cofad.",
      "Behavior Research Methods, 57, Article 326.",
      "https://doi.org/10.3758/s13428-025-02833-w"
    ),
    paste(
      "Titz, J., & Burkhardt, M. (2021). cofad: An R package and Shiny app",
      "for contrast analysis. Journal of Open Source Software, 6(67), 3822.",
      "https://doi.org/10.21105/joss.03822"
    ),
    sep = "\n\n"
  )
}

cofad_citation_html <- function() {
  paste0(
    '<div class="csl-bib-body">',
    '<div class="csl-entry">Henninger, M., Malejka, S., &amp; Titz, J. ',
    '(2025). Contrast analysis for competing hypotheses: A tutorial using ',
    'the R package cofad. <i>Behavior Research Methods, 57</i>, Article 326. ',
    '<a href="https://doi.org/10.3758/s13428-025-02833-w">',
    'https://doi.org/10.3758/s13428-025-02833-w</a></div>',
    '<div class="csl-entry">Titz, J., &amp; Burkhardt, M. (2021). cofad: ',
    'An R package and Shiny app for contrast analysis. ',
    '<i>Journal of Open Source Software, 6</i>(67), 3822. ',
    '<a href="https://doi.org/10.21105/joss.03822">',
    'https://doi.org/10.21105/joss.03822</a></div>',
    '</div>'
  )
}

cofad_citation_bibtex <- function() {
  paste(
    paste0(
      "@article{henninger2025cofad,\n",
      "  author = {Henninger, Mirka and Malejka, Simone and Titz, Johannes},\n",
      "  title = {Contrast analysis for competing hypotheses: A tutorial ",
      "using the {R} package cofad},\n",
      "  journal = {Behavior Research Methods},\n",
      "  year = {2025},\n  volume = {57},\n  pages = {326},\n",
      "  doi = {10.3758/s13428-025-02833-w}\n}"
    ),
    paste0(
      "@article{titz2021cofad,\n",
      "  author = {Titz, Johannes and Burkhardt, Markus},\n",
      "  title = {cofad: An {R} package and {Shiny} app for contrast analysis},\n",
      "  journal = {Journal of Open Source Software},\n",
      "  year = {2021},\n  volume = {6},\n  number = {67},\n",
      "  pages = {3822},\n  doi = {10.21105/joss.03822}\n}"
    ),
    sep = "\n\n"
  )
}

# Backward-compatible internal helper.
cite <- cofad_citation_html

cofad_citation_panel <- function() {
  shiny::tagList(
    shiny::tags$h4("Cite cofad"),
    shiny::tags$div(
      class = "cofad-copy-layout cofad-citation-layout",
      shiny::tags$div(
        class = "cofad-copy-content",
        shiny::tags$p("Please cite both the tutorial and the software paper:"),
        shiny::HTML(cofad_citation_html())
      ),
      shiny::tags$div(
        class = "cofad-copy-actions cofad-citation-actions",
        shiny::actionButton(
          "copy_citation_plain", "Copy plain text",
          class = "cofad-copy-button",
          onclick = "cofadCopyCitation('plain'); return false;"
        ),
        shiny::actionButton(
          "copy_citation_html", "Copy HTML",
          class = "cofad-copy-button",
          onclick = "cofadCopyCitation('html'); return false;"
        ),
        shiny::actionButton(
          "copy_citation_bib", "Copy BibTeX",
          class = "cofad-copy-button",
          onclick = "cofadCopyCitation('bibtex'); return false;"
        ),
        shiny::tags$span(id = "cofad-copy-status", role = "status")
      )
    ),
    shiny::tags$textarea(
      id = "cofad-citation-plain", style = "display:none;",
      cofad_citation_plain()
    ),
    shiny::tags$textarea(
      id = "cofad-citation-html", style = "display:none;",
      cofad_citation_html()
    ),
    shiny::tags$textarea(
      id = "cofad-citation-bibtex", style = "display:none;",
      cofad_citation_bibtex()
    )
  )
}

#' Create a detailed variance-decomposition table
#'
#' The contrast accounts for one degree of freedom of the between-group sum of
#' squares. The remaining between-group variation is shown separately.
#'
#' @param object A `cofad_bw` or `cofad_mx` object.
#' @return A formatted data frame for display in the Shiny app.
#' @noRd
detailed_f_table <- function(object) {
  format_statistic <- function(x, digits = 3) {
    ifelse(
      is.na(x), "",
      formatC(x, format = "f", digits = digits, drop0trailing = FALSE)
    )
  }
  format_probability <- function(x) {
    vapply(x, function(value) {
      if (is.na(value)) "" else format_report_p(value)
    }, character(1))
  }
  s <- object$sig
  df_between <- unname(s["df_total"] - s["df_inn"])
  df_residual <- df_between - unname(s["df_contrast"])
  ss_residual <- unname(s["ss_between"] - s["ss_kontrast"])
  if (abs(ss_residual) < sqrt(.Machine$double.eps)) ss_residual <- 0

  ms_between <- unname(s["ss_between"]) / df_between
  ms_residual <- if (df_residual > 0) ss_residual / df_residual else NA_real_
  f_between <- ms_between / unname(s["ms_within"])
  f_residual <- if (df_residual > 0) {
    ms_residual / unname(s["ms_within"])
  } else {
    NA_real_
  }

  source_labels <- if (inherits(object, "cofad_mx")) {
    c(
      "Between groups in within-contrast scores (overall)",
      "Planned between \u00d7 within contrast",
      "Residual between groups in within-contrast scores",
      "Within-group variation in within-contrast scores (error)",
      "Total variation in within-contrast scores"
    )
  } else {
    c(
      "Between groups (overall)", "Contrast", "Residual between groups",
      "Within groups (error)", "Total"
    )
  }

  tab <- data.frame(
    Source = source_labels,
    SS = c(
      s["ss_between"], s["ss_kontrast"], ss_residual,
      s["ss_within"], s["ss_total"]
    ),
    df = c(
      df_between, s["df_contrast"], df_residual, s["df_inn"], s["df_total"]
    ),
    MS = c(
      ms_between, s["ss_kontrast"] / s["df_contrast"], ms_residual,
      s["ms_within"], NA_real_
    ),
    F = c(f_between, s["f_contrast"], f_residual, NA_real_, NA_real_),
    p = c(
      stats::pf(f_between, df_between, s["df_inn"], lower.tail = FALSE),
      s["p_contrast"],
      if (df_residual > 0) {
        stats::pf(f_residual, df_residual, s["df_inn"], lower.tail = FALSE)
      } else {
        NA_real_
      },
      NA_real_, NA_real_
    ),
    eta2 = c(
      s["ss_between"], s["ss_kontrast"], ss_residual,
      s["ss_within"], NA_real_
    ) / unname(s["ss_total"]),
    partial_eta2 = c(
      unname(s["ss_between"] / (s["ss_between"] + s["ss_within"])),
      unname(s["ss_kontrast"] / (s["ss_kontrast"] + s["ss_within"])),
      unname(ss_residual / (ss_residual + s["ss_within"])),
      NA_real_, NA_real_
    ),
    check.names = FALSE
  )

  tab$SS <- format_statistic(tab$SS)
  tab$df <- format_statistic(tab$df, digits = 0)
  tab$MS <- format_statistic(tab$MS)
  tab$F <- format_statistic(tab$F)
  tab$p <- format_probability(tab$p)
  tab$eta2 <- format_statistic(tab$eta2)
  tab$partial_eta2 <- format_statistic(tab$partial_eta2)
  rownames(tab) <- NULL
  attr(tab, "header_tooltips") <- c(
    eta2 = paste0(
      "Ordinary eta squared: the sum of squares in this row divided by ",
      "the total sum of squares."
    ),
    partial_eta2 = paste0(
      "Partial eta squared: effect SS divided by effect SS plus ",
      "within-group/error SS."
    )
  )
  attr(tab, "cell_tooltips") <- list(
    eta2 = c(
      "Between-groups eta squared = SS between / SS total.",
      paste0(
        "Contrast eta squared = SS contrast / SS total; this is ",
        "equivalent to r_es squared."
      ),
      paste0(
        "Residual between-groups eta squared = SS residual between / ",
        "SS total."
      ),
      paste0(
        "Within-group/error SS share = SS within / SS total. This is a ",
        "descriptive variance share, not a tested effect."
      ),
      ""
    ),
    partial_eta2 = c(
      paste0(
        "Between-groups partial eta squared = SS between / ",
        "(SS between + SS within/error)."
      ),
      paste0(
        "Contrast partial eta squared = SS contrast / ",
        "(SS contrast + SS within/error); this is equivalent to ",
        "r_contrast squared."
      ),
      paste0(
        "Residual between-groups partial eta squared = SS residual between / ",
        "(SS residual between + SS within/error)."
      ),
      "",
      ""
    )
  )
  tab
}

#' Calculate the contrast and error components of a within-subjects F test
#' @noRd
within_f_components <- function(object) {
  stopifnot(inherits(object, "cofad_wi"))
  df_error <- unname(object$sig[[3]])
  f_value <- unname(object$sig[[1]])^2
  score_variance <- unname(object$desc[[3]])^2
  score_scale <- if (identical(object$within_score, "L")) {
    sum(object$lambda_within^2)
  } else {
    1
  }
  ms_error <- score_variance / score_scale
  ms_contrast <- f_value * ms_error
  ss_error <- ms_error * df_error
  list(
    ss_contrast = ms_contrast,
    ss_error = ss_error,
    ss_total = ms_contrast + ss_error,
    ms_contrast = ms_contrast,
    ms_error = ms_error,
    df_error = df_error,
    f_value = f_value
  )
}

#' Create an F table for a within-subjects contrast test
#'
#' A planned within-subjects contrast is tested as a t test of participants'
#' contrast scores. The contrast mean square and its participant-by-contrast
#' error mean square reproduce the exactly equivalent nondirectional F test.
#'
#' @param object A `cofad_wi` object.
#' @return A formatted data frame for display in the Shiny app.
#' @noRd
detailed_within_f_table <- function(object) {
  components <- within_f_components(object)
  eta2 <- components$ss_contrast / components$ss_total
  format_statistic <- function(x, digits = 3) {
    ifelse(
      is.na(x), "",
      formatC(x, format = "f", digits = digits, drop0trailing = FALSE)
    )
  }
  tab <- data.frame(
    Source = c(
      "Within-subjects contrast",
      "Contrast \u00d7 participants (error)",
      "Total contrast-related variation"
    ),
    SS = format_statistic(c(
      components$ss_contrast, components$ss_error, components$ss_total
    )),
    df = format_statistic(c(
      1, components$df_error, components$df_error + 1
    ), digits = 0),
    MS = format_statistic(c(
      components$ms_contrast, components$ms_error, NA_real_
    )),
    F = format_statistic(c(components$f_value, NA_real_, NA_real_)),
    p = c(
      format_report_p(stats::pf(
        components$f_value, df1 = 1, df2 = components$df_error,
        lower.tail = FALSE
      )),
      "", ""
    ),
    eta2 = format_statistic(c(eta2, 1 - eta2, NA_real_)),
    partial_eta2 = c(format_statistic(eta2), "", ""),
    check.names = FALSE
  )
  attr(tab, "header_tooltips") <- c(
    eta2 = paste(
      "Eta squared within the contrast-specific decomposition: row SS divided",
      "by total contrast-related SS."
    ),
    partial_eta2 = paste(
      "Partial eta squared: contrast SS divided by contrast SS plus its",
      "participant-error SS. With one planned contrast, this equals eta squared."
    )
  )
  attr(tab, "cell_tooltips") <- list(
    eta2 = c(
      "Contrast eta squared = contrast SS / total contrast-related SS.",
      "Participant-error share = error SS / total contrast-related SS.",
      ""
    ),
    partial_eta2 = c(
      paste(
        "Partial eta squared = contrast SS / (contrast SS + participant-error",
        "SS); here it is identical to contrast eta squared and r_contrast",
        "squared."
      ),
      "", ""
    )
  )
  tab
}

#' Create a detailed effect-size table
#'
#' @param object A cofad result.
#' @return A formatted data frame for display in the Shiny app.
#' @noRd
detailed_effect_table <- function(object) {
  format_statistic <- function(x, digits = 3) {
    ifelse(
      is.na(x), "",
      formatC(x, format = "f", digits = digits, drop0trailing = FALSE)
    )
  }
  if (inherits(object, "cofad_bw") || inherits(object, "cofad_mx")) {
    s <- object$sig
    r <- unname(object$effects[c(
      "r_effectsize", "r_contrast", "r_alerting"
    )])
    r_squared <- c(
      s["ss_kontrast"] / s["ss_total"],
      s["ss_kontrast"] / (s["ss_kontrast"] + s["ss_within"]),
      s["ss_kontrast"] / s["ss_between"]
    )
    definitions <- if (inherits(object, "cofad_mx")) {
      c(
        "Planned mixed contrast / total variation in within-contrast scores",
        paste0(
          "Planned mixed contrast / (planned contrast + within-group score ",
          "variation)"
        ),
        "Planned mixed contrast / all between-group score variation"
      )
    } else {
      c(
        "SS contrast / SS total",
        "SS contrast / (SS contrast + SS within)",
        "SS contrast / SS between"
      )
    }
    data.frame(
      Measure = c("r effect size", "r contrast", "r alerting"),
      Estimate = format_statistic(r),
      `Squared / explained proportion` = format_statistic(r_squared),
      `Sum-of-squares definition` = definitions,
      check.names = FALSE
    )
  } else {
    data.frame(
      Measure = c("r contrast", "g contrast"),
      Estimate = format_statistic(unname(object$effects)),
      Interpretation = c(
        "Correlation-form effect linked to the contrast test",
        "Standardized mean of participants' L values"
      ),
      check.names = FALSE
    )
  }
}

cofad_effect_order_note <- function(object) {
  if (inherits(object, "cofad_bw") || inherits(object, "cofad_mx")) {
    shiny::HTML(paste0(
      "Effect-size magnitude order: |<i>r</i><sub>es</sub>| &le; ",
      "|<i>r</i><sub>contrast</sub>| and ",
      "|<i>r</i><sub>es</sub>| &le; |<i>r</i><sub>alerting</sub>|. ",
      "There is no fixed order between |<i>r</i><sub>contrast</sub>| and ",
      "|<i>r</i><sub>alerting</sub>|: |<i>r</i><sub>alerting</sub>| &ge; ",
      "|<i>r</i><sub>contrast</sub>| when residual between-group SS is no ",
      "larger than within-group/error SS; otherwise their order reverses. ",
      "The sign of all three measures indicates contrast direction."
    ))
  } else {
    shiny::HTML(paste0(
      "<i>r</i><sub>contrast</sub> and <i>g</i><sub>contrast</sub> use ",
      "different metrics, so they do not have a general magnitude ordering."
    ))
  }
}

#' Format the paper-ready report with mathematical effect-size notation
#' @noRd
cofad_report_tag <- function(report) {
  escaped <- gsub("&", "&amp;", report, fixed = TRUE)
  escaped <- gsub("<", "&lt;", escaped, fixed = TRUE)
  escaped <- gsub(">", "&gt;", escaped, fixed = TRUE)
  formatted <- gsub(
    "r\u2091\u209b", "<i>r</i><sub>es</sub>", escaped, fixed = TRUE
  )
  formatted <- gsub(
    "r_contrast", "<i>r</i><sub>contrast</sub>", formatted, fixed = TRUE
  )

  shiny::HTML(paste0(
    '<div id="cofad-report-text" class="cofad-report">',
    formatted,
    "</div>"
  ))
}

#' Convert a data frame to a small Bootstrap-compatible HTML table
#' @noRd
cofad_html_table <- function(x, id = NULL, right_align = character()) {
  header_tooltips <- attr(x, "header_tooltips")
  cell_tooltips <- attr(x, "cell_tooltips")
  has_tooltip <- function(tooltip) {
    length(tooltip) == 1 && !is.na(tooltip) && nzchar(tooltip)
  }
  table_class <- function(name, tooltip) {
    classes <- c(
      if (name %in% right_align) "cofad-number",
      if (has_tooltip(tooltip)) "cofad-tooltip"
    )
    if (length(classes)) paste(classes, collapse = " ") else NULL
  }
  display_name <- function(name) {
    if (identical(name, "Squared / explained proportion")) {
      "r\u00b2"
    } else if (identical(name, "eta2")) {
      shiny::HTML("<i>&eta;</i>&sup2;")
    } else if (identical(name, "partial_eta2")) {
      shiny::HTML("<i>&eta;</i><sub>p</sub>&sup2;")
    } else if (name %in% c("F", "p")) {
      shiny::HTML(paste0("<i>", name, "</i>"))
    } else {
      name
    }
  }
  shiny::tags$table(
    id = id,
    class = "table table-condensed cofad-booktabs",
    shiny::tags$thead(shiny::tags$tr(lapply(names(x), function(name) {
      tooltip <- if (!is.null(header_tooltips) &&
          name %in% names(header_tooltips)) {
        unname(header_tooltips[[name]])
      }
      shiny::tags$th(
        class = table_class(name, tooltip),
        title = if (has_tooltip(tooltip)) tooltip else NULL,
        tabindex = if (has_tooltip(tooltip)) "0" else NULL,
        display_name(name)
      )
    }))),
    shiny::tags$tbody(lapply(seq_len(nrow(x)), function(i) {
      shiny::tags$tr(lapply(names(x), function(name) {
        tooltip <- if (!is.null(cell_tooltips) &&
            name %in% names(cell_tooltips) &&
            length(cell_tooltips[[name]]) >= i) {
          cell_tooltips[[name]][[i]]
        }
        shiny::tags$td(
          class = table_class(name, tooltip),
          title = if (has_tooltip(tooltip)) tooltip else NULL,
          tabindex = if (has_tooltip(tooltip)) "0" else NULL,
          x[[name]][[i]]
        )
      }))
    }))
  )
}

cofad_copy_button <- function(target, label) {
  shiny::tags$button(
    type = "button", class = "btn btn-default btn-sm cofad-copy-button",
    onclick = paste0("cofadCopyTable('", target, "'); return false;"),
    label
  )
}

cofad_f_table_export_buttons <- function(target = "cofad-f-table") {
  shiny::tagList(
    shiny::tags$button(
      type = "button", class = "btn btn-default btn-sm cofad-copy-button",
      title = "Copy a fixed-width table for use with a monospaced font.",
      onclick = paste0(
        "cofadCopyTablePlain('", target, "'); return false;"
      ),
      "Copy plain text"
    ),
    shiny::tags$button(
      type = "button", class = "btn btn-default btn-sm cofad-copy-button",
      title = "Copy a formatted HTML table for rich-text applications.",
      onclick = paste0(
        "cofadCopyTableHtml('", target, "'); return false;"
      ),
      "Copy HTML"
    ),
    shiny::tags$button(
      type = "button", class = "btn btn-default btn-sm cofad-copy-button",
      title = "Download the formatted table as a Microsoft Word document.",
      onclick = paste0(
        "cofadDownloadTableDocx('", target,
        "', 'cofad-f-table.docx'); return false;"
      ),
      "Download DOCX"
    )
  )
}

#' Generate reproducible R code for the current app model
#'
#' @param model Named character vector containing the four app model roles.
#' @param lambda_between,lambda_within Favored contrast weights.
#' @param lambda_between_rival,lambda_within_rival Rival contrast weights.
#' @param compare_competing Whether favored and rival contrasts are compared.
#' @param within_score Participant-level within score, `"L"` or `"r"`.
#' @param example_name Optional package example data-set name.
#' @return A single character string containing runnable R code.
#' @noRd
cofad_r_code <- function(
    model, lambda_between = NULL, lambda_between_rival = NULL,
    lambda_within = NULL, lambda_within_rival = NULL,
    compare_competing = FALSE, within_score = "L", example_name = NULL) {
  quote_r <- function(value) encodeString(value, quote = '"')
  format_weight <- function(value) {
    format(signif(value, 10), trim = TRUE, scientific = FALSE)
  }
  weight_code <- function(name, values) {
    if (is.null(values)) return(character())
    entries <- paste0(
      "  ", quote_r(names(values)), " = ",
      vapply(values, format_weight, character(1))
    )
    paste0(name, " <- c(\n", paste(entries, collapse = ",\n"), "\n)")
  }
  variable_argument <- function(argument, variable) {
    if (!nzchar(variable)) return(character())
    reserved <- c(
      "if", "else", "repeat", "while", "function", "for", "in", "next",
      "break", "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_",
      "NA_real_", "NA_complex_", "NA_character_"
    )
    syntactic <- make.names(variable) == variable && !variable %in% reserved
    display <- if (syntactic) {
      variable
    } else {
      paste0("`", gsub("`", "\\`", variable, fixed = TRUE), "`")
    }
    paste0("  ", argument, " = ", display)
  }
  contrast_code <- function(prefix, favored, rival) {
    if (is.null(favored)) return(character())
    if (isTRUE(compare_competing) && !is.null(rival)) {
      c(
        weight_code(paste0(prefix, "_favored"), favored),
        weight_code(paste0(prefix, "_rival"), rival),
        paste0(
          prefix, " <- lambda_diff(\n",
          "  lambda_favored = ", prefix, "_favored,\n",
          "  lambda_rival = ", prefix, "_rival\n)"
        )
      )
    } else {
      weight_code(prefix, favored)
    }
  }

  data_name <- if (!is.null(example_name) && nzchar(example_name)) {
    example_name
  } else {
    "dat"
  }
  preamble <- if (!is.null(example_name) && nzchar(example_name)) {
    c(
      "library(cofad)",
      paste0("data(", quote_r(example_name), ")")
    )
  } else {
    c(
      "library(cofad)",
      "# Replace dat with the data frame you imported."
    )
  }
  definitions <- c(
    contrast_code(
      "lambda_between", lambda_between, lambda_between_rival
    ),
    contrast_code("lambda_within", lambda_within, lambda_within_rival)
  )
  arguments <- c(
    variable_argument("dv", model[["dv_name"]]),
    variable_argument("between", model[["between_name"]]),
    if (!is.null(lambda_between)) "  lambda_between = lambda_between",
    variable_argument("within", model[["within_name"]]),
    if (!is.null(lambda_within)) "  lambda_within = lambda_within",
    variable_argument("id", model[["id_name"]]),
    if (nzchar(model[["within_name"]])) {
      paste0("  within_score = ", quote_r(within_score))
    },
    paste0("  data = ", data_name)
  )
  call <- paste0(
    "result <- calc_contrast(\n",
    paste(arguments, collapse = ",\n"),
    "\n)"
  )
  sub(
    "^[[:space:]]+", "",
    paste(c(preamble, definitions, call, "result"), collapse = "\n\n")
  )
}

#' Calculate variance shares under the three effect-size denominators
#' @noRd
variance_partition_data <- function(object) {
  if (inherits(object, "cofad_wi")) {
    within <- within_f_components(object)
    components <- stats::setNames(
      c(within$ss_contrast, within$ss_error),
      c("Contrast", "Contrast \u00d7 participants/error")
    )
    shares <- matrix(
      components / within$ss_total, nrow = 1,
      dimnames = list(contrast_total = "Contrast-related SS", names(components))
    )
    numerators <- matrix(
      components, nrow = 1,
      dimnames = list(contrast_total = "Contrast-related SS", names(components))
    )
    eta2 <- unname(components[[1]] / within$ss_total)
    return(list(
      components = components,
      numerators = numerators,
      denominators = c(contrast_total = within$ss_total),
      shares = shares,
      metrics = c(eta2 = eta2, partial_eta2 = eta2),
      row_labels = "Contrast-related SS"
    ))
  }
  s <- object$sig
  component_names <- if (inherits(object, "cofad_mx")) {
    c(
      "Planned mixed contrast", "Other between-group score variation",
      "Within-group score variation"
    )
  } else {
    c("Contrast", "Other between-group", "Within-group/error")
  }
  components <- stats::setNames(c(
    unname(s["ss_kontrast"]),
    unname(s["ss_between"] - s["ss_kontrast"]),
    unname(s["ss_within"])
  ), component_names)
  components[abs(components) < sqrt(.Machine$double.eps)] <- 0

  denominators <- c(
    total = unname(s["ss_total"]),
    between = unname(s["ss_between"]),
    contrast_error = unname(s["ss_kontrast"] + s["ss_within"])
  )
  numerators <- rbind(
    total = unname(components),
    between = c(unname(components[c(1, 2)]), 0),
    contrast_error = c(components[[1]], 0, components[[3]])
  )
  shares <- numerators
  for (i in seq_len(nrow(shares))) {
    shares[i, ] <- if (is.finite(denominators[[i]]) &&
        denominators[[i]] > 0) {
      numerators[i, ] / denominators[[i]]
    } else {
      rep(NA_real_, ncol(shares))
    }
  }
  colnames(shares) <- names(components)
  colnames(numerators) <- names(components)

  metrics <- c(
    r_es2 = components[[1]] / denominators[["total"]],
    r_alerting2 = components[[1]] / denominators[["between"]],
    r_contrast2 = components[[1]] / denominators[["contrast_error"]]
  )
  list(
    components = components,
    numerators = numerators,
    denominators = denominators,
    shares = shares,
    metrics = metrics,
    row_labels = c("Total SS", "Between-group SS", "Contrast + error SS")
  )
}

#' Plot the partition of total variation and effect-size denominators
#' @noRd
plot_variance_partition <- function(object) {
  partition <- variance_partition_data(object)
  components <- partition$components
  colors <- c("#E69F00", "#56B4E9", "#BDBDBD")
  y_positions <- c(3, 2, 1)

  old_par <- graphics::par(mar = c(3.3, 8.5, 1.1, 7.8), xpd = NA)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::plot(
    NA_real_, NA_real_, type = "n", axes = FALSE,
    xlim = c(0, 1), ylim = c(0.5, 3.5), xlab = "", ylab = ""
  )
  graphics::axis(
    1, at = seq(0, 1, by = 0.2),
    labels = paste0(seq(0, 100, by = 20), "%")
  )
  graphics::axis(
    2, at = y_positions, labels = partition$row_labels,
    tick = FALSE, las = 1, cex.axis = 0.84
  )
  graphics::mtext("Share of the row denominator", side = 1, line = 2.2,
                  cex = 0.85)

  for (row in seq_len(nrow(partition$shares))) {
    left <- 0
    for (segment in seq_len(ncol(partition$shares))) {
      share <- partition$shares[row, segment]
      if (!is.finite(share) || share <= 0) next
      graphics::rect(
        left, y_positions[[row]] - 0.28, left + share,
        y_positions[[row]] + 0.28, col = colors[[segment]], border = "white"
      )
      if (share >= 0.055) {
        graphics::text(
          left + share / 2, y_positions[[row]],
          labels = paste0(round(100 * share, 1), "%"), cex = 0.78
        )
      }
      left <- left + share
    }
  }

  metric_expressions <- list(
    bquote(r[es]^2 == .(round(partition$metrics[[1]], 3))),
    bquote(r[alerting]^2 == .(round(partition$metrics[[2]], 3))),
    bquote(r[contrast]^2 == .(round(partition$metrics[[3]], 3)))
  )
  for (row in seq_along(y_positions)) {
    graphics::text(
      1.025, y_positions[[row]], labels = metric_expressions[[row]],
      adj = c(0, 0.5), cex = 0.82
    )
  }
  graphics::legend(
    x = 0, y = 0.43, legend = names(components), fill = colors, border = NA,
    horiz = TRUE, bty = "n", xpd = NA, cex = 0.72
  )
  invisible(components)
}

#' Create an interactive variance-partition figure
#' @noRd
plotly_variance_partition <- function(object) {
  partition <- variance_partition_data(object)
  within_result <- inherits(object, "cofad_wi")
  # ColorBrewer Set2: qualitative, colorblind-friendly, and readable on white.
  colors <- c("#66C2A5", "#FC8D62", "#8DA0CB")[
    seq_len(ncol(partition$shares))
  ]
  format_ss <- function(x) trimws(formatC(x, digits = 4, format = "fg"))
  component_labels <- if (within_result) {
    names(partition$components)
  } else {
    c("Contrast", "Other between-group", "Within-group/error")
  }

  figure <- plotly::plot_ly()
  for (segment in seq_len(ncol(partition$shares))) {
    shares <- partition$shares[, segment]
    labels <- ifelse(
      is.finite(shares) & shares >= 0.055,
      paste0(round(100 * shares, 1), "%"), ""
    )
    hover_labels <- paste0(
      "<b>", component_labels[[segment]], "</b><br>",
      partition$row_labels, "<br>",
      "SS = ", format_ss(partition$numerators[, segment]), "<br>",
      "Share = ", round(100 * shares, 1), "%<br>",
      format_ss(partition$numerators[, segment]), " / ",
      format_ss(unname(partition$denominators))
    )
    figure <- plotly::add_bars(
      figure,
      x = shares,
      y = partition$row_labels,
      name = component_labels[[segment]],
      orientation = "h",
      marker = list(color = colors[[segment]], line = list(color = "white")),
      text = labels,
      textposition = "inside",
      insidetextanchor = "middle",
      hovertext = hover_labels,
      hovertemplate = "%{hovertext}<extra></extra>"
    )
  }

  metric_html <- if (within_result) {
    paste0(
      "<i>\u03b7</i><sup>2</sup> = <i>\u03b7</i><sub>p</sub><sup>2</sup> = ",
      "<i>r</i><sub>contrast</sub><sup>2</sup> = ",
      formatC(partition$metrics[["eta2"]], digits = 3, format = "f")
    )
  } else {
    c(
      "<i>r</i><sub>es</sub><sup>2</sup>",
      "<i>r</i><sub>alerting</sub><sup>2</sup>",
      "<i>r</i><sub>contrast</sub><sup>2</sup>"
    )
  }
  annotations <- lapply(seq_along(partition$row_labels), function(row) {
    list(
      x = 1.015, y = partition$row_labels[[row]], xref = "x", yref = "y",
      text = if (within_result) metric_html[[row]] else paste0(
        metric_html[[row]], " = ",
        formatC(partition$metrics[[row]], digits = 3, format = "f")
      ),
      showarrow = FALSE, xanchor = "left", align = "left"
    )
  })

  figure <- plotly::layout(
    figure,
    barmode = "stack",
    bargap = 0.34,
    showlegend = TRUE,
    legend = list(
      orientation = "h", x = 0, y = 1.18,
      xanchor = "left", yanchor = "bottom", traceorder = "normal",
      font = list(size = 10.5)
    ),
    margin = list(l = 132, r = 90, t = 72, b = 55),
    xaxis = list(
      title = "Share of the row denominator",
      range = c(0, 1.23),
      tickvals = seq(0, 1, by = 0.2),
      ticktext = paste0(seq(0, 100, by = 20), "%"),
      fixedrange = TRUE
    ),
    yaxis = list(
      title = "",
      categoryorder = "array",
      categoryarray = rev(partition$row_labels),
      ticksuffix = "\u00a0\u00a0",
      fixedrange = TRUE
    ),
    annotations = annotations,
    hovermode = "closest"
  )
  plotly::config(
    figure, responsive = TRUE, displaylogo = FALSE,
    modeBarButtonsToRemove = c(
      "select2d", "lasso2d", "zoom2d", "pan2d", "autoScale2d"
    )
  )
}

#' Calculate lambdas for two competing hypotheses
#'
#' If you want to test two competing hypotheses, you can use this helper
#' function to create the correct difference lambdas. There is no magic here.
#' The two contrasts are z-standardized first and then subtracted
#' (lambda_favored - lambda_rival). You can use the new difference lambdas
#' as the input for calc_contrast.
#'
#' @param lambda_favored Lambdas of the favored hypothesis. Has to be a
#'   named vector with the names corresponding with the groups in the analyzed
#'   data set. Alternatively, use the parameter `labels`.
#' @param lambda_rival Lambdas of the rival hypothesis. Has to be a
#'   named vector with the names corresponding with the groups in the analyzed
#'   data set. Alternatively, use the parameter `labels`.
#' @param labels If you provide lambdas without names, you can set the group
#'   labels for both contrasts here.
#' @param lambda_preferred Deprecated. Use `lambda_favored` instead.
#' @param lambda_competing Deprecated. Use `lambda_rival` instead.
#'
#' @return Lambdas for difference between lambda_favored and lambda_rival
#'
#' @examples
#' lambda <- lambda_diff(lambda_favored = c("A" = 1, "B" = 2, "C" = 3),
#'                       lambda_rival = c("A" = 1, "B" = 2, "C" = 6))
#' lambda
#' # same result
#' lambda2 <- lambda_diff(lambda_favored = c(1, 2, 3),
#'                        lambda_rival = c(1, 2, 6),
#'                        labels = c("A", "B", "C"))
#' lambda2
#' @export
lambda_diff <- function(lambda_favored = NULL,
                        lambda_rival = NULL,
                        labels = NULL,
                        lambda_preferred = NULL,
                        lambda_competing = NULL) {

  # Deprecation handling
  if (!is.null(lambda_preferred)) {
    warning(
      "`lambda_preferred` is deprecated; use `lambda_favored` instead.",
      call. = FALSE
    )
    lambda_favored <- lambda_preferred
  }
  if (!is.null(lambda_competing)) {
    warning(
      "`lambda_competing` is deprecated; use `lambda_rival` instead.",
      call. = FALSE
    )
    lambda_rival <- lambda_competing
  }

  # Argument checks
  if (cor(lambda_favored, lambda_rival) == 1) {
    stop('Your lambdas are perfectly correlated. ',
         'It does not make sense to compare them.')
  }
  if ((is.null(names(lambda_favored)) | is.null(names(lambda_rival))) &
      is.null(labels)) {
    stop('Please provide group labels for your lambdas. ',
         'For instance, c("A" = 1, "B" = 2, ...)')
  }
  if ((!is.null(names(lambda_favored)) | !is.null(names(lambda_rival))) &
      !is.null(labels)) {
    stop('Use either a named vector for the lambdas',
         ' or the labels parameter to specify the group labels. ',
         'Do not use both.')
  }
  if (!is.null(labels)) {
    names(lambda_favored) <- names(lambda_rival) <- labels
  }

  lambda_favored <- lambda_favored[sort(names(lambda_favored))]
  lambda_rival <- lambda_rival[sort(names(lambda_rival))]

  if (!(identical(names(lambda_favored), names(lambda_rival)))) {
    stop('Please provide the same labels for your lambdas\n',
         'current labels of favored lambdas: ',
         paste(names(lambda_favored), collapse = " "),
         '\ncurrent labels of rival lambdas: ',
         paste(names(lambda_rival), collapse = " "))
  }

  lambda_diff <- as.numeric(zscale(lambda_favored) - zscale(lambda_rival))
  names(lambda_diff) <- names(lambda_favored)
  return(lambda_diff)
}

#' Validate and combine two competing contrast vectors for the app
#' @noRd
cofad_competing_lambda <- function(lambda_favored, lambda_rival) {
  if (!is.numeric(lambda_favored) || !is.numeric(lambda_rival) ||
      !length(lambda_favored) || length(lambda_favored) != length(lambda_rival)) {
    stop("Favored and rival contrasts must be numeric vectors of equal length.")
  }
  if (is.null(names(lambda_favored)) || is.null(names(lambda_rival)) ||
      !setequal(names(lambda_favored), names(lambda_rival))) {
    stop("Favored and rival contrasts must use the same level names.")
  }
  lambda_rival <- lambda_rival[names(lambda_favored)]
  if (anyNA(lambda_favored) || anyNA(lambda_rival) ||
      any(!is.finite(lambda_favored)) || any(!is.finite(lambda_rival))) {
    stop("Favored and rival contrasts must contain finite values.")
  }
  favored_sd <- stats::sd(lambda_favored)
  rival_sd <- stats::sd(lambda_rival)
  if (!is.finite(favored_sd) || !is.finite(rival_sd) ||
      favored_sd == 0 || rival_sd == 0) {
    stop("Each competing contrast must contain at least two different values.")
  }
  relationship <- stats::cor(lambda_favored, lambda_rival)
  if (!is.finite(relationship) ||
      abs(relationship - 1) <= sqrt(.Machine$double.eps)) {
    stop("Favored and rival contrasts cannot be perfectly identical in shape.")
  }
  lambda_diff(lambda_favored, lambda_rival)
}


zscale <- function(x) {
  n <- length(x)
  sqrt(n / (n - 1)) * (x - mean(x)) / sd(x)
}

cn <- function(...) {
  values <- list(...)
  expressions <- as.character(substitute(list(...)))[-1]
  supplied_names <- names(values)
  if (is.null(supplied_names)) supplied_names <- rep("", length(values))
  names(values) <- ifelse(nzchar(supplied_names), supplied_names, expressions)
  unlist(values)
}
