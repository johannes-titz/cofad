test_that("within contrast has an exact nondirectional F equivalent", {
  data("sedlmeier_p537", package = "cofad")
  result <- calc_contrast(
    dv = reading_test, within = music, id = participant,
    lambda_within = c(
      "without music" = 1.25, "white noise" = 0.25,
      classic = -0.75, jazz = -0.75
    ),
    data = sedlmeier_p537
  )
  table <- detailed_within_f_table(result)

  expect_equal(as.numeric(table$F[[1]]), unname(result$sig[[1]])^2,
               tolerance = 0.001)
  expect_identical(table$df, c("1", "7", "8"))
  expect_identical(
    table$Source,
    c(
      "Within-subjects contrast", "Contrast × participants (error)",
      "Total contrast-related variation"
    )
  )
  expect_equal(as.numeric(table$MS[[1]]) / as.numeric(table$MS[[2]]),
               as.numeric(table$F[[1]]), tolerance = 0.002)
  expect_equal(as.numeric(table$SS[[3]]),
               sum(as.numeric(table$SS[1:2])), tolerance = 0.002)
  expect_equal(as.numeric(table$eta2[[1]]),
               as.numeric(table$partial_eta2[[1]]), tolerance = 0.001)
  expect_equal(
    as.numeric(table$partial_eta2[[1]]),
    unname(result$effects[[1]]^2), tolerance = 0.001
  )
  expect_equal(sum(as.numeric(table$eta2[1:2])), 1, tolerance = 0.001)
  expect_equal(
    as.numeric(table$p[[1]]),
    stats::pf(unname(result$sig[[1]])^2, 1, unname(result$sig[[3]]),
              lower.tail = FALSE),
    tolerance = 1e-4
  )
  html <- as.character(cofad_html_table(table))
  expect_match(html, "&eta;", fixed = TRUE)
  expect_match(
    attr(table, "cell_tooltips")$partial_eta2[[1]],
    "r_contrast squared", fixed = TRUE
  )
  expect_match(html, "cofad-tooltip", fixed = TRUE)
  expect_identical(
    names(attr(table, "header_tooltips")), c("eta2", "partial_eta2")
  )
  expect_identical(
    names(attr(table, "cell_tooltips")), c("eta2", "partial_eta2")
  )
  expect_match(html, "<th>SS</th>", fixed = TRUE)
  expect_match(html, "<th><i>F</i></th>", fixed = TRUE)
})

test_that("R code generator reproduces single and competing app models", {
  between_model <- c(
    dv_name = "dv", between_name = "between", within_name = "", id_name = ""
  )
  between_code <- cofad_r_code(
    between_model,
    lambda_between = c(A = -3, B = -1, C = 1, D = 3),
    example_name = "rosenthal_tbl31"
  )
  expect_match(between_code, "library(cofad)", fixed = TRUE)
  expect_match(between_code, "^library\\(cofad\\)")
  expect_match(between_code, 'data("rosenthal_tbl31")', fixed = TRUE)
  expect_match(between_code, "lambda_between <- c(", fixed = TRUE)
  expect_match(between_code, "result <- calc_contrast", fixed = TRUE)
  expect_match(between_code, "dv = dv", fixed = TRUE)
  expect_match(between_code, "between = between", fixed = TRUE)
  expect_match(between_code, "data = rosenthal_tbl31", fixed = TRUE)
  expect_false(grepl("cofad::", between_code, fixed = TRUE))
  expect_false(grepl("lambda_diff", between_code, fixed = TRUE))

  within_model <- c(
    dv_name = "reading_test", between_name = "", within_name = "music",
    id_name = "participant"
  )
  within_code <- cofad_r_code(
    within_model,
    lambda_within = c(a = 1, b = -1),
    lambda_within_rival = c(a = -1, b = 1),
    compare_competing = TRUE, within_score = "r"
  )
  expect_match(within_code, "lambda_diff", fixed = TRUE)
  expect_false(grepl("cofad::", within_code, fixed = TRUE))
  expect_match(within_code, 'within_score = "r"', fixed = TRUE)
  expect_match(within_code, "id = participant", fixed = TRUE)
  expect_match(within_code, "data = dat", fixed = TRUE)
  expect_match(within_code, "Replace dat", fixed = TRUE)
})

test_that("R code panel exposes a plain-text copy control", {
  ui <- as.character(myui(NULL))
  ui_source <- paste(deparse(body(myui)), collapse = "\n")
  script <- paste(
    readLines(cofad_resource("cofad-copy.js"), warn = FALSE), collapse = "\n"
  )

  expect_match(ui, 'id="code_region"', fixed = TRUE)
  expect_match(ui, '<h3 class="box-title">4. R code</h3>', fixed = TRUE)
  expect_match(ui_source, "justify-content: flex-start", fixed = TRUE)
  expect_match(script, "window.cofadCopyRCode", fixed = TRUE)
  expect_match(script, "cofad-r-code-copy-text", fixed = TRUE)

  shiny::testServer(myserver, {
    session$setInputs(example_dataset = "rosenthal_tbl31")
    session$flushReact()
    code_html <- output$r_code_region$html
    expect_lt(
      regexpr("cofad-r-code-actions", code_html, fixed = TRUE)[[1]],
      regexpr('id="cofad-r-code"', code_html, fixed = TRUE)[[1]]
    )
    expect_match(code_html, "library(cofad)", fixed = TRUE)
  })
})
