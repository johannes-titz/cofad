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

  expect_equal(as.numeric(table$F), unname(result$sig[[1]])^2, tolerance = 0.001)
  expect_identical(table$df1, "1")
  expect_identical(as.numeric(table$df2), unname(result$sig[[3]]))
  expect_equal(
    as.numeric(table$p),
    stats::pf(unname(result$sig[[1]])^2, 1, unname(result$sig[[3]]),
              lower.tail = FALSE),
    tolerance = 1e-4
  )
  html <- as.character(cofad_html_table(table))
  expect_match(html, "F = t squared", fixed = TRUE)
  expect_match(html, "cofad-tooltip", fixed = TRUE)
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
  expect_match(between_code, 'data("rosenthal_tbl31"', fixed = TRUE)
  expect_match(between_code, "lambda_between <- c(", fixed = TRUE)
  expect_match(between_code, "cofad::calc_contrast", fixed = TRUE)
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
  expect_match(within_code, "cofad::lambda_diff", fixed = TRUE)
  expect_match(within_code, 'within_score = "r"', fixed = TRUE)
  expect_match(within_code, 'id = as.factor(dat[["participant"]])', fixed = TRUE)
  expect_match(within_code, "Replace dat", fixed = TRUE)
})

test_that("R code panel exposes a plain-text copy control", {
  ui <- as.character(myui(NULL))
  script <- paste(
    readLines(cofad_resource("cofad-copy.js"), warn = FALSE), collapse = "\n"
  )

  expect_match(ui, 'id="code_region"', fixed = TRUE)
  expect_match(ui, '<h3 class="box-title">4. R code</h3>', fixed = TRUE)
  expect_match(script, "window.cofadCopyRCode", fixed = TRUE)
  expect_match(script, "cofad-r-code-copy-text", fixed = TRUE)
})
