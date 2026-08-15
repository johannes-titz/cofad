test_that("F table reports partial eta squared for tested components", {
  data("rosenthal_tbl31", package = "cofad")
  result <- calc_contrast(
    dv = rosenthal_tbl31$dv,
    between = rosenthal_tbl31$between,
    lambda_between = c(A = -3, B = -1, C = 1, D = 3)
  )
  table <- detailed_f_table(result)
  s <- result$sig
  residual_ss <- unname(s["ss_between"] - s["ss_kontrast"])

  expect_equal(
    as.numeric(table$partial_eta2[1:3]),
    c(
      unname(s["ss_between"] / (s["ss_between"] + s["ss_within"])),
      unname(s["ss_kontrast"] / (s["ss_kontrast"] + s["ss_within"])),
      unname(residual_ss / (residual_ss + s["ss_within"]))
    ),
    tolerance = 0.001
  )
  expect_equal(
    as.numeric(table$partial_eta2[[2]]),
    unname(result$effects[["r_contrast"]]^2),
    tolerance = 0.001
  )
  expect_identical(table$partial_eta2[4:5], c("", ""))
})

test_that("eta calculations are tooltipped with dotted underlines", {
  data("rosenthal_tbl31", package = "cofad")
  result <- calc_contrast(
    dv = rosenthal_tbl31$dv,
    between = rosenthal_tbl31$between,
    lambda_between = c(A = -3, B = -1, C = 1, D = 3)
  )
  table <- detailed_f_table(result)
  html <- as.character(cofad_html_table(
    table,
    right_align = c("SS", "df", "MS", "F", "p", "eta2", "partial_eta2")
  ))
  ui_source <- paste(deparse(body(myui)), collapse = "\n")

  expect_match(
    attr(table, "header_tooltips")[["partial_eta2"]],
    "effect SS plus within-group/error SS", fixed = TRUE
  )
  expect_match(
    attr(table, "cell_tooltips")$partial_eta2[[2]],
    "equivalent to r_contrast squared", fixed = TRUE
  )
  expect_match(html, "<sub>p</sub>", fixed = TRUE)
  expect_false(grepl(
    '<(th|td)(?![^>]*cofad-tooltip)[^>]*title=', html, perl = TRUE
  ))
  expect_match(ui_source, "text-decoration-style: dotted", fixed = TRUE)
})
