test_that("result tables support compact layout, numeric alignment, and copying", {
  table <- cofad_html_table(
    data.frame(Source = "Contrast", SS = "1.250", F = "4.000"),
    id = "cofad-f-table", right_align = c("SS", "F")
  )
  html <- as.character(table)
  expect_match(html, 'id="cofad-f-table"', fixed = TRUE)
  expect_match(html, "cofad-booktabs", fixed = TRUE)
  expect_match(html, '<th class="cofad-number">SS</th>', fixed = TRUE)
  expect_match(html, '<th class="cofad-number"><i>F</i></th>', fixed = TRUE)
  expect_match(html, '<td class="cofad-number">1.250</td>', fixed = TRUE)

  effect_html <- as.character(cofad_html_table(data.frame(
    `Squared / explained proportion` = "0.250", check.names = FALSE
  )))
  expect_match(effect_html, "r\u00b2", fixed = TRUE)

  button <- as.character(cofad_copy_button("cofad-f-table", "Copy F table"))
  expect_match(button, "cofadCopyTable")
})

test_that("the F table reports eta squared and precise small p values", {
  group <- factor(rep(c("A", "B"), each = 10))
  result <- calc_contrast(
    dv = c(1:10, 21:30), between = group,
    lambda_between = c(A = -1, B = 1)
  )
  table <- detailed_f_table(result)
  expected_eta2 <- unname(result$sig["ss_kontrast"] / result$sig["ss_total"])

  expect_equal(as.numeric(table$eta2[[2]]), expected_eta2, tolerance = 0.001)
  expect_match(table$p[[2]], "e-", fixed = TRUE)
  expect_false(grepl("<", table$p[[2]], fixed = TRUE))
  html <- as.character(cofad_html_table(table))
  expect_match(html, "<i>F</i>", fixed = TRUE)
  expect_match(html, "<i>p</i>", fixed = TRUE)
  expect_match(html, "&eta;", fixed = TRUE)
  expect_match(html, "&sup2;", fixed = TRUE)
})

test_that("paper-ready reports explain participant L and r scores", {
  data(rosenthal_tbl53)
  args <- list(
    dv = rosenthal_tbl53$dv,
    between = rosenthal_tbl53$between,
    lambda_between = c(age8 = -1, age10 = 0, age12 = 1),
    within = rosenthal_tbl53$within,
    lambda_within = c(`1` = -3, `2` = -1, `3` = 1, `4` = 3),
    id = rosenthal_tbl53$id
  )
  report_l <- trimws(paste(
    capture.output(print(do.call(calc_contrast, args))), collapse = " "
  ))
  args$within_score <- "r"
  report_r <- trimws(paste(
    capture.output(print(do.call(calc_contrast, args))), collapse = " "
  ))

  expect_match(report_l, "^We ran a mixed contrast analysis")
  expect_match(report_l, "weighted sums retaining response magnitude")
  expect_match(report_r, "correlations measuring agreement")
  expect_match(report_l, "t\\([0-9]+\\) =")
  expect_match(report_l, "r\u2091\u209b =", fixed = TRUE)
  expect_false(grepl("resulted in F\\(", report_l))
})

test_that("within paper-ready reports use r contrast", {
  data(sedlmeier_p537)
  result <- calc_contrast(
    dv = reading_test, within = music, id = participant,
    lambda_within = c(
      "without music" = 1.25, "white noise" = 0.25,
      classic = -0.75, jazz = -0.75
    ),
    data = sedlmeier_p537
  )
  report <- trimws(paste(capture.output(print(result)), collapse = " "))
  html <- as.character(cofad_report_tag(report))

  expect_match(report, "r_contrast = 0.894", fixed = TRUE)
  expect_false(grepl("g_effectsize", report, fixed = TRUE))
  expect_match(html, "<i>r</i><sub>contrast</sub>", fixed = TRUE)
})

test_that("directional reports retain an opposite-direction warning", {
  data(furr_p4)
  result <- calc_contrast(
    dv = furr_p4$empathy,
    between = furr_p4$major,
    lambda_between = c(
      business = 3, chemistry = 1, education = -1, psychology = -3
    )
  )
  report <- trimws(paste(capture.output(print(result)), collapse = " "))

  expect_match(report, "t\\([0-9]+\\) = -")
  expect_match(report, "Attention: Contrast fits in the opposite direction")
})

test_that("mixed tables describe variation in derived within-contrast scores", {
  data(rosenthal_tbl53)
  result <- calc_contrast(
    dv = rosenthal_tbl53$dv,
    between = rosenthal_tbl53$between,
    lambda_between = c(age8 = -1, age10 = 0, age12 = 1),
    within = rosenthal_tbl53$within,
    lambda_within = c(`1` = -3, `2` = -1, `3` = 1, `4` = 3),
    id = rosenthal_tbl53$id
  )

  expect_match(detailed_f_table(result)$Source[[1]], "within-contrast scores")
  expect_match(
    detailed_effect_table(result)$`Sum-of-squares definition`[[1]],
    "total variation in within-contrast scores"
  )
})

test_that("model UI hides contrast tables for absent factors", {
  server_source <- paste(deparse(body(myserver)), collapse = "\n")

  expect_match(
    server_source,
    '"input.between_name != \'\' && (input.within_name == \'\' ||"',
    fixed = TRUE
  )
  expect_match(
    server_source, '"input.mixed_effect == \'interaction\')"', fixed = TRUE
  )
  expect_match(server_source, 'condition = "input.within_name != \'\'"', fixed = TRUE)
})

test_that("reports use scientific notation for very small p values", {
  expect_identical(format_report_p(0.000012345), "1.234e-05")
  expect_identical(format_report_p(0.012346), "0.01235")
})

test_that("effect-size notes state the guaranteed and conditional order", {
  data(furr_p4)
  between <- calc_contrast(
    dv = furr_p4$empathy, between = furr_p4$major,
    lambda_between = c(
      business = -1, chemistry = -1, education = 1, psychology = 1
    )
  )
  note <- as.character(cofad_effect_order_note(between))
  expect_match(note, "|<i>r</i><sub>es</sub>|", fixed = TRUE)
  expect_false(grepl("r_effectsize", note, fixed = TRUE))
  expect_match(note, "There is no fixed order", fixed = TRUE)
  expect_match(note, "residual between-group SS", fixed = TRUE)
})

test_that("the visible report uses math notation and copies Unicode subscripts", {
  report <- "We found r\u2091\u209b = .42 < the upper bound."
  html <- as.character(cofad_report_tag(report))
  script <- paste(
    readLines(cofad_resource("cofad-copy.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(html, "<i>r</i>", fixed = TRUE)
  expect_match(html, "<sub>es</sub>", fixed = TRUE)
  expect_match(html, "&lt; the upper bound", fixed = TRUE)
  expect_match(script, '"text/html": new Blob([html]', fixed = TRUE)
  expect_match(script, "report.innerHTML", fixed = TRUE)
  expect_match(script, "plainSource.value.trim()", fixed = TRUE)
  expect_match(
    paste(deparse(body(myserver)), collapse = "\n"),
    "Copy report (HTML)", fixed = TRUE
  )
})

test_that("the app title includes version and webR build information", {
  old_options <- options(
    cofad.version = "9.8.7", cofad.webR = FALSE, cofad.commit = NULL
  )
  on.exit(options(old_options), add = TRUE)

  expect_identical(cofad_version_label(), "cofad v9.8.7")
  options(
    cofad.version = "9.8.7.9000", cofad.webR = TRUE,
    cofad.commit = "abc12345"
  )
  expect_identical(
    cofad_version_label(), "cofad v9.8.7.9000 (dev abc12345)"
  )
  expect_match(as.character(cofad_version_title()), "dev abc12345", fixed = TRUE)
})

test_that("all four main app panels are collapsible and Help starts open", {
  html <- as.character(myui(NULL))
  collapse_controls <- gregexpr('data-widget="collapse"', html, fixed = TRUE)
  expect_length(regmatches(html, collapse_controls)[[1]], 4)
  expect_false(grepl("collapsed-box", html, fixed = TRUE))
})
