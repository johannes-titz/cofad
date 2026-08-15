test_that("F-table export controls offer plain, HTML, and DOCX formats", {
  buttons <- as.character(cofad_f_table_export_buttons())
  script_path <- cofad_resource("cofad-copy.js")
  script <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

  expect_match(buttons, "Copy plain text", fixed = TRUE)
  expect_match(buttons, "cofadCopyTablePlain", fixed = TRUE)
  expect_match(buttons, "Copy HTML", fixed = TRUE)
  expect_match(buttons, "cofadCopyTableHtml", fixed = TRUE)
  expect_match(buttons, "Download DOCX", fixed = TRUE)
  expect_match(buttons, "cofadDownloadTableDocx", fixed = TRUE)
  expect_match(script, "fixedWidthTable", fixed = TRUE)
  expect_match(script, "word/document.xml", fixed = TRUE)
  expect_match(script, '["F", "p"].includes', fixed = TRUE)
  expect_match(script, "<w:i/>", fixed = TRUE)
  expect_match(
    script,
    "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    fixed = TRUE
  )
})

test_that("copy controls use the quiet uniform action style", {
  ui_source <- paste(deparse(body(myui)), collapse = "\n")
  citation <- as.character(cofad_citation_panel())

  expect_match(ui_source, "background-color: #f5f6f7", fixed = TRUE)
  expect_match(ui_source, "border: 1px solid #aeb4bb", fixed = TRUE)
  expect_match(ui_source, "align-items: stretch", fixed = TRUE)
  expect_match(ui_source, "width: 100%", fixed = TRUE)
  expect_match(ui_source, ".cofad-footer { color: #fff", fixed = TRUE)
  expect_match(citation, "cofad-copy-button", fixed = TRUE)
})

test_that("Plotly variance components use ColorBrewer Set2 colors", {
  data("rosenthal_tbl31", package = "cofad")
  result <- calc_contrast(
    dv = rosenthal_tbl31$dv,
    between = rosenthal_tbl31$between,
    lambda_between = c(A = -3, B = -1, C = 1, D = 3)
  )
  built <- plotly::plotly_build(plotly_variance_partition(result))
  colors <- vapply(
    built$x$data, function(trace) trace$marker$color, character(1)
  )

  expect_identical(colors, c("#66C2A5", "#FC8D62", "#8DA0CB"))
})
