test_that("copy controls sit beside their content", {
  citation <- as.character(cofad_citation_panel())
  ui_source <- paste(deparse(body(myui)), collapse = "\n")
  server_source <- paste(deparse(body(myserver)), collapse = "\n")

  expect_match(
    ui_source, ".cofad-copy-layout { display: flex", fixed = TRUE
  )
  expect_match(
    citation,
    'class="cofad-copy-layout cofad-citation-layout"', fixed = TRUE
  )
  expect_match(
    citation,
    'class="cofad-copy-actions cofad-citation-actions"', fixed = TRUE
  )
  expect_match(
    server_source,
    'class = "cofad-copy-layout cofad-copy-layout-report"', fixed = TRUE
  )
  expect_match(server_source, 'class = "cofad-copy-actions"', fixed = TRUE)
})

test_that("example details appear only as option tooltips", {
  ui <- as.character(myui(NULL))
  selector <- as.character(cofad_example_select())

  expect_false(grepl('id="example_description"', ui, fixed = TRUE))
  expect_match(
    selector,
    'title="Four independent groups with a linear contrast', fixed = TRUE
  )
})
