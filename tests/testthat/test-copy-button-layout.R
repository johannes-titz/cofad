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
  expect_match(
    server_source, 'class = "cofad-table-content"', fixed = TRUE
  )
  expect_match(server_source, 'class = "cofad-copy-actions"', fixed = TRUE)
})

test_that("headings inside panels are smaller than panel titles", {
  ui_source <- paste(deparse(body(myui)), collapse = "\n")

  expect_match(
    ui_source, ".box-header .box-title { font-size: 18px", fixed = TRUE
  )
  expect_match(ui_source, ".box-body h4 { font-size: 15px", fixed = TRUE)
})

test_that("Handsontable columns redraw after hidden panels become visible", {
  ui_source <- paste(deparse(body(myui)), collapse = "\n")
  script <- paste(
    readLines(cofad_resource("cofad-copy.js"), warn = FALSE), collapse = "\n"
  )

  expect_match(
    ui_source, ".cofad-hot-wrap .rhandsontable { max-width: none", fixed = TRUE
  )
  expect_match(script, "scheduleHotTableRender", fixed = TRUE)
  expect_match(script, 'window.jQuery(document).on("shiny:value"', fixed = TRUE)
  expect_match(script, "widget.hot.render()", fixed = TRUE)
})

test_that("between results use one table with its note below", {
  shiny::testServer(myserver, {
    session$setInputs(example_dataset = "rosenthal_tbl31")
    session$flushReact()
    invisible(output$variables)
    session$setInputs(
      dv_name = "dv", between_name = "between",
      within_name = "", id_name = ""
    )
    session$flushReact()

    result_html <- output$table_region
    expect_match(result_html, 'id="cofad-f-table"', fixed = TRUE)
    expect_match(
      result_html,
      "The contrast is a one-degree-of-freedom component", fixed = TRUE
    )
    expect_false(grepl('id="cofad-effect-table"', result_html, fixed = TRUE))
    expect_false(grepl(
      "Effect sizes and explained proportions", result_html, fixed = TRUE
    ))
  })
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
