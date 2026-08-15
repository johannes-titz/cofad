variance_display_result <- function() {
  data("rosenthal_tbl31", package = "cofad")
  calc_contrast(
    dv = rosenthal_tbl31$dv,
    between = rosenthal_tbl31$between,
    lambda_between = c(A = -3, B = -1, C = 1, D = 3)
  )
}

test_that("F table includes the within-group SS share and eta tooltips", {
  result <- variance_display_result()
  table <- cofad:::detailed_f_table(result)
  expected <- unname(result$sig["ss_within"] / result$sig["ss_total"])

  expect_equal(as.numeric(table$eta2[[4]]), expected, tolerance = 0.001)
  expect_match(attr(table, "header_tooltips")[["eta2"]], "total sum")
  expect_match(
    attr(table, "cell_tooltips")$eta2[[2]], "equivalent to r_es squared"
  )
  expect_match(
    attr(table, "cell_tooltips")$eta2[[4]], "descriptive variance share"
  )

  html <- as.character(cofad:::cofad_html_table(
    table, right_align = c("SS", "df", "MS", "F", "p", "eta2")
  ))
  expect_match(html, "Ordinary eta squared", fixed = TRUE)
  expect_match(html, "Within-group/error SS share", fixed = TRUE)
  expect_match(html, 'class="cofad-number cofad-tooltip"', fixed = TRUE)
})

test_that("variance rows use the three contrast-effect denominators", {
  result <- variance_display_result()
  partition <- cofad:::variance_partition_data(result)

  expect_equal(
    unname(rowSums(partition$shares)), rep(1, 3), tolerance = 1e-12
  )
  expect_equal(
    unname(partition$metrics),
    unname(result$effects[c("r_effectsize", "r_alerting", "r_contrast")]^2),
    tolerance = 1e-12
  )
  expect_equal(
    partition$shares["total", ],
    partition$components / unname(result$sig["ss_total"]),
    tolerance = 1e-12
  )
})

test_that("variance data handle a degenerate zero-variation result", {
  result <- variance_display_result()
  zero <- result
  zero$sig[c("ss_kontrast", "ss_between", "ss_within", "ss_total")] <- 0
  expect_true(all(is.na(cofad:::variance_partition_data(zero)$shares)))
})

test_that("Plotly partition puts component labels above interactive bars", {
  result <- variance_display_result()
  figure <- cofad:::plotly_variance_partition(result)
  built <- plotly::plotly_build(figure)

  expect_s3_class(figure, "plotly")
  expect_identical(
    vapply(built$x$data, `[[`, character(1), "name"),
    c("Contrast", "Other between-group", "Within-group/error")
  )
  expect_identical(built$x$layout$legend$orientation, "h")
  expect_identical(built$x$layout$legend$traceorder, "normal")
  expect_gt(built$x$layout$legend$y, 1)
  expect_identical(built$x$layout$yaxis$ticksuffix, "\u00a0\u00a0")
  expect_match(built$x$data[[1]]$hovertext[[1]], "SS =", fixed = TRUE)
  expect_match(built$x$data[[1]]$hovertext[[1]], "289 / 455", fixed = TRUE)
  expect_match(
    paste(vapply(built$x$layout$annotations, `[[`, character(1), "text"),
          collapse = " "),
    "<sub>alerting</sub>", fixed = TRUE
  )
  expect_equal(
    built$x$data[[1]]$x[[1]],
    unname(result$sig["ss_kontrast"] / result$sig["ss_total"])
  )
})

test_that("mixed partitions label derived score variation", {
  data("rosenthal_tbl53", package = "cofad")
  result <- calc_contrast(
    dv = rosenthal_tbl53$dv,
    between = rosenthal_tbl53$between,
    lambda_between = c(age8 = -1, age10 = 0, age12 = 1),
    within = rosenthal_tbl53$within,
    lambda_within = c(`1` = -3, `2` = -1, `3` = 1, `4` = 3),
    id = rosenthal_tbl53$id
  )
  partition <- cofad:::variance_partition_data(result)

  expect_match(names(partition$components)[[1]], "Planned mixed contrast")
  expect_match(names(partition$components)[[3]], "score variation")
})

test_that("within partitions show contrast and participant-error variation", {
  data("sedlmeier_p537", package = "cofad")
  result <- calc_contrast(
    dv = reading_test, within = music, id = participant,
    lambda_within = c(
      "without music" = 1.25, "white noise" = 0.25,
      classic = -0.75, jazz = -0.75
    ),
    data = sedlmeier_p537
  )
  partition <- variance_partition_data(result)
  figure <- plotly_variance_partition(result)
  built <- plotly::plotly_build(figure)

  expect_equal(unname(rowSums(partition$shares)), 1, tolerance = 1e-12)
  expect_equal(
    partition$metrics[["eta2"]], partition$metrics[["partial_eta2"]]
  )
  expect_identical(
    vapply(built$x$data, `[[`, character(1), "name"),
    c("Contrast", "Contrast × participants/error")
  )
  expect_match(
    built$x$layout$annotations[[1]]$text,
    "<i>η</i><sup>2</sup>", fixed = TRUE
  )
  expect_false(grepl("&eta;", built$x$layout$annotations[[1]]$text, fixed = TRUE))
  expect_match(
    built$x$layout$annotations[[1]]$text,
    "<sub>contrast</sub><sup>2</sup>", fixed = TRUE
  )
})

test_that("Shiny exposes the Plotly partition output", {
  path <- test_path("rosenthal_tbl53.csv")
  upload <- list(
    name = basename(path), datapath = path, size = file.info(path)$size,
    type = "text/csv"
  )

  shiny::testServer(cofad:::myserver, {
    session$setInputs(datafile = upload)
    session$flushReact()
    invisible(output$variables)
    session$setInputs(
      dv_name = "dv", between_name = "between", within_name = "within",
      id_name = "id"
    )
    session$flushReact()

    rendered <- output$variance_partition
    expect_type(rendered, "character")
    expect_match(rendered, "Other between-group", fixed = TRUE)
    expect_match(rendered, "SS =", fixed = TRUE)
  })
})
