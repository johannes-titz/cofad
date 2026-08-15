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

test_that("variance hover identifies components and their denominator", {
  result <- variance_display_result()
  total_contrast <- cofad:::variance_partition_hover(
    result, list(x = 0.2, y = 3)
  )
  between_other <- cofad:::variance_partition_hover(
    result, list(x = 0.99, y = 2)
  )

  expect_identical(total_contrast$component, "Contrast")
  expect_identical(total_contrast$metric, "r_es2")
  expect_equal(
    total_contrast$share,
    unname(result$sig["ss_kontrast"] / result$sig["ss_total"])
  )
  expect_identical(between_other$component, "Other between-group")
  expect_identical(between_other$metric, "r_alerting2")
  expect_null(cofad:::variance_partition_hover(result, NULL))
  expect_null(cofad:::variance_partition_hover(result, list(x = 0.2, y = 4)))
  expect_null(cofad:::variance_partition_hover(result, list(x = 1.2, y = 3)))

  zero <- result
  zero$sig[c("ss_kontrast", "ss_between", "ss_within", "ss_total")] <- 0
  expect_true(all(is.na(cofad:::variance_partition_data(zero)$shares)))
  expect_null(cofad:::variance_partition_hover(zero, list(x = 0.2, y = 3)))
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

test_that("Shiny hover details expose the exact SS calculation", {
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
      id_name = "id", variance_hover = list(x = 0.1, y = 3)
    )
    session$flushReact()

    rendered <- output$variance_partition_details$html
    expect_match(rendered, "SS =")
    expect_match(rendered, "<sub>es</sub>")
  })
})
