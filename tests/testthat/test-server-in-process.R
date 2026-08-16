uploaded_file <- function(path) {
  path <- normalizePath(path)
  list(
    name = basename(path), size = unname(file.info(path)$size),
    type = "text/csv", datapath = path
  )
}

test_that("between Shiny workflow runs in-process", {
  file <- uploaded_file(test_path("sedlmeier_p525.csv"))
  shiny::testServer(myserver, {
    session$setInputs(datafile = file)
    session$flushReact()
    expect_identical(reactive$design_suggestion$design, "between")
    expect_identical(unname(reactive$model_spec),
                     c("lsg", "between", "", ""))

    invisible(output$variables)
    session$setInputs(
      dv_name = "lsg", between_name = "between", within_name = "",
      id_name = ""
    )
    session$flushReact()
    expect_match(output$hot_model, "Dependent variable")
    expect_false(grepl("use_between_contrast", output$variables$html,
                       fixed = TRUE))
    expect_match(output$hot_lambda_between, '"n":5')
    expect_match(output$hot_lambda_between, '"renderAllColumns":true')
    expect_match(output$table_region, "Variance decomposition")
    expect_match(output$table_region, "Residual between groups")
    expect_match(output$citation_region$html, "Henninger")
  })
})

test_that("within Shiny workflow runs in-process", {
  file <- uploaded_file(test_path("sedlmeier_p537.csv"))
  shiny::testServer(myserver, {
    session$setInputs(datafile = file)
    session$flushReact()
    invisible(output$variables)
    session$setInputs(
      dv_name = "reading_test", between_name = "", within_name = "music",
      id_name = "participant"
    )
    session$flushReact()
    expect_identical(reactive$design_suggestion$design, "within")
    expect_false(grepl("use_within_contrast", output$variables$html,
                       fixed = TRUE))
    expect_match(output$hot_lambda_within, "without music")
    expect_match(output$table_region, "within-subjects contrast")
    expect_match(output$table_region, "Variance decomposition")
    expect_match(output$table_region, 'id="cofad-within-f-table"', fixed = TRUE)
    expect_match(output$table_region, "partial eta squared", fixed = TRUE)
    expect_false(grepl(
      'id="cofad-effect-table"', output$table_region, fixed = TRUE
    ))
    expect_false(grepl(
      "different metrics", output$table_region, fixed = TRUE
    ))
    expect_match(output$r_code_region$html, "calc_contrast", fixed = TRUE)
    expect_match(output$r_code_region$html, "data = dat", fixed = TRUE)
    expect_match(output$r_code_region$html, "within_score", fixed = TRUE)
    expect_match(output$variance_partition, "Contrast-related SS", fixed = TRUE)
  })
})

test_that("mixed Shiny workflow runs in-process", {
  file <- uploaded_file(test_path("rosenthal_tbl53.csv"))
  shiny::testServer(myserver, {
    session$setInputs(datafile = file)
    session$flushReact()
    invisible(output$variables)
    session$setInputs(
      dv_name = "dv", between_name = "between", within_name = "within",
      id_name = "id"
    )
    session$flushReact()
    expect_identical(reactive$design_suggestion$design, "mixed")
    expect_identical(reactive$mixed_effect, "interaction")
    expect_match(output$variables$html, "Mixed-design contrast to test",
                 fixed = TRUE)
    expect_match(output$variables$html, "Between × within contrast",
                 fixed = TRUE)
    expect_false(grepl("use_between_contrast", output$variables$html,
                       fixed = TRUE))
    expect_false(grepl("use_within_contrast", output$variables$html,
                       fixed = TRUE))
    expect_s3_class(analysis(), "cofad_mx")
    expect_match(output$table_region, "within-contrast L values")
    expect_match(output$table_region, "Partition of total variation")

    session$setInputs(mixed_effect = "within")
    session$flushReact()
    expect_s3_class(analysis(), "cofad_wi")
    expect_false(grepl("lambda_between = lambda_between",
                       output$r_code_region$html, fixed = TRUE))

    session$setInputs(mixed_effect = "interaction")
    session$flushReact()
    expect_s3_class(analysis(), "cofad_mx")
    expect_match(output$r_code_region$html,
                 "lambda_between = lambda_between", fixed = TRUE)
  })
})
