test_that("every app example has valid source-defined roles and weights", {
  info <- cofad_example_info()

  for (name in info$name) {
    data <- load_cofad_example(name)
    spec <- cofad_example_spec(name)

    expect_false(is.null(spec), info = name)
    expect_named(
      spec$roles,
      c("dv_name", "between_name", "within_name", "id_name"),
      info = name
    )
    used_roles <- unname(spec$roles[nzchar(spec$roles)])
    expect_true(all(used_roles %in% names(data)), info = name)

    for (kind in c("between", "within")) {
      weights <- spec[[kind]]
      rival <- spec[[paste0(kind, "_rival")]]
      role <- spec$roles[[paste0(kind, "_name")]]
      if (!is.null(weights)) {
        expected <- levels(as.factor(data[[role]]))
        expect_setequal(names(weights), expected)
        expect_equal(sum(weights), 0, tolerance = 1e-12,
                     info = paste(name, kind))
      }
      if (!is.null(rival)) {
        expect_setequal(names(rival), names(weights))
        expect_equal(sum(rival), 0, tolerance = 1e-12,
                     info = paste(name, kind, "rival"))
      }
    }
    expect_identical(
      spec$competing,
      !is.null(spec$between_rival) || !is.null(spec$within_rival),
      info = name
    )
  }
})

test_that("all source-defined example analyses run", {
  for (name in cofad_example_info()$name) {
    data <- load_cofad_example(name)
    spec <- cofad_example_spec(name)
    role <- spec$roles
    between_weights <- spec$between
    within_weights <- spec$within
    if (!is.null(spec$between_rival)) {
      between_weights <- cofad_competing_lambda(
        between_weights, spec$between_rival
      )
    }
    if (!is.null(spec$within_rival)) {
      within_weights <- cofad_competing_lambda(
        within_weights, spec$within_rival
      )
    }

    result <- calc_contrast(
      dv = data[[role[["dv_name"]]]],
      between = if (nzchar(role[["between_name"]])) {
        as.factor(data[[role[["between_name"]]]])
      },
      lambda_between = between_weights,
      id = if (nzchar(role[["id_name"]])) {
        as.factor(data[[role[["id_name"]]]])
      },
      within = if (nzchar(role[["within_name"]])) {
        as.factor(data[[role[["within_name"]]]])
      },
      lambda_within = within_weights
    )
    expect_true(inherits(result, c("cofad_bw", "cofad_wi", "cofad_mx")))
  }
})

test_that("example selection installs its documented contrasts", {
  shiny::testServer(myserver, {
    session$setInputs(
      example_dataset = "rosenthal_tbl31", dv_name = "dv",
      between_name = "between", within_name = "", id_name = ""
    )
    session$flushReact()
    session$setInputs(example_dataset = "maraver")
    session$flushReact()
    expect_identical(reactive$model_spec, cofad_example_spec("maraver")$roles)
    expect_equal(
      reactive$lambda_between,
      cofad_example_spec("maraver")$between
    )
    expect_equal(
      reactive$lambda_between_rival,
      cofad_example_spec("maraver")$between_rival
    )
    expect_true(reactive$compare_competing)
    expect_match(output$table_region, "two competing contrasts", fixed = TRUE)

    session$setInputs(example_dataset = "rosenthal_tbl59")
    session$flushReact()
    expect_identical(reactive$mixed_effect, "within")
    expect_equal(
      reactive$lambda_between,
      c(placebo = -0.5, psychotherapy = 0.5)
    )
    expect_equal(
      reactive$lambda_within,
      c(treatment = 1, placebo = -1)
    )
    expect_false(reactive$compare_competing)
    expect_s3_class(analysis(), "cofad_wi")
    expect_match(output$table_region, "t(4) = 2.449", fixed = TRUE)

    session$setInputs(example_dataset = "rosenthal_tbl68_mixed")
    session$flushReact()
    expect_identical(reactive$mixed_effect, "within")
    expect_equal(reactive$lambda_between, c(girl = -0.5, boy = 0.5))
    expect_true(reactive$compare_competing)
    expect_equal(
      reactive$lambda_within,
      c(t1 = -1, t2 = 0, t3 = 0, t4 = 1)
    )
    expect_equal(
      reactive$lambda_within_rival,
      c(t1 = -3, t2 = -1, t3 = 1, t4 = 3)
    )
    expect_s3_class(analysis(), "cofad_wi")
    expect_match(output$table_region, "t(6) = 5.189", fixed = TRUE)
  })
})
