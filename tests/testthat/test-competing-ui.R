test_that("competing contrast vectors are standardized before subtraction", {
  favored <- c(KT = -2, JT = 3, MT = -1)
  rival <- c(KT = -2, JT = 1, MT = 1)

  expect_equal(
    cofad_competing_lambda(favored, rival),
    lambda_diff(favored, rival)
  )
  expect_error(
    cofad_competing_lambda(favored, favored),
    "perfectly identical"
  )
  expect_error(
    cofad_competing_lambda(favored, c(KT = 0, JT = 0, MT = 0)),
    "two different values"
  )
})

test_that("between competing mode adds and removes the rival column", {
  data("sedlmeier_p525", package = "cofad")
  shiny::testServer(myserver, {
    session$setInputs(example_dataset = "sedlmeier_p525")
    session$flushReact()
    invisible(output$variables)
    session$setInputs(
      dv_name = "lsg", between_name = "between",
      within_name = "", id_name = "", compare_competing = FALSE
    )
    session$flushReact()

    expect_match(output$variables$html, "compare_competing", fixed = TRUE)
    expect_match(output$variables$html, "favored minus", fixed = TRUE)
    expect_match(output$variables$html, "initial rival reverses", fixed = TRUE)
    expect_false(grepl("rival", output$hot_lambda_between, fixed = TRUE))

    favored <- c(JT = 3, KT = -2, MT = -1)
    rival <- c(JT = 1, KT = -2, MT = 1)
    reactive$lambda_between <- favored
    reactive$lambda_between_rival <- rival
    session$setInputs(compare_competing = TRUE)
    session$flushReact()

    expected <- calc_contrast(
      dv = sedlmeier_p525$lsg,
      between = sedlmeier_p525$between,
      lambda_between = cofad_competing_lambda(favored, rival)
    )
    expect_match(output$hot_lambda_between, "favored", fixed = TRUE)
    expect_match(output$hot_lambda_between, "rival", fixed = TRUE)
    expect_equal(analysis()$sig, expected$sig)
    expect_match(
      output$table_region,
      "z-standardizing the favored and rival weights", fixed = TRUE
    )

    session$setInputs(compare_competing = FALSE)
    session$flushReact()
    single <- calc_contrast(
      dv = sedlmeier_p525$lsg,
      between = sedlmeier_p525$between,
      lambda_between = favored
    )
    expect_false(grepl("rival", output$hot_lambda_between, fixed = TRUE))
    expect_equal(analysis()$sig, single$sig)
  })
})

test_that("within competing mode uses the two within contrast columns", {
  data("sedlmeier_p537", package = "cofad")
  shiny::testServer(myserver, {
    session$setInputs(example_dataset = "sedlmeier_p537")
    session$flushReact()
    invisible(output$variables)
    session$setInputs(
      dv_name = "reading_test", between_name = "", within_name = "music",
      id_name = "participant", within_score = "L", compare_competing = TRUE
    )
    session$flushReact()

    favored <- c(
      `without music` = 3, `white noise` = -1, classic = -1, jazz = -1
    )
    rival <- c(
      `without music` = 1.25, `white noise` = 0.25,
      classic = -0.75, jazz = -0.75
    )
    reactive$lambda_within <- favored
    reactive$lambda_within_rival <- rival
    session$flushReact()

    expected <- calc_contrast(
      dv = sedlmeier_p537$reading_test,
      within = sedlmeier_p537$music,
      lambda_within = cofad_competing_lambda(favored, rival),
      id = sedlmeier_p537$participant
    )
    expect_match(output$hot_lambda_within, "rival", fixed = TRUE)
    expect_equal(analysis()$sig, expected$sig)

    session$setInputs(compare_competing = FALSE)
    session$flushReact()
    expect_false(grepl("rival", output$hot_lambda_within, fixed = TRUE))
  })
})

test_that("mixed competing mode activates both contrast tables", {
  shiny::testServer(myserver, {
    session$setInputs(example_dataset = "rosenthal_tbl53")
    session$flushReact()
    invisible(output$variables)
    session$setInputs(
      dv_name = "dv", between_name = "between", within_name = "within",
      id_name = "id", within_score = "L", compare_competing = TRUE
    )
    session$flushReact()

    expect_match(output$hot_lambda_between, "rival", fixed = TRUE)
    expect_match(output$hot_lambda_within, "rival", fixed = TRUE)
    expect_s3_class(analysis(), "cofad_mx")

    session$setInputs(compare_competing = FALSE)
    session$flushReact()
    expect_false(grepl("rival", output$hot_lambda_between, fixed = TRUE))
    expect_false(grepl("rival", output$hot_lambda_within, fixed = TRUE))
  })
})
