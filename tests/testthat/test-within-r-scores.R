test_that("Rosenthal Table 5.4 distinguishes L magnitude and r pattern fit", {
  dat <- data.frame(
    id = factor(rep(c("A", "B"), each = 4)),
    within = factor(rep(1:4, 2)),
    dv = c(1, 8, 2, 9, 1, 2, 3, 4)
  )
  lambda <- c("1" = -3, "2" = -1, "3" = 1, "4" = 3)

  expect_equal(
    participant_within_scores(dat$dv, dat$within, lambda, dat$id, "L"),
    c(A = 18, B = 10)
  )
  expect_equal(
    round(participant_within_scores(
      dat$dv, dat$within, lambda, dat$id, "r"
    ), 2),
    c(A = .57, B = 1)
  )
})

test_that("Rosenthal Table 5.3 r scores reproduce the unrounded analysis", {
  data(rosenthal_tbl53)
  lambda_within <- c("1" = -3, "2" = -1, "3" = 1, "4" = 3)
  result <- calc_contrast(
    dv, between, c(age8 = -1, age10 = 0, age12 = 1),
    within, lambda_within, id = id, data = rosenthal_tbl53,
    within_score = "r"
  )

  expect_s3_class(result, "cofad_mx")
  expect_identical(result$within_score, "r")
  expect_equal(
    round(result$participant_scores, 2),
    c(`1` = .26, `2` = .45, `3` = .77, `4` = .67, `5` = .45,
      `6` = .67, `7` = .94, `8` = .92, `9` = .95)
  )
  expect_equal(unname(result$sig["f_contrast"]), 10.42270254, tolerance = 1e-7)
  expect_equal(unname(result$sig["p_contrast"]), .01794608, tolerance = 1e-7)
})

test_that("r score limitations are reported", {
  expect_warning(
    participant_within_scores(
      c(1, 2, 2, 1), factor(rep(1:2, 2)), c(`1` = -1, `2` = 1),
      factor(rep(1:2, each = 2)), "r"
    ),
    "only -1 or 1"
  )
  expect_error(
    suppressWarnings(participant_within_scores(
      c(2, 2, 1, 3), factor(rep(1:2, 2)), c(`1` = -1, `2` = 1),
      factor(rep(1:2, each = 2)), "r"
    )),
    "constant response"
  )
})

test_that("the app can switch a mixed analysis from L to r scores", {
  path <- test_path("rosenthal_tbl53.csv")
  file <- list(
    name = basename(path), size = unname(file.info(path)$size),
    type = "text/csv", datapath = normalizePath(path)
  )
  shiny::testServer(myserver, {
    session$setInputs(datafile = file)
    session$flushReact()
    invisible(output$variables)
    session$setInputs(
      dv_name = "dv", between_name = "between", within_name = "within",
      id_name = "id", within_score = "r"
    )
    session$flushReact()
    expect_match(output$table_region, "within-contrast r scores")
  })
})
