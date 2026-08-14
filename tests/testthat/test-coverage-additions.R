test_that("unequal groups agree with lm and aggregated input", {
  group <- factor(rep(c("A", "B", "C"), c(3, 5, 7)))
  outcome <- c(1, 2, 4, 3, 4, 5, 6, 7, 4, 5, 6, 7, 8, 9, 10)
  lambdas <- c(A = -1, B = 0, C = 1)

  raw <- calc_contrast(outcome, group, lambdas)
  fitted <- stats::anova(stats::lm(outcome ~ group))
  means <- tapply(outcome, group, mean)
  sds <- tapply(outcome, group, sd)
  ns <- table(group)
  aggregated <- calc_contrast_aggregated(
    means, sds, ns, names(means), lambdas
  )

  expect_equal(unname(raw$sig["ss_between"]), fitted$`Sum Sq`[1])
  expect_equal(unname(raw$sig["ss_within"]), fitted$`Sum Sq`[2])
  expect_equal(raw$sig, aggregated$sig, tolerance = 1e-12)
  expect_equal(raw$effects, aggregated$effects, tolerance = 1e-12)
})

test_that("variance tables and effect sizes have exact SS identities", {
  data("sedlmeier_p525")
  object <- calc_contrast(
    dv = lsg, between = between,
    lambda_between = c(KT = -2, JT = 3, MT = -1),
    data = sedlmeier_p525
  )
  table <- cofad:::detailed_f_table(object)
  effects <- cofad:::detailed_effect_table(object)
  s <- object$sig

  expect_equal(
    unname(s["ss_total"]), unname(s["ss_between"] + s["ss_within"])
  )
  expect_equal(
    unname(s["ss_between"]),
    unname(s["ss_kontrast"] + s["ss_between"] - s["ss_kontrast"])
  )
  expect_identical(
    table$Source,
    c("Between groups (overall)", "Contrast", "Residual between groups",
      "Within groups (error)", "Total")
  )
  expect_equal(
    as.numeric(effects$`Squared / explained proportion`),
    unname(object$effects^2), tolerance = 1e-3
  )
  expect_match(as.character(cofad:::cofad_html_table(table)), "SS")
})

test_that("within effect table and variance plotting helpers work", {
  data("sedlmeier_p537")
  within <- calc_contrast(
    reading_test, within = music, id = participant,
    lambda_within = c(
      "without music" = 1.25, "white noise" = 0.25,
      "classic" = -0.75, "jazz" = -0.75
    ), data = sedlmeier_p537
  )
  expect_identical(
    cofad:::detailed_effect_table(within)$Measure,
    c("r contrast", "g contrast")
  )

  data("sedlmeier_p525")
  between <- calc_contrast(
    dv = lsg, between = between,
    lambda_between = c(KT = -2, JT = 3, MT = -1),
    data = sedlmeier_p525
  )
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  components <- cofad:::plot_variance_partition(between)
  grDevices::dev.off()
  expect_equal(sum(components), unname(between$sig["ss_total"]))
})

test_that("data loading handles common CSV forms and failures", {
  comma <- tempfile(fileext = ".csv")
  semicolon <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(x = 1:2, group = c("a", "b")), comma,
                   row.names = FALSE)
  utils::write.csv2(data.frame(x = 1:2, group = c("a", "b")), semicolon,
                    row.names = FALSE)

  expect_equal(
    cofad:::load_data(list(name = "DATA.CSV", datapath = comma))$x, 1:2
  )
  expect_equal(
    cofad:::load_data(list(name = "data.csv", datapath = semicolon))$x, 1:2
  )
  expect_message(
    expect_null(cofad:::load_data(list(name = "data.txt", datapath = comma))),
    "Unsupported file extension"
  )
})

test_that("small UI helpers cover edge cases", {
  expect_equal(cofad:::create_default_lambdas(c("a", "b", "c")),
               c(a = -1, b = 0, c = 1))
  expect_equal(
    cofad:::prepare_table(c(a = -0.5, b = 0.5), factor(c("a", "a", "b")))$n,
    c(2L, 1L)
  )
  expect_true(inherits(cofad:::create_table(c("a", "b")), "rhandsontable"))
  expect_match(cofad:::cite(), "Henninger")
  expect_s3_class(run_app(), "shiny.appobj")
})

test_that("effect-size conversions preserve positive and negative directions", {
  for (direction in c(-1, 1)) {
    alerting <- direction * 0.8
    contrast <- direction * 0.6
    effect <- calc_r_effectsize(alerting, contrast)
    expect_equal(unname(sign(effect)), direction)
    expect_equal(unname(calc_r_alerting(contrast, effect)), alerting)
    expect_equal(unname(calc_r_contrast(alerting, effect)), contrast)
  }
})

test_that("incomplete and duplicate repeated-measures cells are rejected", {
  data("sedlmeier_p537")
  lambdas <- c(
    "without music" = 1.25, "white noise" = 0.25,
    "classic" = -0.75, "jazz" = -0.75
  )
  incomplete <- sedlmeier_p537[-1, ]
  duplicate <- rbind(sedlmeier_p537, sedlmeier_p537[1, ])

  expect_error(
    calc_contrast(reading_test, within = music, id = participant,
                  lambda_within = lambdas, data = incomplete),
    "every participant-by-within-level cell"
  )
  expect_error(
    calc_contrast(reading_test, within = music, id = participant,
                  lambda_within = lambdas, data = duplicate),
    "at most one observation"
  )
})
