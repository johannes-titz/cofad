expected_examples <- data.frame(
  data = c(
    "akan", "furr_p4", "haans_within1by4", "maraver",
    "rosenthal_chap5_q2", "rosenthal_p141", "rosenthal_tbl31",
    "rosenthal_tbl53", "rosenthal_tbl59", "rosenthal_tbl68", "schwoebel",
    "sedlmeier_p525", "sedlmeier_p537", "testing_effect"
  ),
  design = c(
    "within", "between", "within", "between", "mixed", "mixed", "between",
    "mixed", "mixed", "within", "between", "between", "within", "between"
  ),
  dv = c(
    "contexts", "empathy", "value", "prop_recalled", "dv", "dv", "dv",
    "dv", "dv", "dv", "percent_recalled", "lsg", "reading_test", "recalled"
  ),
  between = c(
    "", "major", "", "condition", "between", "bw", "between", "between",
    "pt", "", "condition", "between", "", "condition"
  ),
  within = c(
    "condition", "", "name", "", "within", "med", "", "within", "med",
    "within", "", "", "music", ""
  ),
  id = c(
    "subject", "", "person", "", "id", "id", "", "id", "id", "id", "",
    "", "participant", ""
  ),
  stringsAsFactors = FALSE
)

test_that("all packaged examples receive the expected conservative suggestion", {
  for (i in seq_len(nrow(expected_examples))) {
    env <- new.env(parent = emptyenv())
    utils::data(
      list = expected_examples$data[i], package = "cofad", envir = env
    )
    result <- detect_design(env[[expected_examples$data[i]]])
    expect_identical(result$design, expected_examples$design[i],
                     info = expected_examples$data[i])
    expect_identical(result$dv_name, expected_examples$dv[i],
                     info = expected_examples$data[i])
    expect_identical(result$between_name, expected_examples$between[i],
                     info = expected_examples$data[i])
    expect_identical(result$within_name, expected_examples$within[i],
                     info = expected_examples$data[i])
    expect_identical(result$id_name, expected_examples$id[i],
                     info = expected_examples$data[i])
    expect_true(result$confidence >= 0.80, info = expected_examples$data[i])
  }
})

test_that("detector is invariant to row and column order", {
  data("rosenthal_tbl53")
  set.seed(1)
  shuffled <- rosenthal_tbl53[
    sample(seq_len(nrow(rosenthal_tbl53))),
    sample(names(rosenthal_tbl53))
  ]
  original <- detect_design(rosenthal_tbl53)
  reordered <- detect_design(shuffled)
  fields <- c("design", "dv_name", "between_name", "within_name", "id_name")
  expect_identical(original[fields], reordered[fields])
})

test_that("ambiguous data stay undetermined", {
  ambiguous <- data.frame(x = 1:10, y = stats::rnorm(10))
  result <- detect_design(ambiguous)
  expect_identical(result$design, "undetermined")
  expect_identical(result$id_name, "")
  expect_identical(result$between_name, "")
  expect_identical(result$within_name, "")
})
