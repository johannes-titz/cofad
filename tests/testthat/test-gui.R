library(shinytest2)
modify_hot_table <- function(row, col, value, table) {
  paste0(
    "var $table = HTMLWidgets.getInstance(", table, ").hot;",
    "$table.setDataAtCell(", row - 1, ", ", col - 1, ", ", value, ");"
  )
}

test_that("gui between works", {
  skip_on_cran()
  skip_on_ci()
  start <- run_app()
  app <- AppDriver$new(start, name = "between")
  app$upload_file(datafile = "sedlmeier_p525.csv") # change path
  app$set_inputs("between_name" = "between")

  app$run_js(modify_hot_table(1, 2, 60, "hot_lambda_between"))
  app$wait_for_idle()
  app$run_js(modify_hot_table(2, 2, -40, "hot_lambda_between"))
  app$wait_for_idle()
  app$run_js(modify_hot_table(3, 2, -20, "hot_lambda_between"))
  app$wait_for_idle()

  tbl <- app$get_value(output = "table_region")

  expect_true(grepl("6.519", tbl))
  expect_true(grepl("0.59", tbl))
})

test_that("gui within works", {
  skip_on_cran()
  app <- AppDriver$new(run_app())
  app$upload_file(datafile = "sedlmeier_p537.csv") # change path

  app$set_inputs("within_name" = "music", "id_name" = "participant")
  app$run_js(modify_hot_table(1, 2, -0.75, "hot_lambda_within"))
  app$wait_for_idle()
  app$run_js(modify_hot_table(2, 2, -0.75, "hot_lambda_within"))
  app$wait_for_idle()
  app$run_js(modify_hot_table(3, 2, 0.25, "hot_lambda_within"))
  app$wait_for_idle()
  app$run_js(modify_hot_table(4, 2, 1.25, "hot_lambda_within"))
  app$wait_for_idle()

  tbl <- app$get_value(output = "table_region")
  # sedlmeier 5.27
  expect_true(grepl("5.269", tbl))
})

test_that("gui mixed works", {
  skip_on_cran()
  app <- AppDriver$new(run_app())
  app$upload_file(datafile = "rosenthal_tbl53.csv")
  app$set_inputs("within_name" = "within", "id_name" = "id",
                 "between_name" = "between")
  app$run_js(modify_hot_table(1, 2, 0, "hot_lambda_between"))
  app$wait_for_idle()
  app$run_js(modify_hot_table(2, 2, 1, "hot_lambda_between"))
  app$wait_for_idle()
  app$run_js(modify_hot_table(3, 2, -1, "hot_lambda_between"))
  app$wait_for_idle()
  # within is linear

  tbl <- app$get_value(output = "table_region")
  # original vaue: 20.19
  expect_true(grepl("20.211", tbl))
  expect_true(grepl("0.871", tbl))
})
