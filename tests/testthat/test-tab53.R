context("shiny test for sedlmeier p. 357 data")
library(testthat)

test_that("furr can be loaded, dv, iv and lambdas specified and the result is
          correct", {
  skip_on_cran()
  local_edition(3)
  csv_file <- c()
  csv_file$datapath <- file.path("rosenthal_tbl53.csv")
  csv_file$name <- "rosenthal_tbl53.csv"
  testServer(myserver, {
    session$setInputs(datafile = csv_file)
    session$setInputs(sort_dv_name = "var",
                      sort_within_name = "within",
                      sort_id_name = "ID",
                      sort_between_name = "between")
    # this is somewhat hacky to generate the proper input for rhandsontable
    wi <- readRDS("rosenthal_tbl53.csv_hot_lambda_wi.RData")
    btw <- readRDS("rosenthal_tbl53.csv_hot_lambda_btw.RData")
    session$setInputs(hot_lambda_wi = wi, hot_lambda_btw = btw)
    expect_true(grepl("20.211", session$output$table_region))
    expect_snapshot(session$output$table_region)
  })
})
