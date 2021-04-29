context("shiny test for furr data")
library(testthat)

test_that("furr can be loaded, dv, iv and lambdas specified and the result is
          correct", {
  skip_on_cran()
  local_edition(3)
  csv_file <- c()
  csv_file$datapath <- file.path("furr.csv")
  csv_file$name <- "furr.csv"
  testServer(myserver, {
    session$setInputs(datafile = csv_file)
    session$setInputs(sort_dv_name = "empathy",
                      sort_between_name = "major")
    # this is somewhat hacky to generate the proper input for rhandsontable
    res <- readRDS("hot_lambda_btw.RData")
    session$setInputs(hot_lambda_btw = res)
    expect_true(grepl("6.154", session$output$table_region))
    expect_snapshot(session$output$table_region)
  })
})
