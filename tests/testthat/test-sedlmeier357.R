context("shiny test for sedlmeier p. 357 data")
library(testthat)

test_that("furr can be loaded, dv, iv and lambdas specified and the result is
          correct", {
  skip_on_cran()
  local_edition(3)
  csv_file <- c()
  csv_file$datapath <- file.path("sedlmeier537.csv")
  csv_file$name <- "sedlmeier537.csv"
  testServer(myserver, {
    session$setInputs(datafile = csv_file)
    session$setInputs(sort_dv_name = "reading_test",
                      sort_within_name = "music",
                      sort_id_name = "participant")
    # this is somewhat hacky to generate the proper input for rhandsontable
    res <- readRDS("sedlmeier537.csv_hot_lambda_wi.RData")
    session$setInputs(hot_lambda_wi = res)
    expect_true(grepl("5.875", session$output$table_region))
    expect_snapshot(session$output$table_region)
  })
})
