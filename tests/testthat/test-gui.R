context("gui examples")
library(testthat)

test_that("gui works for standard cases", {
  skip_on_ci()
  # run bash script
  system("bash test3.sh", wait = TRUE)

  furr <- readLines("furr.html")
  sedlmeier <- readLines("sedlmeier537.html")
  rosenthal <- readLines("rosenthal_tbl53.html")

  expect_true(any(grepl("6.154", furr)))
  expect_true(any(grepl("5.269", sedlmeier)))
  expect_true(any(grepl("20.211", rosenthal)))
})
