library(testthat)
library(brpextravis)

#test_check("brpextravis")
test_that("File name",{
  filename <- make_filename(2013)
  expect_that(filename,equals("accident_2013.csv.bz2"))
})
