test_that("cs_data rejects invalid input", {
  expect_error(cs_get_data(), "File must be specified.")

  f <- system.file("extdata/cs_data.csv", package="NHMDE")
  expect_silent(cs_get_data(file=f, project=NULL))

  expect_error(cs_get_data(f, project="testthat"), "No function to get data from project.")

  expect_equal(dim(cs_get_data(file=f, project=NULL)), c(2,11))

  f <- system.file("extdata/cs_data_malformed.csv", package="NHMDE")
  expect_error(cs_get_data(file=f, project=NULL), "Data does not have expected columns.")

})
