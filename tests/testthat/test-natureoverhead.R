test_that("cs_data rejects invalid input", {
  f <- system.file("extdata/no_data.csv", package="NHMDE")
  expect_silent(cs_get_data(f, project="natureoverheard"))
})
