context("readMsp")

test_that("readMsp correctly imports NIST formatted data", {
  dataPath <- system.file("extdata", "NIST_Format.msp", package="mspReader", mustWork = TRUE)
  expect_true(file.exists(dataPath))

  dat <- readMsp(path = dataPath, commentType = "NIST")

  expect_length(dat, 2)
  expect_length(dat$data, 4)
  expect_length(dat$info, 7)
})

test_that("readMsp correctly imports MoNA formatted data", {
  dataPath <- system.file("extdata", "MoNA_Format.msp", package="mspReader", mustWork = TRUE)
  expect_true(file.exists(dataPath))

  dat <- readMsp(path = dataPath, commentType = "MoNA")

  expect_length(dat, 2)
  expect_length(dat$data, 3)
  expect_length(dat$info, 13)
})
