test_that("test omopDataFolder", {
  Sys.setenv("OMOP_DATA_FOLDER" = "")
  expect_no_error(pth <- omopDataFolder())
  expect_identical(pth, file.path(tempdir(), "OMOP_DATA_FOLDER"))
  newPth <- file.path(tempdir(), "OMOP_DATA")
  expect_no_error(pth <- omopDataFolder(path = newPth))
  expect_identical(pth, newPth)
  expect_identical(pth, omopDataFolder())
})
