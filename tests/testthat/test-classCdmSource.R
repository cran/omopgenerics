test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("identical_type_insensitive works", {
  df1 <- data.frame(
    a = c(1, 2, 3),
    b = c("test1", "test2", "test3")
  )

  df2 <- data.frame(
    a = c(1.0, 2.0, 3),
    b = c("test1", "test2", "test3")
  )

  df3 <- data.frame(
    a = c(1.0, 2.0, 3),
    b = c("test", "test2", "test3")
  )

  df4 <- data.frame(
    a = c(1.0, 2.0),
    b = c("test", "test2")
  )

  df5 <- data.frame()

  df6 <- data.frame()

  df7 <- data.frame(
    a = c(1.1, 2.0, 3),
    b = c("test1", "test2", "test3")
  )

  result <- identical_type_insensitive(df1, df2)

  expect_true(result)

  result <- identical_type_insensitive(df2, df3)

  expect_false(result)

  result <- identical_type_insensitive(df3, df4)

  expect_false(result)

  result <- identical_type_insensitive(df4, df5)

  expect_false(result)

  result <- identical_type_insensitive(df5, df6)

  expect_true(result)

  result <- identical_type_insensitive(df1, df7)

  expect_false(result)


})
