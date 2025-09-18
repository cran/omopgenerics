test_that("test codelist works", {
  # single codelist
  codes <- list("disease" = c(1L, 2L, 3L))
  expect_no_error(codes <- newCodelist(codes))
  class <- codes |>
    newCodelist() |>
    class()
  expect_true(c("codelist") %in% class)
  expect_no_error(print(codes))

  # multiple codelists
  codes <- list(
    "disease X" = c(1L, 2L, 3L),
    "disease Y" = c(4L, 5L)
  )
  expect_no_error(codes <- newCodelist(codes))
  class <- codes |>
    newCodelist() |>
    class()
  expect_true(c("codelist") %in% class)
  expect_no_error(print(codes))


  expect_no_error(emptyCodelist())
  expect_true(inherits(emptyCodelist(), "codelist"))

  # expected errors
  expect_error(newCodelist(c(1L, 2L, 3L)))
  expect_error(newCodelist(list(c(1L, 2L, 3L))))

  codes <- list("disease" = dplyr::tibble(
    concept_id = c(1L, 2L, 3L),
    other_info = c("a", "b", "c")
  ))


  expect_error(codes <- newCodelist(codes))

  codes <- list(
    "disease X" = c(1L, NA, 3L),
    "disease Y" = c(4L, 5L)
  )
  expect_error(codes <- newCodelist(codes))

  codes <- list(
    "disease X" = c(1L, 2L, 3L),
    "disease Y" = as.character(c(4, 5))
  )
  expect_error(codes <- newCodelist(codes))

  # codelist with identical names
  codes_identical <- list(
    a = c(123L),
    a = c(123L)
  )

  expect_error(newCodelist(codes_identical))

  # bind codelists
  x <- newCodelist(list(x = c(1L, 2L, 3L), y = c(3L, 4L)))
  y <- newCodelist(list(a = 1L))
  z <- newCodelist(list(a = 2L))
  expect_no_error(res <- c(x, y))
  expect_true(all(c("x", "y", "a") %in% names(res)))
  expect_identical(c(x, y), bind(x, y))
  expect_identical(c(x, emptyCodelist()), x)
  expect_warning(expect_identical(c(y, z), newCodelist(list(a_1 = 1L, a_2 = 2L))))
  expect_identical(c(x, x), x)

  expect_true(inherits(res[c("x", "y")], "codelist"))

  expect_equal(
    dplyr::as_tibble(x),
    dplyr::tibble(codelist_name = "x", concept_id = x$x) |>
      dplyr::union_all(dplyr::tibble(codelist_name = "y", concept_id = x$y))
  )

  expect_identical(x, x |> dplyr::as_tibble() |> newCodelist())

  skip_if_not_installed("bit64")
  expect_warning(newCodelist(list("disease X" = bit64::as.integer64(c(4, 5)))))
})
