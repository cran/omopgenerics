test_that("test codelist with details", {
  # single codelist
  codes <- list("disease" = dplyr::tibble(
    concept_id = c(1, 2, 3),
    other_info = c("a", "b", "c")
  ))

  expect_no_error(codes <- newCodelistWithDetails(codes))
  expect_true(inherits(codes, "codelist_with_details"))
  expect_no_error(print(codes))


  # multiple codelists
  codes <- list(
    "disease X" = dplyr::tibble(
      concept_id = c(1, 2, 3),
      other_info = c("a", "b", "c")
    ),
    "disease Y" = dplyr::tibble(
      concept_id = c(4, 5),
      other_info = c("d", "e")
    )
  )
  expect_no_error(codes <- newCodelistWithDetails(codes))
  expect_true(inherits(codes, "codelist_with_details"))
  expect_no_error(print(codes))

  expect_no_error(emptyCodelistWithDetails())

  # expected errors
  codes <- list("disease" = c(1, 2, 3))
  expect_error(codes <- newCodelistWithDetails(codes))

  codes <- list("disease" = dplyr::tibble(other_info = c("a", "b", "c")))
  expect_error(codes <- newCodelistWithDetails(codes))

  expect_error(newCodelistWithDetails(list(dplyr::tibble(concept_id = c(1, 2, 3)))))
  # cannot be mixed
  codes <- list(
    "disease X" = dplyr::tibble(
      concept_id = c(1, 2, 3),
      other_info = c("a", "b", "c")
    ),
    "disease Y" = c(4, 5)
  )
  expect_error(codes <- newCodelistWithDetails(codes))


  codes <- list("disease" = c(1, 2, 3))
  expect_error(codes <- newCodelistWithDetails(codes))

  expect_no_error(emptyCodelistWithDetails())

  # bind codelists
  x <- newCodelistWithDetails(list(
    "disease X" = dplyr::tibble(
      concept_id = c(1, 2, 3),
      other_info = c("a", "b", "c")
    ),
    "disease Y" = dplyr::tibble(
      concept_id = c(6, 55, 69, 12),
      col = c("-", "A", "hj", "-")
    )
  ))
  y <- newCodelistWithDetails(list(
    "disease Z" = dplyr::tibble(
      concept_id = c(13, 23),
      other_info = c("a", "b")
    )
  ))
  z <- newCodelistWithDetails(list(
    "disease Y" = dplyr::tibble(
      concept_id = c(6, 5, 9, 45, 67)
    )
  ))
  expect_no_error(res <- c(x, y))
  expect_true(all(c("disease X", "disease Y", "disease Z") %in% names(res)))
  expect_identical(c(x, y), bind(x, y))
  expect_identical(c(x, emptyCodelistWithDetails()), x)
  x1 <- x
  names(x1) <- c("disease X", "disease Y_1")
  z1 <- z
  names(z1) <- c("disease Y_2")
  expect_warning(expect_identical(c(x, z), c(x1, z1)))
  expect_identical(c(x, x), x)

  expect_true(inherits(res[c("disease X", "disease Z")], "codelist_with_details"))

  expect_equal(
    dplyr::as_tibble(x),
    x |>
      unclass() |>
      dplyr::bind_rows(.id = "codelist_with_details_name")
  )

  expect_identical(x, x |> dplyr::as_tibble() |> newCodelistWithDetails())
})
