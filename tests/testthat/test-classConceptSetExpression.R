test_that("simple examples of concept set", {
  asthma_cs <- list(
    "asthma_narrow" = dplyr::tibble(
      "concept_id" = 1,
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    ),
    "asthma_broad" = dplyr::tibble(
      "concept_id" = c(1, 2),
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    )
  )
  expect_no_error(asthma_cs <- newConceptSetExpression(asthma_cs))
  expect_true(inherits(asthma_cs, "concept_set_expression"))
  expect_no_error(dplyr::as_tibble(asthma_cs))
  expect_identical(newConceptSetExpression(dplyr::as_tibble(asthma_cs)), asthma_cs)

  # no error if extra columns
  asthma_cs <- list(
    "asthma_narrow" = dplyr::tibble(
      "concept_id" = 1,
      "concept_name" = "asthma",
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    ),
    "asthma_broad" = dplyr::tibble(
      "concept_id" = c(1, 2),
      "concept_name" = "asthma",
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    )
  )

  expect_no_error(asthma_cs <- newConceptSetExpression(asthma_cs))

  # expected errors
  expect_error(newConceptSetExpression(list(
    "asthma_narrow" = dplyr::tibble(
      "concept_id" = 1,
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    ),
    dplyr::tibble( # no name
      "concept_id" = 2,
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    )
  )))

  expect_error(newConceptSetExpression(list(
    "asthma_narrow" = dplyr::tibble(
      "concept_id" = 1,
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    ),
    "asthma_broad" = dplyr::tibble(
      "concept_id" = "not a id",
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    )
  )))

  expect_error(newConceptSetExpression(list(
    "asthma_narrow" = dplyr::tibble(
      "concept_id" = 1,
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    ),
    "asthma_broad" = dplyr::tibble(
      "concept_id" = 2,
      "excluded" = "not logical",
      "descendants" = TRUE,
      "mapped" = FALSE
    )
  )))

  expect_error(newConceptSetExpression(list(
    "asthma_narrow" = dplyr::tibble(
      "concept_id" = 1,
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    ),
    "asthma_broad" = dplyr::tibble(
      "concept_id" = 2,
      "excluded" = TRUE,
      "descendants" = "not logical",
      "mapped" = FALSE
    )
  )))

  expect_error(newConceptSetExpression(list(
    "asthma_narrow" = dplyr::tibble(
      "concept_id" = 1,
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    ),
    "asthma_broad" = dplyr::tibble(
      "concept_id" = 2,
      "excluded" = TRUE,
      "descendants" = TRUE,
      "mapped" = "not logical"
    )
  )))

  expect_error(newConceptSetExpression(list(
    "diabetes" = dplyr::tibble(
      "concept_id" = c(201820, NA),
      "excluded" = c(FALSE, FALSE),
      "descendants" = c(TRUE, FALSE),
      "mapped" = c(FALSE, FALSE)
    ),
    "asthma" = dplyr::tibble(
      "concept_id" = 317009,
      "excluded" = FALSE,
      "descendants" = FALSE,
      "mapped" = FALSE
    )
  )))

  expect_no_error(emptyConceptSetExpression())
  expect_true(inherits(emptyConceptSetExpression(),
                       "concept_set_expression"))

  codelist <- newCodelist(list(disease_1 = c(1L, 2L, 3L), y = 5L))
  expect_true(inherits(codelist, "codelist"))
  expect_no_error(cs <- newConceptSetExpression(x = codelist))
  expect_true(inherits(cs, "concept_set_expression"))

  # bind codelists
  x <- newConceptSetExpression(list(
    "disease X" = dplyr::tibble(
      concept_id = c(1, 2, 3)
    ),
    "disease Y" = dplyr::tibble(
      concept_id = c(6, 55, 69, 12),
      descendants  = TRUE
    )
  ))
  y <- newConceptSetExpression(list(
    "disease Z" = dplyr::tibble(
      concept_id = c(13, 23),
      exclude = c(FALSE, TRUE)
    )
  ))
  z <- newConceptSetExpression(list(
    "disease Y" = dplyr::tibble(
      concept_id = c(6, 5, 9, 45, 67),
      mapped = TRUE
    )
  ))
  expect_no_error(res <- c(x, y))
  expect_true(all(c("disease X", "disease Y", "disease Z") %in% names(res)))
  expect_identical(c(x, y), bind(x, y))
  expect_identical(c(x, emptyConceptSetExpression()), x)
  x1 <- x
  names(x1) <- c("disease X", "disease Y_1")
  z1 <- z
  names(z1) <- c("disease Y_2")
  expect_warning(expect_identical(c(x, z), c(x1, z1)))
  expect_identical(c(x, x), x)

  expect_true(inherits(res[c("disease X", "disease Z")], "concept_set_expression"))

  expect_equal(
    dplyr::as_tibble(x),
    x |>
      unclass() |>
      dplyr::bind_rows(.id = "concept_set_expression_name")
  )

  expect_identical(x, x |> dplyr::as_tibble() |> newConceptSetExpression())
})
