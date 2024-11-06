test_that("bomen_calc matches reference outputs", {
  # Load reference outputs
  reference_outputs <-
    readRDS(test_path("reference_outputs.RDS"))
  test_data <-
    readRDS(test_path("test_data.RDS"))

  new_result1 <- bomen_calc(
    test_data |> filter(Jaar == 2024),
    group = c("Jaar"),
    group2 = NULL,
    respons = NULL
  )
  expect_equal(new_result1, reference_outputs$case1)

  # Case 2: List group with multiple groupings, with group2 and response
  normal_groups <- list(
    c("Jaar"),
    c("Jaar", "SoortType"),
    c("Jaar", "SoortIndeling")
  )

  new_result2 <- bomen_calc(
    test_data,
    group = normal_groups,
    group2 = "SPEC_DES",
    respons = "BladverliesNetto"
  )

  expect_equal(new_result2, reference_outputs$case2)
})
