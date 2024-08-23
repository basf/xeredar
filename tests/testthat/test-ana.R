test_that("Output from ana function is list with correct content", {
  mixedaov <- lme4::lmer(Fluor ~ Conc + (1 | Replicate) + (1 | Replicate:Conc),
    data = xeredar::valid_data_xeta[["triac_france_spiked"]], REML = TRUE
  )
  expected <- ana(xeredar::valid_data_xeta[["triac_france_spiked"]],
                  mixedaov, dagostino = FALSE)
  expect_type(expected, "list")
  expect_named(expected, c(
    "ProcessedData", "SummaryDF",
    "SummaryDF_Rep", "NormalityTest",
    "LeveneTest", "BoxPlots",
    "Monotonicity Test", "Dunnetts",
    "WilliamsDecrease",
    "WilliamsIncrease"
  ))
  expect_s3_class(expected[["ProcessedData"]], "data.frame")
  expect_s3_class(expected[["SummaryDF"]], "data.frame")
  expect_s3_class(expected[["SummaryDF_Rep"]], "data.frame")
  expect_s3_class(expected[["NormalityTest"]], "htest")
  expect_s3_class(expected[["LeveneTest"]], "data.frame")
  expect_s3_class(expected[["BoxPlots"]], "ggplot")
  expect_s3_class(expected[["Monotonicity Test"]], "data.frame")
  expect_s3_class(expected[["Dunnetts"]], "data.frame")
  expect_s3_class(expected[["WilliamsDecrease"]], "data.frame")
  expect_s3_class(expected[["WilliamsIncrease"]], "data.frame")
})
