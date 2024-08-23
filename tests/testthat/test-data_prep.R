test_that("Output from dataPrep function is list with correct content", {
  expected <- data_prep(xeredar::valid_data_xeta[["triac_japan_unspiked"]],
    row = FALSE, dagostino = FALSE,
    trimming = TRUE
  )
  expect_type(expected, "list")
  expect_named(expected, c(
    "ProcessedData", "SummaryDF",
    "SummaryDF_Rep", "NormalityTest",
    "LeveneTest", "BoxPlots",
    "Monotonicity Test", "Dunnetts",
    "WilliamsDecrease",
    "WilliamsIncrease",
    "MixedAnova", "Justify"
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
  expect_s4_class(expected[["MixedAnova"]], "lmerMod")
  expect_type(expected[["Justify"]], "character")
  expected <- data_prep(xeredar::valid_data_xeta[["triac_japan_spiked"]],
    row = FALSE, dagostino = FALSE,
    trimming = TRUE, alpha = 0.005
  )
  expect_type(expected, "list")
  expect_named(expected, c(
    "ProcessedData", "SummaryDF",
    "SummaryDF_Rep", "NormalityTest",
    "LeveneTest", "BoxPlots",
    "Monotonicity Test", "Dunnetts",
    "WilliamsDecrease",
    "WilliamsIncrease",
    "MixedAnova", "Justify"
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
  expect_s4_class(expected[["MixedAnova"]], "lmerMod")
  expect_type(expected[["Justify"]], "character")
  expected <- data_prep(xeredar::valid_data_xeta[["triac_france_unspiked"]],
    row = FALSE, dagostino = FALSE,
    trimming = TRUE
  )
  expect_type(expected, "list")
  expect_named(expected, c(
    "ProcessedData", "SummaryDF",
    "SummaryDF_Rep", "NormalityTest",
    "LeveneTest", "BoxPlots",
    "Monotonicity Test", "Dunnetts",
    "WilliamsDecrease",
    "WilliamsIncrease",
    "MixedAnova", "Justify"
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
  expect_s4_class(expected[["MixedAnova"]], "lmerMod")
  expect_type(expected[["Justify"]], "character")
  expected <- data_prep(xeredar::valid_data_xeta[["triac_france_spiked"]],
    row = FALSE, dagostino = FALSE,
    trimming = TRUE
  )
  expect_type(expected, "list")
  expect_named(expected, c(
    "ProcessedData", "SummaryDF",
    "SummaryDF_Rep", "NormalityTest",
    "LeveneTest", "BoxPlots",
    "Monotonicity Test", "Dunnetts",
    "WilliamsDecrease",
    "WilliamsIncrease",
    "MixedAnova", "Justify"
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
  expect_s4_class(expected[["MixedAnova"]], "lmerMod")
  expect_type(expected[["Justify"]], "character")
  expected <- data_prep(xeredar::valid_data_xeta[["triac_usa_unspiked"]],
    row = FALSE, dagostino = FALSE,
    trimming = TRUE
  )
  expect_type(expected, "list")
  expect_named(expected, c(
    "ProcessedData", "SummaryDF",
    "SummaryDF_Rep", "NormalityTest",
    "LeveneTest", "BoxPlots",
    "Monotonicity Test", "Dunnetts",
    "WilliamsDecrease",
    "WilliamsIncrease",
    "MixedAnova", "Justify"
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
  expect_s4_class(expected[["MixedAnova"]], "lmerMod")
  expect_type(expected[["Justify"]], "character")
  expected <- data_prep(xeredar::valid_data_xeta[["triac_usa_spiked"]],
    row = FALSE, dagostino = FALSE,
    trimming = TRUE
  )
  expect_type(expected, "list")
  expect_named(expected, c(
    "ProcessedData", "SummaryDF",
    "SummaryDF_Rep", "NormalityTest",
    "LeveneTest", "BoxPlots",
    "Monotonicity Test", "Dunnetts",
    "WilliamsDecrease",
    "WilliamsIncrease",
    "MixedAnova", "Justify"
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
  expect_s4_class(expected[["MixedAnova"]], "lmerMod")
  expect_type(expected[["Justify"]], "character")
})
