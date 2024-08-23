test_that("Output from Validity function is list with correct content", {
  expected <- validity(xeredar::valid_data_xeta[["ptu_usa_combined"]],
                       xeredar::valid_data_xeta[["ptu_usa_spiked"]],
                       row = FALSE)
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

  expected <- validity(xeredar::valid_data_xeta[["triac_japan_combined"]],
                       xeredar::valid_data_xeta[["triac_japan_spiked"]],
                       row = FALSE, dagostino = FALSE)
  expect_type(expected, "character")

  expected <- validity(
    xeredar::valid_data_xeta[["t4_france_combined"]],
    xeredar::valid_data_xeta[["t4_france_unspiked"]],
    row = FALSE, dagostino = FALSE
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
  expected <- validity(xeredar::valid_data_xeta[["t4_france_combined"]],
                       xeredar::valid_data_xeta[["t4_france_spiked"]],
                       row = FALSE, dagostino = FALSE)
  expect_type(expected, "list")
  expect_named(expected, c(
    "ProcessedData", "SummaryDF",
    "SummaryDF_Rep", "NormalityTest",
    "LeveneTest", "BoxPlots",
    "Monotonicity Test", "Dunnetts",
    "WilliamsDecrease", "WilliamsIncrease",
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
  expected <- validity(xeredar::valid_data_xeta[["t4_usa_combined"]],
                       xeredar::valid_data_xeta[["t4_usa_unspiked"]],
                       row = FALSE, dagostino = FALSE, alpha = 0)
  expect_type(expected, "list")
  expect_named(expected, c(
    "ProcessedData", "SummaryDF", "SummaryDF_Rep",
    "NormalityTest", "LeveneTest", "BoxPlots",
    "Monotonicity Test", "Dunnetts", "WilliamsDecrease",
    "WilliamsIncrease", "MixedAnova", "Justify"
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
  expected <- validity(xeredar::valid_data_xeta[["t4_usa_combined"]],
                       xeredar::valid_data_xeta[["t4_usa_spiked"]],
                       row = FALSE, dagostino = FALSE, alpha = 0)
  expect_type(expected, "list")
  expect_named(expected, c(
    "ProcessedData", "SummaryDF", "SummaryDF_Rep",
    "NormalityTest", "LeveneTest", "BoxPlots",
    "Monotonicity Test", "Dunnetts", "WilliamsDecrease",
    "WilliamsIncrease", "MixedAnova", "Justify"
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
