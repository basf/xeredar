#' eventTimeData from StatCharrms
#'
#' The same dataset as stored in the StatCharrms package
"eventTimeData"


#' fecundityData from StatCharrms
#'
#' The same dataset as stored in the StatCharrms package
"fecundityData"

#' lengthWeightData from StatCharrms
#'
#' The same dataset as stored in the StatCharrms package
"lengthWeightData"


#' williamsTestLookUpTable from StatCharrms
#' @description This is the look up table for the critical values provided in
#'   Green (2018) derived from the StatCharrms package.
#'   This table is automatically called during the the execution of williamsTest
#'   function. Q and B: If c= number of reps on controls,
#'   r = number if reps in each treatment, w=c/r,
#'   then tcrit= Q- 10^-2 *B(1-1/w).
"williamsTestLookUpTable"

#' valid_data_xeta data used for testing the functions.
#' It is a list of data frames containing the spiked/unspiked or
#' combined measurements of all xeta validation studies.
#' The column conc in each dataframe is a factor with specified levels,
#' as the Williams test will otherwise not order the concentrations in the
#' correct way.
"valid_data_xeta"

#' testDataSpiked data used for testing the functions.
#' This is a data frame containing the spiked measurements of a xeta studies.
#' The two test_data sets are the only datasets with exposure well information
#' (column Row). The column Conc in each dataframe is a factor with specified
#'  levels, as the Williams test will otherwise not order the concentrations
#'  in the correct way.
"testDataSpiked"

#' testDataUnspiked data used for testing the functions.
#' This is a data frame containing the spiked measurements of a xeta studies.
#' The two test_data sets are the only datasets with exposure well information
#' (column Row). The column Conc in each dataframe is a factor with specified
#' levels, as the Williams test will otherwise not order the concentrations
#' in the correct way.
"testDataUnspiked"

#' Negative_Amant_IDEA_RADAR data used for testing the functions.
#' It contains spiked and unspiked data.The column Conc is a factor with
#' specified levels, as the Williams test will otherwise not order the
#' concentrations in the correct way. This dataset is from the RADAR study
#' validation in the lab IDEA with an inert chemical .
"Negative_Amant_IDEA_RADAR"

#' Negative_Amant_IDEA_RADAR_Spiked data used for testing the functions.
#' It contains only spiked data.The column Conc is a factor with specified
#' levels, as the Williams test will otherwise not order the concentrations in
#' the correct way. This dataset is from the RADAR study validation in the lab
#' IDEA with an inert chemical.
"Negative_Amant_IDEA_RADAR_Spiked"

#' Negative_Amant_IDEA_RADAR_Unspiked data used for testing the functions.
#' It contains only unspiked data.The column Conc is a factor with specified
#' levels, as the Williams test will otherwise not order the concentrations in
#' the correct way. This dataset is from the RADAR study validation in the lab
#' IDEA with an inert chemical .
"Negative_Amant_IDEA_RADAR_Unspiked"


#' Negative_Amant_Watchfrog_RADAR data used for testing the functions.
#' It contains spiked and unspiked data.The column Conc is a factor with
#' specified levels, as the Williams test will otherwise not order the
#' concentrations in the correct way. This dataset is from the RADAR study
#'  validation in the lab Watchfrog with an inert chemical.
"Negative_Amant_Watchfrog_RADAR"

#' Negative_Amant_Watchfrog_RADAR_Spiked data used for testing the functions.
#' It contains only spiked data.The column Conc is a factor with specified
#' levels, as the Williams test will otherwise not order the concentrations in
#' the correct way. This dataset is from the RADAR study validation in the lab
#' Watchfrog with an inert chemical .
"Negative_Amant_Watchfrog_RADAR_Spiked"

#' Negative_Amant_Watchfrog_RADAR_Unspiked data used for testing the functions.
#' It contains only unspiked data.The column Conc is a factor with specified
#' levels, as the Williams test will otherwise not order the concentrations in
#' the correct way. This dataset is from the RADAR study validation in the lab
#' Watchfrog with an inert chemical .
"Negative_Amant_Watchfrog_RADAR_Unspiked"

#' Negative_Amant_FIWI_RADAR data used for testing the functions.
#' It contains spiked and unspiked data.The column Conc is a factor with
#' specified levels, as the Williams test will otherwise not order the
#' concentrations in the correct way. This dataset is from the RADAR study
#' validation in the lab FIWI with an inert chemical.
"Negative_Amant_FIWI_RADAR"

#' Negative_Amant_FIWI_RADAR_Spiked data used for testing the functions. It
#' contains only spiked data.The column Conc is a factor with specified levels,
#' as the Williams test will otherwise not order the concentrations in the
#' correct way. This dataset is from the RADAR study validation in the lab
#' FIWI with an inert chemical.
"Negative_Amant_FIWI_RADAR_Spiked"

#' Negative_Amant_FIWI_RADAR_Unspiked data used for testing the functions.
#' It contains only unspiked data.
#' The column Conc is a factor with specified levels, as the Williams test will
#' otherwise not order the concentrations in the correct way. This dataset is
#' from the RADAR study validation in the lab FIWI with an inert chemical.
"Negative_Amant_FIWI_RADAR_Unspiked"

#' Pos_Anas_CEFAS_RADAR data used for testing the functions. It contains spiked
#' and unspiked data.The column Conc is a factor with specified levels, as the
#' Williams test will otherwise not order the concentrations in the correct way.
#' This dataset is from the RADAR study validation in the lab Cefas with an
#' androgen axis active chemical.
"Pos_Anas_CEFAS_RADAR"

#' Pos_Anas_CEFAS_RADAR_Spiked data used for testing the functions.
#' It contains only spiked data.The column Conc is a factor with specified
#' levels, as the Williams test will otherwise not order the concentrations
#' in the correct way. This dataset is from the RADAR study validation in the
#' lab Cefas with an androgen axis active chemical.
"Pos_Anas_CEFAS_RADAR_Spiked"

#' Pos_Anas_CEFAS_RADAR_Unspiked data used for testing the functions.
#' It contains only unspiked data.The column Conc is a factor with specified
#' levels, as the Williams test will otherwise not order the concentrations in
#' the correct way. This dataset is from the RADAR study validation in the lab
#' Cefas with an androgen axis active chemical.
"Pos_Anas_CEFAS_RADAR_Unspiked"


#' Pos_Anas_FIWI_RADAR data used for testing the functions. It contains spiked
#' and unspiked data.The column Conc is a factor with specified levels, as the
#' Williams test will otherwise not order the concentrations in the correct way.
#' This dataset is from the RADAR study validation in the lab FIWI with an
#' androgen axis active chemical.
"Pos_Anas_FIWI_RADAR"

#' Pos_Anas_FIWI_RADAR_Spiked data used for testing the functions.
#' It contains only spiked data.The column Conc is a factor with specified
#' levels, as the Williams test will otherwise not order the concentrations
#' in the correct way. This dataset is from the RADAR study validation in the
#' lab FIWI with an androgen axis active chemical.
"Pos_Anas_FIWI_RADAR_Spiked"

#' Pos_Anas_FIWI_RADAR_Unspiked data used for testing the functions.
#' It contains only unspiked data.The column Conc is a factor with specified
#' levels, as the Williams test will otherwise not order the concentrations in
#' the correct way. This dataset is from the RADAR study validation in the lab
#' FIWI with an androgen axis active chemical.
"Pos_Anas_FIWI_RADAR_Unspiked"


#' Pos_Anas_IDEA_RADAR data used for testing the functions.
#' It contains spiked and unspiked data.The column Conc is a factor with
#' specified levels, as the Williams test will otherwise not order the
#' concentrations in the correct way. This dataset is from the RADAR study
#' validation in the lab IDEA with an androgen axis active chemical.
"Pos_Anas_IDEA_RADAR"

#' Pos_Anas_IDEA_RADAR_Spiked data used for testing the functions.
#' It contains only spiked data.The column Conc is a factor with specified
#' levels, as the Williams test will otherwise not order the concentrations in
#' the correct way. This dataset is from the RADAR study validation in the lab
#' IDEA with an androgen axis active chemical.
"Pos_Anas_IDEA_RADAR_Spiked"

#' Pos_Anas_IDEA_RADAR_Unspiked data used for testing the functions.
#' It contains only unspiked data.The column Conc is a factor with specified
#' levels, as the Williams test will otherwise not order the concentrations in
#' the correct way. This dataset is from the RADAR study validation in the lab
#' IDEA with an androgen axis active chemical.
"Pos_Anas_IDEA_RADAR_Unspiked"

#' Pos_Anas_Watchfrog_RADAR data used for testing the functions.
#' It contains spiked and unspiked data.The column Conc is a factor with
#' specified levels, as the Williams test will otherwise not order the
#' concentrations in the correct way. This dataset is from the RADAR study
#' validation in the lab Watchfrog with an androgen axis active chemical.
"Pos_Anas_Watchfrog_RADAR"

#' Pos_Anas_Watchfrog_RADAR_Spiked data used for testing the functions.
#' It contains only spiked data.The column Conc is a factor with specified
#' levels, as the Williams test will otherwise not order the concentrations in
#' the correct way. This dataset is from the RADAR study validation in the lab
#' Watchfrog with an androgen axis active chemical.
"Pos_Anas_Watchfrog_RADAR_Spiked"

#' Pos_Anas_Watchfrog_RADAR_Unspiked data used for testing the functions.
#' It contains only unspiked data.The column Conc is a factor with specified
#' levels, as the Williams test will otherwise not order the concentrations in
#' the correct way. This dataset is from the RADAR study validation in the lab
#' Watchfrog with an androgen axis active chemical.
"Pos_Anas_Watchfrog_RADAR_Unspiked"

#' Pos_mDHT_Fraunhofer_RADAR data used for testing the functions.
#' It contains spiked and unspiked data.The column Conc is a factor with
#' specified levels, as the Williams test will otherwise not order the
#' concentrations in the correct way. This dataset is from the RADAR study
#' validation in the lab Fraunhofer with an androgen axis active chemical.
"Pos_mDHT_Fraunhofer_RADAR"

#' Pos_mDHT_Fraunhofer_RADAR_Spiked data used for testing the functions.
#' It contains only spiked data.The column Conc is a factor with specified
#' levels, as the Williams test will otherwise not order the concentrations in
#' the correct way. This dataset is from the RADAR study validation in the lab
#' Fraunhofer with an androgen axis active chemical.
"Pos_mDHT_Fraunhofer_RADAR_Spiked"

#' Pos_mDHT_Fraunhofer_RADAR_Unspiked data used for testing the functions.
#' It contains only unspiked data.The column Conc is a factor with specified
#' levels, as the Williams test will otherwise not order the concentrations in
#' the correct way. This dataset is from the RADAR study validation in the lab
#' Fraunhofer with an androgen axis active chemical.
"Pos_mDHT_Fraunhofer_RADAR_Unspiked"


#' REACTIV data used for testing the functions.
#' It contains both spiked and unspiked data.The column Conc is a factor with specified
#' levels, as the Williams test will otherwise not order the concentrations in
#' the correct way. This dataset is an artificiald dataset.
"reactiv_data"
