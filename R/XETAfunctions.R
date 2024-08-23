#' ana: Conducting analysis of XETA
#' @description This function is used by the data_prep function
#'  (which decides whether trimming and/or transformations are conducted) to
#'  carry out the actual analysis.
#' However, the ana function can also be used independently from data_prep,
#' in case a different approach than recommended by data_prep is anticipated.
#'
#' @param dataframe A dataframe containing only the original spiked or unspiked
#'  samples without T4 control.
#' @param mixedaov A mixed ANOVA object with Fluor as dependent variable and
#'   Conc + (1|Replicate)+(1|Replicate:Conc)+(1|Replicate:Conc:Row) as linear
#'  predictor. The last random term is optional.
#' @param dagostino boolean. Default is FALSE; when FALSE, the shapiro wilks
#'   test from the stats package is carried out, when TRUE the dAgostina Pearson
#'   normality test from the fBasics test is carried out.
#' @param seed numeric. The number use to set the seed. Default is 123
#'
#' @return A list (collection) containing the trimmed or untrimmed data frame,
#'   two summary data frames containing information summarized based on the
#'   concentration and the replicate, the result from the shapiro and levene
#'   test, the boxplots per replicate, the result of the monotonicity test as
#' well as the result from the Dunnett's and Williams test.
#' @importFrom rlang .data
#' @export
#' @examples
#' mixedaov <- lme4::lmer(
#'   Fluor ~ Conc +
#'     (1 | Replicate) + (1 | Replicate:Conc) + (1 | Replicate:Conc:Row),
#'   data = xeredar::testDataSpiked, REML = TRUE
#' )
#' Result1 <- ana(xeredar::testDataSpiked, mixedaov)
#'
#' # second example:
#' mixedaov <- lme4::lmer(Fluor ~ Conc + (1 | Replicate) + (1 | Replicate:Conc),
#'   data = xeredar::valid_data_xeta[["triac_france_spiked"]], REML = TRUE
#' )
#' Result2 <- ana(xeredar::valid_data_xeta[["triac_france_spiked"]], mixedaov)
#'
#' # third example:
#' mixedaov <- lme4::lmer(log(Fluor) ~ Conc + (1 | Replicate) +
#'   (1 | Replicate:Conc), data =
#'   xeredar::valid_data_xeta[["triac_japan_spiked"]],
#'   REML = TRUE
#' )
#' Result2 <- ana(xeredar::valid_data_xeta[["triac_japan_spiked"]], mixedaov)
#'
ana <- function(dataframe, mixedaov, dagostino = FALSE, seed = 123) {
  validate_dfxeta(df = dataframe, row = FALSE)
  validate_mixedanova(mixedaov)

  summary_df <- dataframe |>
    dplyr::group_by(.data$Conc) |>
    dplyr::summarise(
      N = dplyr::n(),
      Mean = mean(.data$Fluor, na.rm = TRUE),
      "Standard deviation" = stats::sd(.data$Fluor, na.rm = TRUE),
      "Coefficient of variation" =
        stats::sd(.data$Fluor, na.rm = TRUE) / mean(.data$Fluor, na.rm = TRUE)
    )

  summary_df_rep <- dataframe |>
    dplyr::group_by(.data$Conc, .data$Replicate) |>
    dplyr::summarise(
      N = dplyr::n(),
      Mean = mean(.data$Fluor, na.rm = TRUE),
      "Standard deviation" = stats::sd(.data$Fluor, na.rm = TRUE),
      "Coefficient of variation" =
        stats::sd(.data$Fluor, na.rm = TRUE) / mean(.data$Fluor, na.rm = TRUE)
    )

  normality_test <- norm_test(dagostino = dagostino, mixedaov = mixedaov)
  levenetest <- broom::tidy(car::leveneTest(
    stats::resid(mixedaov),
    mixedaov@frame[["Conc"]]
  ))

  box_plots <- box_plots_ana(df = dataframe)

  mono_test <- monotonicityTest(
    Data = dataframe,
    Treatment = "Conc", Response = "Fluor"
  )


  set.seed(seed)
  dunnett <- summary(
    multcomp::glht(mixedaov, emmeans::emm(trt.vs.ctrl1 ~ Conc))
  )
  means <- dataframe |>
    dplyr::group_by(.data$Conc) |>
    dplyr::summarise(Mean = mean(.data$Fluor, na.rm = TRUE)) |>
    as.data.frame(.data)
  dunnetts <- data.frame(
    "Estimate" = dunnett[["test"]][["coefficients"]],
    "SE" = dunnett[["test"]][["sigma"]],
    "df" = rep(dunnett[["df"]],
      times =
        length(dunnett[["test"]][["sigma"]])
    ),
    "t value" = dunnett[["test"]][["tstat"]],
    "adj p" = dunnett[["test"]][["pvalues"]]
    [-length(unique(dataframe$Conc))],
    "% Incr" = 100 * c((means[, "Mean"] /
                          means[1, "Mean"]) - 1)[-1],
    check.names = FALSE
  )

  if (length(unique(dataframe$Conc)) > 3) {
    colnames(mixedaov@frame)[1] <- "Fluor"
    amalgameans <- williamsTest(
      df = mixedaov@frame,
      resp = "Fluor",
      trt = "Conc",
      direction = "decreasing"
    )

    diffd <- amalgameans$Y0 - amalgameans$Y.Tilde
    sediff <- rev(dunnett[["test"]][["sigma"]])
    willd <- diffd / sediff
    df <- dunnett[["df"]]
    crit <- rev(xeredar::williamsTestLookUpTable[
      which(xeredar::williamsTestLookUpTable$df == df),
      c(seq(from = 2, to = 2 * length(unique(dataframe$Conc))
            - 2, by = 2))
    ]) # four doses --> Q4
    signd <- crit < willd
    williams_decrease <- data.frame(
      "Conc" = amalgameans[, 1],
      "Y.Tilde" = amalgameans[, 2],
      "Y0" = amalgameans[, 3],
      "DIFF Decr" = diffd,
      "SE_DIFF" = sediff,
      "DF" = df,
      "WILL Decr" = willd,
      "crit Val" = c(t(crit)),
      "Sign" = c(t(signd)),
      "% Incr" = rev(dunnetts$`% Incr`),
      check.names = FALSE
    )


    amalgameans <- williamsTest(
      df = mixedaov@frame,
      resp = "Fluor",
      trt = "Conc",
      direction = "increasing"
    )
    diffi <- amalgameans$Y.Tilde - amalgameans$Y0
    willi <- diffi / sediff
    signi <- crit < willi
    williams_increase <- data.frame(
      "Conc" = amalgameans[, 1],
      "Y.Tilde" = amalgameans[, 2],
      "Y0" = amalgameans[, 3],
      "DIFF Incr" = diffi,
      "SE_DIFF" = sediff,
      "DF" = df,
      "WILL Incr" = willi,
      "crit Val" = c(t(crit)),
      "Sign" = c(t(signi)),
      "% Incr" = rev(dunnetts$`% Incr`),
      check.names = FALSE
    )
  } else {
    williams_decrease <- "The number of treatment concentrations is not
    high enough to use a trend test. Please use the pairwise Dunnett's test."
    williams_increase <- "The number of treatment concentrations is not
    high enough to use a trend test. Please use the pairwise Dunnett's test."
  }
  collection <- list(
    "ProcessedData" = dataframe,
    "SummaryDF" = summary_df,
    "SummaryDF_Rep" = summary_df_rep,
    "NormalityTest" = normality_test,
    "LeveneTest" = levenetest,
    "BoxPlots" = box_plots,
    "Monotonicity Test" = mono_test,
    "Dunnetts" = dunnetts,
    "WilliamsDecrease" = williams_decrease,
    "WilliamsIncrease" = williams_increase
  )
  return(collection)
}


#' box_plots_ana: internal function used by the function ana() to
#' create boxplots
#' @description This function is used by the ana() function to
#'  create replicate specific boxplots.

#'
#' @param df A dataframe containing only the original spiked or unspiked
#'  samples without T4 control.'
#' @return A figure
#' #' @importFrom rlang .data
#' @export
#'
#' @examples
#' box_plots_ana(df =xeredar::valid_data_xeta[["triac_france_spiked"]])
box_plots_ana <- function(df) {
  validate_dfxeta(df = df, row = FALSE)
  ggplot2::ggplot(
    df,
    ggplot2::aes(.data$Fluor, .data$Conc)
  ) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_grid(rows = ~ Replicate) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Fluorescence", y = "Treatment concentration")
}








#' norm_test choose whether you want to carry out the d'Agostino Pearson
#' normality test from the fBasics package or the shapiro wilks test from the
#' stats package
#'
#' @description This function is called from the Ana and dataPrep function
#' @param dagostino boolean. Default is FALSE; when FALSE, the shapiro wilks
#'   test from the stats package is carried out, when TRUE the dAgostino Pearson
#'   normality test from the fBasics test is carried out.
#' @param mixedaov a mixed anova model object specified with lmer
#'   from lme4.
#'
#' @return a list with p-values from the selected normality test.
#' @export
#'
#' @examples
#' mixedaov <- lme4::lmer(Fluor ~ Conc + (1 | Replicate) + (1 | Replicate:Conc),
#'   data = xeredar::valid_data_xeta[["triac_france_spiked"]], REML = TRUE
#' )
#' norm_test(dagostino = TRUE, mixedaov = mixedaov)
norm_test <- function(dagostino = FALSE, mixedaov) {
  # TODO: Check inputs (esp. mixed anova)
  if (dagostino) {
    # TODO: Do we really want to print always? (I find this annoying...)
    invisible(fBasics::dagoTest(stats::resid(mixedaov))@test)
  } else {
    invisible(stats::shapiro.test(stats::resid(mixedaov)))
  }
}



#' anova_assumpts checks whether normality and variance homogeneity
#' assumptions are fulfilled.
#'
#' @param dagostino boolean. Default is FALSE; when FALSE, the shapiro wilks
#'   test from the stats package is carried out, when TRUE the dAgostino Pearson
#'   normality test from the fBasics test is carried out.
#' @param mixedaov a mixed anova model object specified with lmer
#'   from lme4.
#' @param alpha Numeric. Default is 0.01. This value is the alpha value for
#'  the normality and variance homogeneity test.
#'
#' @return boolean; if TRUE, the p-values from the normality and
#'  variance homogeneity test are larger then alpha. If FALSE, one
#'  or both p-values from those tests is/are smaller than alpha.
#' @export
#'
#' @examples
#' mixedaov <- lme4::lmer(Fluor ~ Conc + (1 | Replicate) + (1 | Replicate:Conc),
#'   data = xeredar::valid_data_xeta[["triac_france_unspiked"]], REML = TRUE
#' )
#' anova_assumpts(dagostino = TRUE, mixedaov = mixedaov, alpha = 0.01)
anova_assumpts <- function(dagostino, mixedaov, alpha) {
  ifelse(
    norm_test(dagostino = dagostino, mixedaov = mixedaov)$p.value[1] > alpha &
      !is.na(norm_test(dagostino = dagostino, mixedaov = mixedaov)$p.value[1]) &
      broom::tidy(
        car::leveneTest(
          stats::resid(mixedaov),
          mixedaov@frame[["Conc"]]
        )
      )$p.value > alpha, TRUE, FALSE
  )
}

#' data_prep: This function recommends a data preparation method
#' (transformation and/or trimming) and conducts the analysis
#' accordingly.
#'
#' @description This function decides whether trimming, outlier removal and/or
#' transformations are conducted. The actual analysis is carried out by the
#' ana function.
#'
#' @param row Please specify FALSE (boolean) in case you did not record the
#' exposure well ID/ Row of the 96 well plate or you want to use the reduced
#' mixed ANOVA model where the exposure well ID is not a random effect.
#' @param dataframe A dataframe containing only the original spiked or unspiked
#' samples without T4 control.
#' @param dagostino boolean. Default is FALSE; when FALSE, the shapiro
#' wilks test from the stats package is carried out, when TRUE the
#' dAgostino Pearson normality test from the fBasics test is carried out.
#' @param alpha Numeric. Default is 0.01. This value is the alpha value for the
#' normality and variance homogeneity test.
#' @param trimming boolean. When TRUE, trimming is conducted when required. When
#' FALSE trimming is never conducted. Default is TRUE.
#' @param outlier boolean, When TRUE, outlier removal is conducted when
#' required. When FALSE, outlier removal is never conducted.
#' Default is TRUE.
#' @param boxcox boolean, When TRUE boxcox transformation is performed when
#' necessary and when no zeros are present in the data. When FALSE it will never
#' be conducted. Default is TRUE.
#' @param artc boolean, When TRUE ART-C transformation is performed when
#' necessary.
#' When FALSE it will never be conducted. Default is FALSE.
#' This feature is experimental!
#' @return A list (collection) containing the trimmed or untrimmed data frame,
#' two summary data frames containing information summarized based on the
#' concentration and the replicate, the result from the shapiro and levene test,
#' the boxplots per replicate, the result of the monotonicity test as well
#' as the result from the Dunnett's and Williams test. Different from the
#' output of the ana function, the returned list also contains the mixed
#' Anova model (mixedaov) whereby Fluor is transformed as recommended
#' and the trimmed or untrimmed data was utilized. Furthermore, the output
#' also contains a reasoning for the recommended transformation
#' and trimming.
#' @export
#' @examples
#'
#' # XETA example with thyroid active chemical (data from validation studies):
#' result1_1 <- data_prep(xeredar::valid_data_xeta[["triac_japan_unspiked"]],
#'  row = FALSE)
#' result1_2 <- data_prep(xeredar::valid_data_xeta[["triac_japan_spiked"]],
#'  row = FALSE)
#' # third example:
#' result2_1 <- data_prep(xeredar::valid_data_xeta[["triac_france_spiked"]],
#'  row = FALSE)
#' result2_2 <- data_prep(xeredar::valid_data_xeta[["triac_france_unspiked"]],
#'  row = FALSE)
#' # fourth example:
#' result3_1 <- data_prep(xeredar::valid_data_xeta[["triac_usa_spiked"]],
#'  row = FALSE)
#' result3_2 <- data_prep(xeredar::valid_data_xeta[["triac_usa_unspiked"]],
#'  row = FALSE)
#'
#' # RADAR example androgen axis active chemical (data from validation studies)
#' result4_1 <- data_prep(xeredar::Pos_Anas_CEFAS_RADAR_Unspiked,
#'   row = FALSE, dagostino = TRUE, trimming = TRUE, outlier = TRUE,
#'   boxcox = FALSE, artc = FALSE, alpha = 0
#' )
#' result4_2 <- data_prep(xeredar::Pos_Anas_CEFAS_RADAR_Spiked,
#'   row = FALSE, dagostino = TRUE, trimming = FALSE, outlier = FALSE,
#'   boxcox = FALSE, alpha = 0
#' )
#' result5_1 <- data_prep(xeredar::Pos_Anas_FIWI_RADAR_Unspiked,
#'   row = FALSE,
#'   dagostino = TRUE, trimming = TRUE, outlier = FALSE,
#'   boxcox = FALSE, artc = FALSE, alpha = 0.05
#' )
#' result5_2 <- data_prep(xeredar::Pos_Anas_FIWI_RADAR_Spiked,
#'   row = FALSE,
#'   dagostino = TRUE, trimming = FALSE
#' )
#' result6_1 <- data_prep(xeredar::Pos_Anas_IDEA_RADAR_Unspiked,
#'   row = FALSE,
#'   dagostino = TRUE, trimming = FALSE
#' )
#' result6_2 <- data_prep(xeredar::Pos_Anas_IDEA_RADAR_Spiked,
#'   row = FALSE,
#'   dagostino = TRUE, trimming = FALSE
#' )
#' result7_1 <- data_prep(xeredar::Pos_Anas_Watchfrog_RADAR_Unspiked,
#'   row = FALSE, dagostino = TRUE, trimming = FALSE
#' )
#' result7_2 <- data_prep(xeredar::Pos_Anas_Watchfrog_RADAR_Spiked,
#'   row = FALSE,
#'   dagostino = TRUE, trimming = FALSE
#' )
data_prep <- function(dataframe, row = TRUE, dagostino = FALSE,
                      trimming = TRUE, outlier = TRUE, boxcox = TRUE,
                      artc = FALSE, alpha = 0.01) {
  validate_dfxeta(df = dataframe, row = row)
  dataframe <- dataframe |> dplyr::filter(!is.na(.data$Fluor))
  if (min(dataframe$Fluor, na.rm = TRUE) <= 0) {
    boxcox <- FALSE
    logarithm <- FALSE
  } else if (boxcox != FALSE) {
    boxcox <- TRUE
    logarithm <- TRUE
  } else {
    boxcox <- FALSE
    logarithm <- TRUE
  }

  if (row) {
    mixedaov <- tryCatch(
      lme4::lmer(Fluor ~ Conc + (1 | Replicate) + (1 | Replicate:Conc) +
                   (1 | Replicate:Conc:Row), data = dataframe, REML = TRUE),
      error = function(e) {
        FALSE
      }
    )
  } else {
    mixedaov <- tryCatch(
      lme4::lmer(Fluor ~ Conc + (1 | Replicate) + (1 | Replicate:Conc),
        data = dataframe, REML = TRUE
      ),
      error = function(e) {
        FALSE
      }
    )
  }
  if (isS4(mixedaov)) {
    if (
        anova_assumpts(
          dagostino = dagostino, mixedaov = mixedaov, alpha = alpha
        )) {
      justify <- ("The raw data should be used for the analysis because
                  the residuals of the mixed ANOVA are normally
                  distributed and show homogeneous variances
                  among treatment groups.")
      collection <- ana(
        dataframe = dataframe, mixedaov = mixedaov, dagostino = dagostino
      )
      collection <- append(collection, list(
        "MixedAnova" = mixedaov, "Justify" = justify
      ))
      return(collection)
    }
  }
  if (trimming) {
    dat_trim <- trim(dataframe)[["dat_trim"]]
    if (row) {
      mixedaov <- tryCatch(
        lme4::lmer(Fluor ~ Conc + (1 | Replicate) + (1 | Replicate:Conc) +
                     (1 | Replicate:Conc:Row), data = dat_trim, REML = TRUE),
        error = function(e) {
          FALSE
        }
      )
    } else {
      mixedaov <- tryCatch(
        lme4::lmer(Fluor ~ Conc + (1 | Replicate) + (1 | Replicate:Conc),
          data = dat_trim, REML = TRUE
        ),
        error = function(e) {
          FALSE
        }
      )
    }
    if (isS4(mixedaov)) {
      if (
          anova_assumpts(
            dagostino = dagostino, mixedaov = mixedaov, alpha = alpha
          )) {
        justify <- ("The trimmed data should be used for the analysis because
                    only after trimming, the residuals of the mixed ANOVA are
                    normally distributed and show homogeneous variances among
                    treatment groups.")
        collection <- ana(dat_trim, mixedaov = mixedaov, dagostino = dagostino)
        collection <- append(collection, list(
          "MixedAnova" = mixedaov, "Justify" = justify
        ))
        return(collection)
      }
    }
  }
  if (outlier) {
    wt_outlier <- rm_outlier(dataframe, row = row)[["wt_outlier"]]
    if (row) {
      mixedaov <- tryCatch(
        lme4::lmer(Fluor ~ Conc + (1 | Replicate) + (1 | Replicate:Conc) +
                     (1 | Replicate:Conc:Row), data = wt_outlier, REML = TRUE),
        error = function(e) {
          FALSE
        }
      )
    } else {
      mixedaov <- tryCatch(
        lme4::lmer(Fluor ~ Conc + (1 | Replicate) + (1 | Replicate:Conc),
          data = wt_outlier, REML = TRUE
        ),
        error = function(e) {
          FALSE
        }
      )
    }
    if (isS4(mixedaov)) {
      if (
          anova_assumpts(
            dagostino = dagostino, mixedaov = mixedaov, alpha = alpha
          )) {
        justify <- ("The data from which outliers were removed with the
                    Tukey-rule should be used for the analysis because only
                    after outlier removal the residuals of the mixed ANOVA are
                    normally distributed and show homogeneous variances among
                    treatment groups")
        collection <- ana(wt_outlier,
                          mixedaov = mixedaov, dagostino = dagostino)
        collection <- append(collection, list(
          "MixedAnova" = mixedaov, "Justify" = justify
        ))
        return(collection)
      }
    }
  }

  if (logarithm) {
    if (row) {
      mixedaov <- tryCatch(
        lme4::lmer(log(Fluor) ~ Conc + (1 | Replicate) + (1 | Replicate:Conc) +
                     (1 | Replicate:Conc:Row), data = dataframe, REML = TRUE),
        error = function(e) {
          FALSE
        }
      )
    } else {
      mixedaov <- tryCatch(
        lme4::lmer(log(Fluor) ~ Conc + (1 | Replicate) + (1 | Replicate:Conc),
          data = dataframe, REML = TRUE
        ),
        error = function(e) {
          FALSE
        }
      )
    }
    if (isS4(mixedaov)) {
      if (
          anova_assumpts(
            dagostino = dagostino, mixedaov = mixedaov, alpha = alpha
          )) {
        justify <- ("The raw data (without trimming or outlier removal) where
                    the fluorescence values are log transformed should be used
                    for the analysis because only after log transformation,
                    the residuals of the mixed ANOVA are normally distributed
                    and have homogeneous variances among treatment groups.")
        collection <- ana(dataframe, mixedaov = mixedaov, dagostino = dagostino)
        collection <- append(collection, list(
          "MixedAnova" = mixedaov, "Justify" = justify
        ))
        return(collection)
      }
    }
  }
  if (row) {
    mixedaov <- tryCatch(
      lme4::lmer(sqrt(Fluor) ~ Conc + (1 | Replicate) + (1 | Replicate:Conc) +
                   (1 | Replicate:Conc:Row), data = dataframe, REML = TRUE),
      error = function(e) {
        FALSE
      }
    )
  } else {
    mixedaov <- tryCatch(
      lme4::lmer(sqrt(Fluor) ~ Conc + (1 | Replicate) + (1 | Replicate:Conc),
        data = dataframe, REML = TRUE
      ),
      error = function(e) {
        FALSE
      }
    )
  }
  if (isS4(mixedaov)) {
    if (
        anova_assumpts(
          dagostino = dagostino, mixedaov = mixedaov, alpha = alpha
        )) {
      justify <- ("The raw data (without trimming or outlier removal) where
                  the fluorescence values are square-root transformed should
                  be used for the analysis because only after sqrt
                  transformation, the residuals of the mixed ANOVA are normally
                  distributed and have homogeneous variances among
                  treatment groups.")
      collection <- ana(dataframe, mixedaov = mixedaov, dagostino = dagostino)
      collection <- append(collection, list(
        "MixedAnova" = mixedaov, "Justify" = justify
      ))
      return(collection)
    }
  }

  if (boxcox) {
    bc <- MASS::boxcox(dataframe$Fluor ~ dataframe$Conc)
    lambda <- bc$x[which.max(bc$y)]
    if (row) {
      mixedaov <- tryCatch(
        lme4::lmer(
          (Fluor^lambda - 1) / lambda ~ Conc + (1 | Replicate) +
            (1 | Replicate:Conc) + (1 | Replicate:Conc:Row),
          data = dataframe, REML = TRUE
        ),
        error = function(e) {
          FALSE
        }
      )
    } else {
      mixedaov <- tryCatch(
        lme4::lmer((Fluor^lambda - 1) / lambda ~ Conc + (1 | Replicate) +
                     (1 | Replicate:Conc), data = dataframe, REML = TRUE),
        error = function(e) {
          FALSE
        }
      )
    }
    if (isS4(mixedaov)) {
      if (
          anova_assumpts(
            dagostino = dagostino, mixedaov = mixedaov, alpha = alpha
          )) {
        justify <- ("The raw data (without trimming or outlier removal)
                    where the fluorescence values are box-cox transformed
                    should be used for the analysis because only after
                    box-cox transformation, the residuals of the mixed
                    ANOVA are normally distributed and have homogeneous
                    variances among treatment groups.")
        collection <- ana(dataframe, mixedaov = mixedaov)
        collection <- append(collection, list(
          "MixedAnova" = mixedaov, "Justify" = justify
        ))
        return(collection)
      }
    }
  }

  if (trimming) {
    dat_trim <- trim(dataframe)$dat_trim

    if (logarithm) {
      if (row) {
        mixedaov <- tryCatch(
          lme4::lmer(
            log(Fluor) ~ Conc + (1 | Replicate) +
              (1 | Replicate:Conc) + (1 | Replicate:Conc:Row),
            data = dat_trim, REML = TRUE
          ),
          error = function(e) {
            FALSE
          }
        )
      } else {
        mixedaov <- tryCatch(
          lme4::lmer(log(Fluor) ~ Conc + (1 | Replicate) + (1 | Replicate:Conc),
            data = dat_trim, REML = TRUE
          ),
          error = function(e) {
            FALSE
          }
        )
      }
      if (isS4(mixedaov)) {
        if (
            anova_assumpts(
              dagostino = dagostino, mixedaov = mixedaov, alpha = alpha
            )) {
          justify <- ("The trimmed data where the fluorescence values are log
                      transformed, should be used for the analysis because
                      only after trimming and log transformation, the residuals
                      of the mixed ANOVA are normally distributed and have
                      homogeneous variances among treatment groups")
          collection <- ana(dat_trim,
                            mixedaov = mixedaov, dagostino = dagostino)
          collection <- append(collection, list(
            "MixedAnova" = mixedaov, "Justify" = justify
          ))
          return(collection)
        }
      }
    }

    if (row) {
      mixedaov <- tryCatch(
        lme4::lmer(sqrt(Fluor) ~ Conc + (1 | Replicate) + (1 | Replicate:Conc)
                   + (1 | Replicate:Conc:Row), data = dat_trim, REML = TRUE),
        error = function(e) {
          FALSE
        }
      )
    } else {
      mixedaov <- tryCatch(
        lme4::lmer(sqrt(Fluor) ~ Conc + (1 | Replicate) + (1 | Replicate:Conc),
          data = dat_trim, REML = TRUE
        ),
        error = function(e) {
          FALSE
        }
      )
    }
    if (isS4(mixedaov)) {
      if (
          anova_assumpts(
            dagostino = dagostino, mixedaov = mixedaov, alpha = alpha
          )) {
        justify <- ("The trimmed data where the fluorescence values are
                    square-root transformed, should be used for the analysis
                    because only after trimming and sqrt transformation, the
                    residuals of the mixed ANOVA are normally distributed
                    and have homogeneous variances among treatment groups")
        collection <- ana(dat_trim, mixedaov = mixedaov, dagostino = dagostino)
        collection <- append(collection, list(
          "MixedAnova" = mixedaov, "Justify" = justify
        ))
        return(collection)
      }
    }

    if (boxcox) {
      bc <- MASS::boxcox(dat_trim$Fluor ~ dat_trim$Conc)
      lambda <- bc$x[which.max(bc$y)]
      if (row) {
        mixedaov <- tryCatch(
          lme4::lmer(
            (Fluor^lambda - 1) / lambda ~ Conc + (1 | Replicate)
            + (1 | Replicate:Conc) + (1 | Replicate:Conc:Row),
            data = dat_trim, REML = TRUE
          ),
          error = function(e) {
            FALSE
          }
        )
      } else {
        mixedaov <- tryCatch(
          lme4::lmer((Fluor^lambda - 1) / lambda ~ Conc + (1 | Replicate)
                     + (1 | Replicate:Conc), data = dat_trim, REML = TRUE),
          error = function(e) {
            FALSE
          }
        )
      }
      if (isS4(mixedaov)) {
        if (
            anova_assumpts(
              dagostino = dagostino, mixedaov = mixedaov, alpha = alpha
            )) {
          justify <- ("The trimmed data where the fluorescence values are
                      box-cox transformed, should be used for the analysis
                      because only after trimming and box-cox transformation,
                      the residuals of the mixed ANOVA are normally distributed
                      and have homogeneous variances among treatment groups")
          collection <- ana(dat_trim,
                            mixedaov = mixedaov, dagostino = dagostino)
          collection <- append(collection, list(
            "MixedAnova" = mixedaov, "Justify" = justify
          ))
          return(collection)
        }
      }
    }
  }
  if (outlier) {
    if (logarithm) {
      if (row) {
        mixedaov <- tryCatch(
          lme4::lmer(
            log(Fluor) ~ Conc + (1 | Replicate) + (1 | Replicate:Conc)
            + (1 | Replicate:Conc:Row),
            data = wt_outlier, REML = TRUE
          ),
          error = function(e) {
            FALSE
          }
        )
      } else {
        mixedaov <- tryCatch(
          lme4::lmer(log(Fluor) ~ Conc + (1 | Replicate) + (1 | Replicate:Conc),
            data = wt_outlier, REML = TRUE
          ),
          error = function(e) {
            FALSE
          }
        )
      }
      if (isS4(mixedaov)) {
        if (
            anova_assumpts(
              dagostino = dagostino, mixedaov = mixedaov, alpha = alpha
            )) {
          justify <- ("The data from which outliers were removed with the
                      Tukey-rule where the fluorescence values are log
                      transformed, should be used for the analysis because
                      only after outlier removal and log transformation, the
                      residuals of the mixed ANOVA are normally distributed
                      and have homogeneous variances among treatment groups")
          collection <- ana(
            wt_outlier,
            mixedaov = mixedaov, dagostino = dagostino
          )
          collection <- append(collection, list(
            "MixedAnova" = mixedaov, "Justify" = justify
          ))
          return(collection)
        }
      }
    }
    if (row) {
      mixedaov <- tryCatch(
        lme4::lmer(
          sqrt(Fluor) ~ Conc + (1 | Replicate) + (1 | Replicate:Conc)
          + (1 | Replicate:Conc:Row),
          data = wt_outlier, REML = TRUE
        ),
        error = function(e) {
          FALSE
        }
      )
    } else {
      mixedaov <- tryCatch(
        lme4::lmer(sqrt(Fluor) ~ Conc + (1 | Replicate) + (1 | Replicate:Conc),
          data = wt_outlier, REML = TRUE
        ),
        error = function(e) {
          FALSE
        }
      )
    }
    if (isS4(mixedaov)) {
      if (
          anova_assumpts(
            dagostino = dagostino, mixedaov = mixedaov, alpha = alpha
          )) {
        justify <- ("The data from which outliers were removed with the
                    Tukey-rule where the fluorescence values are square-root
                    transformed, should be used for the analysis because only
                    after outlier removal and sqrt transformation, the
                    residuals of the mixed ANOVA are normally distributed
                    and have homogeneous variances among treatment groups")
        collection <- ana(wt_outlier,
                          mixedaov = mixedaov, dagostino = dagostino)
        collection <- append(collection, list(
          "MixedAnova" = mixedaov, "Justify" = justify
        ))
        return(collection)
      }
    }

    if (boxcox) {
      bc <- MASS::boxcox(wt_outlier$Fluor ~ wt_outlier$Conc)
      lambda <- bc$x[which.max(bc$y)]
      if (row) {
        mixedaov <- tryCatch(
          lme4::lmer(
            (
             Fluor^lambda - 1) / lambda ~ Conc +
              (1 | Replicate) + (1 | Replicate:Conc) +
              (1 | Replicate:Conc:Row),
            data = wt_outlier, REML = TRUE
          ),
          error = function(e) {
            FALSE
          }
        )
      } else {
        mixedaov <- tryCatch(
          lme4::lmer(
            (Fluor^lambda - 1) /
              lambda ~ Conc +
              (1 | Replicate) + (1 | Replicate:Conc),
            data = wt_outlier, REML = TRUE
          ),
          error = function(e) {
            FALSE
          }
        )
      }
      if (isS4(mixedaov)) {
        if (
            anova_assumpts(
              dagostino = dagostino, mixedaov = mixedaov, alpha = alpha
            )) {
          justify <- ("The data from which outliers were removed
                      with the Tukey-rule where the fluorescence
                      values are box-cox transformed, should be
                      used for the analysis because only after
                      outlier removal and box-cox transformation,
                      the residuals of the mixed ANOVA are normally
                      distributed and have homogeneous variances
                      among treatment groups")
          collection <- ana(
            wt_outlier,
            mixedaov = mixedaov, dagostino = dagostino
          )
          collection <- append(collection, list(
            "MixedAnova" = mixedaov, "Justify" = justify
          ))
          return(collection)
        }
      }
    }
  }
  if (artc) {
    justify <- ("No transformation and/or outlier removal
                was successful in fulfilling the ANOVA
                assumptions. Therefore, the ART-C
                transformation was conducted which renders
                the analysis non-parametric. Normality of
                residuals and homogeneous variances among
                treatment groups is no longer necessary.
                This procedure is experimental.")
    dataframe_nona <- dataframe[!is.na(dataframe$Fluor), ]
    if (row) {
      mixedaov <- tryCatch(
        ARTool::artlm(ARTool::art(
          Fluor ~ Conc + (1 | Replicate) +
            (1 | Replicate:Conc) +
            (1 | Replicate:Conc:Row),
          data = dataframe_nona
        ), "Conc"),
        error = function(e) {
          FALSE
        }
      )
    } else {
      mixedaov <- tryCatch(
        ARTool::artlm(ARTool::art(
          Fluor ~ Conc + (1 | Replicate) +
            (1 | Replicate:Conc),
          data = dataframe_nona
        ), "Conc"),
        error = function(e) {
          FALSE
        }
      )
    }
    if (isS4(mixedaov)) {
      collection <- ana(
        dataframe_nona,
        mixedaov = mixedaov,
        dagostino = dagostino
      )
      collection <- append(collection, list(
        "MixedAnova" = mixedaov, "Justify" = justify
      ))
      return(collection)
    } else {
      collection <- ("No transformation or data processing
                     procedure could be identified which
                     normalize the residuals and homogenize
                     the variances across treatment groups.
                     Please consider support from an expert
                     statistician.")
      return(collection)
    }
  } else {
    collection <- ("No transformation or data processing
                   procedure could be identified which
                   normalize the residuals and homogenize
                   the variances across treatment groups.
                   Please consider support from an expert
                   statistician.")
    return(collection)
  }
}

#' validity: This function regards the validity criterium in the
#' guideline according to which the CV in each run and over all
#' runs is not suposed to be higher than 30\%.
#' @description With this function the Coefficient of Variation
#' of the test medium control/ solvent control per run and
#' overall is determined and when it is over 30\% trimming is
#' conducted and it is decided whether data transformation is
#' necessary using the Ana() function. When the Coefficient of
#' Variation is smaller than 30\%, then this function calls the
#' data_prep() function.
#'
#' @param dataframe the raw dataframe that contains both spiked,
#' unspiked data, including the T4 control
#' @param dat_spiked_unspiked either the dataframe with the
#' spiked or unspiked samples
#' @param row Row Default is TRUE, which means that by default
#' the full model as specified in the Guideline is used. If it
#' is FALSE, the reduced model is selected whereby the
#' variability in the exposure wells is disregarded.
#' @param alpha Numeric. Default is 0.01. This value is the
#' alpha value for the normality and variance homogeneity test.
#' @param trimming boolean. When TRUE, trimming is conducted by
#'  the dataPrep function when required. When FALSE trimming
#'  is never conducted. Default is TRUE.
#' @param outlier boolean, When TRUE, outlier removal is
#'  conducted by the dataPrep function when required. When
#'  FALSE, outlier removal is never conducted. Default is TRUE.
#' @param boxcox boolean, When TRUE boxcox transformation is
#'  performed by the dataPrep function when necessary and
#'  when no zeros are present in the data. When FALSE it will
#'  never be conducted. Default is TRUE.
#' @param artc boolean, When TRUE ART-C transformation is
#'  performed when necessary. When FALSE it will never be
#'  conducted. Default is FALSE.
#'  This feature is experimental!
#' @param dagostino boolean. Default is FALSE; when FALSE,
#'  the shapiro wilks test from the stats package is carried
#'  out, when TRUE the dAgostina Pearson normality test from
#'  the fBasics test is carried out.
#'
#' @return A list (collection) containing the trimmed or
#'  untrimmed data frame, two summary data frames containing
#'  information summarized based on the concentration and
#'  the replicate, the result from the shapiro and levene
#'  test, the boxplots per replicate, the result of the
#'  monotonicity test as well as the result from the
#'  Dunnett's and Williams test. Different from the
#'  output of the Ana function, the returned list also
#'  contains the mixed Anova model (mixedaov) whereby Fluor
#'  is transformed as recommended and the trimmed or
#'  untrimmed data was utilized. Furthermore, the output
#'  also contains a reasoning for the recommended
#'  transformation and trimming.
#' @export
#' @importFrom rlang .data
#'
#' @examples

#' #data from the validity studies
#' Result1_1 <- validity(xeredar::valid_data_xeta[["triac_japan_combined"]],
#'  xeredar::valid_data_xeta[["triac_japan_spiked"]], row = FALSE)
#' Result1_2 <- validity(xeredar::valid_data_xeta[["triac_japan_combined"]],
#'  xeredar::valid_data_xeta[["triac_japan_unspiked"]], row = FALSE)
#'
#' Result2_1 <- validity(xeredar::valid_data_xeta[["triac_france_combined"]],
#'  xeredar::valid_data_xeta[["triac_france_spiked"]], row = FALSE)
#' Result2_2 <- validity(xeredar::valid_data_xeta[["triac_france_combined"]],
#'  xeredar::valid_data_xeta[["triac_france_unspiked"]], row = FALSE)
#'
#' Result3_1 <- validity(xeredar::valid_data_xeta[["triac_usa_combined"]],
#'  xeredar::valid_data_xeta[["triac_usa_spiked"]], row = FALSE)
#' Result3_2 <- validity(xeredar::valid_data_xeta[["triac_usa_combined"]],
#'  xeredar::valid_data_xeta[["triac_usa_unspiked"]], row = FALSE)
validity <- function(dataframe, dat_spiked_unspiked, row = TRUE,
                     dagostino = FALSE, trimming = TRUE,
                     outlier = TRUE, boxcox = TRUE,
                     artc = FALSE, alpha = 0.01) {
  validate_dfxeta(df = dataframe, row = row)
  validate_dfxeta(df = dat_spiked_unspiked, row = row)
  if (
    nrow(
      dataframe |>
        dplyr::filter(.data$Treatment == "FETAX" | .data$Treatment == "0") |>
        dplyr::group_by(.data$Treatment, .data$Replicate) |>
        dplyr::summarize(
          Mean = mean(.data$Fluor, na.rm = TRUE),
          stdev = stats::sd(.data$Fluor, na.rm = TRUE)
        ) |>
        dplyr::mutate(CV = .data$stdev / .data$Mean) |>
        dplyr::filter(.data$CV > 0.3)
    ) > 0 ||
      dataframe |>
        dplyr::filter(.data$Treatment == "FETAX" | .data$Treatment == "0") |>
        dplyr::summarize(CV = stats::sd(.data$Fluor, na.rm = TRUE) /
                           mean(.data$Fluor, na.rm = TRUE)) > 0.3
  ) {
    dat_trim <- trim(dat_spiked_unspiked)[["dat_trim"]]

    output <- data_prep(dat_trim, row = row, dagostino = dagostino,
                        trimming = FALSE, outlier = FALSE,
                        boxcox = boxcox, artc = artc, alpha = alpha)
    if (is.list(output)) {
      output$Justify <- paste("Trimming was conducted because the coefficient
    of variation of the test medium control is higher than 30%.",
                              output$Justify)
    } else {
      output <- paste("Trimming was conducted because the coefficient
    of variation of the test medium control is higher than 30%.", output)
    }

    return(output)

  } else {
    output <- data_prep(dat_spiked_unspiked,
      row = row, dagostino = dagostino,
      trimming = trimming, outlier = outlier,
      boxcox = boxcox, artc = artc, alpha = alpha
    )
    return(output)
  }
}






#' Function for summary of results
#'
#' @param datalist the list that is provided by the
#'  analysis of either spiked or unspiked data
#'
#' @return A summary file
#' @importFrom rlang .data
#' @export
#' @examples
#' Result1_1 <- validity(xeredar::valid_data_xeta[["triac_japan_combined"]],
#'   xeredar::valid_data_xeta[["triac_japan_unspiked"]],
#'   row = FALSE
#' )
#' data_summary(Result1_1)
#'
#' Result1_2 <- validity(xeredar::valid_data_xeta[["triac_japan_combined"]],
#'   xeredar::valid_data_xeta[["triac_japan_spiked"]],
#'   row = FALSE, alpha = 0.005
#' )
#' data_summary(Result1_2)
#'
#' Result2_1 <- validity(xeredar::valid_data_xeta[["triac_france_combined"]],
#'   xeredar::valid_data_xeta[["triac_france_unspiked"]],
#'   row = FALSE
#' )
#' data_summary(Result2_1)
#'
#' Result2_2 <- data_prep(dataframe =
#' xeredar::valid_data_xeta[["triac_france_spiked"]], row = FALSE)
#' data_summary(Result2_2)
#'
#' Result3_1 <- data_prep(dataframe =
#' xeredar::valid_data_xeta[["t4_japan_spiked"]], row = FALSE)
#' data_summary(Result3_1)
#'
#' Result3_2 <- data_prep(dataframe =
#' xeredar::valid_data_xeta[["ptu_usa_spiked"]], row = FALSE)
#' data_summary(Result3_2)
data_summary <- function(datalist) {
  datalist$SummaryDF_Rep$Replicate <- paste0(
    "Replicate ", as.character(datalist$SummaryDF_Rep$Replicate)
  )
  datalist$SummaryDF_Rep <- as.data.frame(datalist$SummaryDF_Rep)

  datalist$SummaryDF_Rep <- datalist$SummaryDF_Rep |>
    dplyr::group_by(.data$Replicate) |>
    dplyr::mutate(
      PercIncreas = 100 * .data$Mean /
        .data$Mean[.data$Conc == "0" | .data$Conc == "FETAX"] - 100
    )

  datalist$SummaryDF <- data.frame(datalist$SummaryDF)
  datalist$SummaryDF <- datalist$SummaryDF |>
    dplyr::mutate(Replicate = "Pooled", .after = "Conc") |>
    dplyr::mutate(
      PercIncreas = 100 * .data$Mean /
        .data$Mean[.data$Conc == "0" | .data$Conc == "FETAX"] - 100
    )

  finaldatalist <- data.frame(rbind(
    datalist$SummaryDF_Rep[, c("Conc", "Replicate", "PercIncreas")],
    datalist$SummaryDF[, c("Conc", "Replicate", "PercIncreas")]
  ))
  finaldatalist <- finaldatalist[which(finaldatalist$Conc != 0), ]

  finaldatalist1 <- stats::reshape(
    finaldatalist,
    idvar = "Replicate",
    timevar = "Conc", direction = "wide"
  )
  rownames(finaldatalist1) <- finaldatalist1[, 1]
  finaldatalist1 <- round(finaldatalist1[, -1], 2)

  dunnett_p_datalist <- with(
    as.data.frame(datalist$Dunnetts),
    ifelse(`adj p` <= 0.05, "*",
      ifelse(`adj p` > 0.05 & `adj p` < 0.1, ".", "ns")
    )
  )

  if (length(unique(finaldatalist$Conc)) < 3) {
    message("Warning: Williams' test is not appropriate,
            as there are insufficient treatment levels
            (< 4, including control) to reveal a
            reliable monotonic dose response and
            to perform Williams' test")
    increasing_p_datalist <- rep("n.d.", length(unique(finaldatalist$Conc)))
    decreasing_p_datalist <- rep("n.d.", length(unique(finaldatalist$Conc)))
  } else {
    increasing_p_datalist <- datalist$WilliamsIncrease[
      rev(seq_len(nrow(datalist$WilliamsIncrease))), "Sign"
    ]
    decreasing_p_datalist <- datalist$WilliamsDecrease[
      rev(seq_len(nrow(datalist$WilliamsDecrease))), "Sign"
    ]
  }

  finaldatalist2 <- data.frame(t(data.frame(
    Dunnett = dunnett_p_datalist,
    IncreasingWilliams = increasing_p_datalist,
    DecreasingWilliams = decreasing_p_datalist
  )))
  names(finaldatalist1) <- unique(finaldatalist$Conc)

  names(finaldatalist2) <- names(finaldatalist1)
  finaldatalist0 <- rbind(finaldatalist1, finaldatalist2)

  finaldatalist0[finaldatalist0 == "FALSE"] <- "ns"
  finaldatalist0[finaldatalist0 == "TRUE"] <- "*"
  return(finaldatalist0)
}
