#' trim function
#' @importFrom rlang .data
#' @description Computes quantiles for each Treatment x Replicate combination
#'   and trim all observations with Flour greater than the quantile tresholds.
#' @param dataframe A data frame with columns "Treatment", "Replicate", "Fluor"
#' @param lwr lower percentile treshold
#' @param upr upper percentile treshold
#' @return Returns a list containing two data frames with the trimmed out
#'   measurements (trimmed_out) and the trimmed data (dat_trim)
#' @export
#' @examples
#' trimmedDataList <- trim(xeredar::testDataSpiked)
trim <- function(dataframe, lwr = 0.1, upr = 0.9) {
  stopifnot(lwr >= 0 && lwr <= 1)
  stopifnot(upr >= 0 && upr <= 1)

  validate_dfxeta(df = dataframe, row = FALSE)

  # quantiles for each treatment x Replicate
  thrs <- dataframe |>
    dplyr::group_by(.data$Treatment, .data$Replicate) |>
    dplyr::summarise(
      thrs = stats::quantile(.data$Fluor,
        probs = c(lwr, upr),
        type = 2
      ), # default quantile type = 2 similar to SAS
      quantile = c("lwr", "upr")
    ) |>
    tidyr::pivot_wider(names_from = "quantile", values_from = "thrs")

  # Indicate which observations are outside of the quantiles
  res <- dataframe |>
    dplyr::left_join(thrs, by = c("Treatment", "Replicate")) |>
    dplyr::mutate(is_within = .data$Fluor >= lwr & .data$Fluor <= upr)

  # trim
  dat_trim <- res |>
    dplyr::filter(.data$is_within) |>
    dplyr::select(names(dataframe))

  trimmed_out <- res |>
    dplyr::filter(!.data$is_within) |>
    dplyr::select(names(dataframe))

  out <- list(dat_trim = dat_trim, trimmed_out = trimmed_out)
  return(out)
}


#' rm_outlier Function to remove outliers based on the Tukey outlier rule.
#'
#' @param dataframe The untrimmed data frame with columns
#'   "Treatment", "Replicate", "Fluor", "Row", "Well", "Conc", "Dose".
#'   The values that were spiked must contain a "T3" in the Treatment name.
#'   The T4 control must be named FETAXT3T4. Take care that you do not contain
#'   any spaces in the treatment names!
#' @param row Please specify FALSE (boolean) in case you did not record the
#'   exposure well ID/ Row of the 96 well plate or you want to use the reduced
#'   mixed ANOVA model where the exposure well ID is not a random effect.
#' @return Returns a list containing two data frames with the measurements
#'   identified as outliers (outliers) and another data frame containing the
#'   remaining measurements (wt_outlier).
#' @export
rm_outlier <- function(dataframe, row = TRUE) {
  validate_dfxeta(df = dataframe, row = FALSE)

  if (row) {
    mixed_anova <- lme4::lmer(
      Fluor ~ Conc +
        (1 | Replicate) + (1 | Replicate:Conc) + (1 | Replicate:Conc:Row),
      data = dataframe, REML = TRUE
    )
  } else {
    mixed_anova <- lme4::lmer(Fluor ~ Conc +
                                (1 | Replicate) + (1 | Replicate:Conc),
                              data = dataframe, REML = TRUE)
  }
  residuals <- data.frame("Residuals" = stats::resid(mixed_anova)) |>
    dplyr::bind_cols(dataframe)

  quantiles <- stats::quantile(residuals$Residuals,
    probs = seq(0.25, 0.75, 0.5), type = 2
  )
  iqr <- stats::IQR(residuals$Residuals, type = 2)
  lb <- quantiles[1] - iqr * 1.5
  ub <- quantiles[2] + iqr * 1.5
  outliers <- residuals |>
    dplyr::filter(.data$Residuals <= lb | .data$Residuals >= ub) |>
    dplyr::mutate("lb" = lb, "ub" = ub)

  wt_outlier <- dataframe |> dplyr::anti_join(outliers)

  out <- list(wt_outlier = wt_outlier, outliers = outliers)
  out
}
