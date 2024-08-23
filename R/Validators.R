#' Valdidating XETA dataframe
#'
#' @param df The dataframe that is used in the parent function
#' @param row logical. Please specify FALSE (boolean) in case you did not record
#'   the exposure well ID/ Row of the 96 well plate or you want to use the
#'   reduced mixed ANOVA model where the exposure well ID is not a random
#'   effect.
#' @return In case the dataframe is valid,
#'   the function invisibly returns it to the parent function
#' @importFrom methods is
#' @export
#' @examples
#' validate_dfxeta(xeredar::valid_data_xeta[["triac_japan_unspiked"]],
#'  row = FALSE)
validate_dfxeta <- function(df, row = TRUE) {
  stopifnot(is.logical(row))

  stopifnot(is.data.frame(df))

  if (!all(c("Replicate", "Treatment", "Fluor", "Conc") %in% colnames(df))) {
    stop(
      "The supplied dataframe either is not a dataframe or
      it does not have the correct column names.
      The column names of the dataframe should be:
      Replicate, Treatment, Fluor, Conc",
      call. = FALSE
    )
  }
  stopifnot(is.character(df$Replicate) | is.factor(df$Replicate))
  stopifnot(is.character(df$Treatment) | is.factor(df$Treatment))
  stopifnot(is.numeric(df$Fluor))
  stopifnot(is.ordered(df$Conc))

  if (row) {
    if (!"Row" %in% colnames(df)) {
      stop("The supplied dataframe does not contain a column named Row",
        call. = FALSE
      )
    }
    if (!is.character(df$Row) && !is.factor(df$Row)) {
      stop("The Row column needs to contain character or factor values.",
        call. = FALSE
      )
    }
  }
}

#' Validate if model is as expected
#'
#' @param mixedaov model to validate
#' @return In case the mixed ANOVA model is valid,
#'   the function invisibly returns it to the parent function
#' @importFrom methods is
#' @export
#' @examples
#' mixedaov <- lme4::lmer(
#'   Fluor ~ Conc +
#'     (1 | Replicate) + (1 | Replicate:Conc) + (1 | Replicate:Conc:Row),
#'   data = xeredar::testDataSpiked, REML = TRUE
#' )
#' validate_mixedanova(mixedaov = mixedaov)
validate_mixedanova <- function(mixedaov) {
  stopifnot(inherits(mixedaov, "lmerMod"))
  stopifnot(all(c("Replicate", "Conc") %in%
                  colnames(mixedaov@frame)))
}
