#' rankTransform
#' @description This is a function originally encoded by Joe Swintek for the
#'  R package StatCharrms. It Transforms a response by rank into critical values
#'  of the standard normal distribution. In the case of ties this function will
#'  use the mean of the transformed response.
#'  This function will rank transform the data the
#'  same way the SAS code below does:
#'  proc rank data=sorted out=assumtst normal=blom
#'  ties=mean;
#'  var Counts;
#'  by generation;
#'  ranks rank_EGGS;
#'  run;
#'
#' @param Data A dataset
#' @param VecName The name (as a string) of the response to be transformed.
#'
#' @return The original data set with a new variable called
#' 'TransformedResponse' which is the rank transform of
#'  the response.
#' @export
#' @author Joe Swintek
#' @references \url{https://www.rdocumentation.org/packages/StatCharrms/versions/0.90.96}
#' @examples
#' data(lengthWeightData)
#' SubData <- lengthWeightData[lengthWeightData$Age == "16 week", ]
#' SubData <- SubData[SubData$Generation == "F1", ]
#' SubData <- SubData[SubData$SEX == "M", ]
#' RankData <- rankTransform(Data = SubData, VecName = "WEIGHT")
#' head(RankData)
rankTransform <-
  function(Data, VecName) {
    Vector <- Data[, VecName]

    # This handles the rank transform
    n <- length(Vector)
    r.i <- rank(Vector, na.last = TRUE, ties.method = c("random"))
    y.i <- (r.i - 3 / 8) / (n + 1 / 4)
    Data$y.i.norm <- stats::qnorm(y.i) # this will be for output
    Data <- Data[with(Data, order(Data$y.i.norm)), ]


    Repeats <- stats::xtabs(~ as.factor(Data[, VecName]))
    # This Block of code is for finding ties and will break if no ties exist
    if (max(Repeats) > 1) { # Check for ties
      Repeats <- Repeats[-which(Repeats == 1)] # Remove non-ties
      Names <- dimnames(Repeats)
      for (e in Names[[1]]) { # For every number that has a tie
        e <- as.numeric(e)
        Data[which(Data[, VecName] == e), "y.i.norm"] <-
          mean(Data[which(Data[, VecName] == e), "y.i.norm"]) # Average
      }
    }
    colnames(Data)[which(colnames(Data) == "y.i.norm")] <-
      "TransformedResponse" # rename column

    return(Data)
  }



#' getLineContrast
#' @description This is a function originally encoded by Joe Swintek
#'  for the R package StatCharrms. Generates linear contrasts for
#'  testing monotonicity based on the OECD guidance No. 54 for statistics
#'  in ecotoxicology.
#' @param Data A data set.
#' @param Treatment The name (as a string) of the treatment variable.
#'
#' @return Returns a numeric vector of contrasts.
#' @export
#' @author Joe Swintek
#' @references \url{https://www.rdocumentation.org/packages/StatCharrms/versions/0.90.96}
#' @references OECD (2006) Current approaches in the statistical analysis of
#' ecotoxicity data: A guidance to application. Environmental Health and
#' Safety Publications. Series on Testing and Assessment. No. 54. Paris.

getLineContrast <-
  function(Data, Treatment) {
    # This attains the contrast used to test a linear relationship
    K <- nlevels(as.factor(Data[, Treatment]))

    # Contrasts
    switch(K,
      return(0),
      return(c(-1, 1)),
      return(c(-1, 0, 1)),
      return(c(-3, -1, 1, 3)),
      return(c(-2, -1, 0, 1, 2)),
      return(c(-5, -3, -1, 1, 3, 5)),
      return(c(-3, -2, -1, 0, 1, 2, 3)),
      return(c(-7, -5, -3, -1, 1, 3, 5, 7)),
      return(c(-4, -3, -2, -1, 0, 1, 2, 3, 4)),
      return(c(-9, -7, -5, -3, -1, 1, 3, 5, 7, 9)),
    )
    return()
  }



#' getQuadContrast
#' @description This is a function originally encoded by
#' Joe Swintek for the R package StatCharrms. Generates
#' quadratic contrasts for testing monotonicity based on
#' the OECD guidance No. 54 for statistics in ecotoxicology.
#' @param Data A data set.
#' @param Treatment The name (as a string) of the treatment variable.
#'
#' @return Returns a numeric vector of contrasts.
#' @export
#' @author Joe Swintek
#' @references \url{https://www.rdocumentation.org/packages/StatCharrms/versions/0.90.96}
#' @references OECD (2006) Current approaches in the
#' statistical analysis of ecotoxicity data: A guidance
#' to application. Environmental Health and Safety
#' Publications. Series on Testing and Assessment.
#' No. 54. Paris.

getQuadContrast <- function(Data, Treatment) {
  # This attains the contrast used to test a quadratic relationship
  K <- nlevels(as.factor(Data[, Treatment]))

  switch(K,
    return(0),
    return(c(0, 0)),
    return(c(1, -2, 1)),
    return(c(1, -1, -1, 1)),
    return(c(2, -1, -2, -1, 2)),
    return(c(5, -1, -4, -4, -1, 5)),
    return(c(5, 0, -3, -4, -3, 0, 5)),
    return(c(7, 1, -3, -5, -5, -3, 1, 7)),
    return(c(28, 7, -8, -17, -20, -17, -8, 7, 28)),
    return(c(6, 2, -1, -3, -4, -4, -3, -1, 2, 6)),
  )
  return()
}




#' monotonicityTest: A test for a monotonic trend
#' as conducted originally in StatCharrms
#' @description This is a function originally encoded
#' by Joe Swintek for the R package StatCharrms.
#' Performs the test for monotonicity as described
#' in the OECD guidance No. 54 for statistics in ecotoxicology.
#' This is the test for monotonicity as done in the SAS Version
#' it sets up linear and Quadratic contrast for an ANOVA
#' Uses .stdEndEnv$WeightVar
#' @param Data A data set.
#' @param Treatment The name (as a string) of the treatment variable.
#' @param Response The name (as a string) of the response variable.
#'
#' @return A table of test statistics for both the
#' linear and quadratic trends.
#' @references \url{https://www.rdocumentation.org/packages/StatCharrms/versions/0.90.96}
#' @references OECD (2006) Current approaches in the
#' statistical analysis of ecotoxicity data: A
#' guidance to application. Environmental Health
#' and Safety Publications. Series on Testing and
#' Assessment. No. 54. Paris.
#' @details Calls getLineContrast and getQuadContrast
#' to attain the contrasts and rankTransform for the
#' monotonicity test.
#' @export
#' @author Joe Swintek
#' @examples
#' data(lengthWeightData)
#' SubData <- lengthWeightData[lengthWeightData$Age == "8 week", ]
#' SubData <- SubData[SubData$Generation == "F1", ]
#' SubData <- SubData[SubData$SEX == "M", ]
monotonicityTest <-
  function(Data, Treatment, Response) {
    set.seed(123)

    # Transform data for rank response
    Data <- as.data.frame(Data)
    Data <- rankTransform(Data, Response)


    # Form Contrasts
    LineContrast <- getLineContrast(Data, Treatment)
    QuadContrast <- getQuadContrast(Data, Treatment)
    Contrasts <- cbind(LineContrast, QuadContrast)
    colnames(Contrasts) <- c("Line", "QuadContrast")

    # ANOVA
    Data[, Treatment] <- as.factor(Data[, Treatment])
    stats::contrasts(Data[, Treatment]) <- Contrasts
    AnovaTable <- stats::aov(Data[, "TransformedResponse"] ~
                               as.factor(Data[, Treatment]))
    # if (is.null(.stdEndEnv$WeightVar)==FALSE){  #if there
    # is a weight and the data is not averaged
    # if (length(.stdEndEnv$WeightVar)==length(Data[ ,Response])){
    # AnovaTable<-aov(Data[ ,'TransformedResponse'] ~
    # as.factor(Data[, Treatment]), weight = .stdEndEnv$WeightVar)
    # }
    # }

    # gather information and clean the table
    CAnova <- stats::summary.lm(AnovaTable)
    MonocityTable <- as.data.frame(CAnova$coefficients[2:3, 3:4])
    rownames(MonocityTable) <- NULL
    MonocityTable <- cbind(c("Linear", "Quadratic"), MonocityTable, ".")
    colnames(MonocityTable)[1] <- "Test"
    colnames(MonocityTable)[4] <- "Significance"
    MonocityTable$Significance <- as.character(MonocityTable$Significance)

    MonocityTable$Significance[MonocityTable[, "Pr(>|t|)"] < 0.05] <- "*"
    MonocityTable$Significance[MonocityTable[, "Pr(>|t|)"] < 0.01] <- "**"
    MonocityTable$Significance[MonocityTable[, "Pr(>|t|)"] < 0.001] <- "***"

    MonocityTable[, "Pr(>|t|)"] <- round(MonocityTable[, "Pr(>|t|)"], 4)
    if (length(which(MonocityTable[, "Pr(>|t|)"] < 10^-4)) > 0) {
      MonocityTable[which(MonocityTable[, "Pr(>|t|)"] < 10^-4), "Pr(>|t|)"] <-
        "<0.0001"
    }
    MonocityTable[, "t value"] <- round(MonocityTable[, "t value"], 2)

    return(MonocityTable)
  }



#' williamsTest: Performs Williams Test
#' @description This function is originally written by
#' Joe Swintek and encodes the Williams test used in
#' ecotoxicology. It is recommended in most ecotoxicological
#' guidelines and the OECD Guidance Document No. 54 on
#' statistics in ecotoxicology. A detailed description
#' is provided in the book of John Green (Green, 2018).
#' @param df 	The data frame; Each row is an observation.
#' @param resp The name (as a string) of the response variable.
#' @param trt The name (as a string) of the treatment variable.
#' @param direction The direction of the test statistic which
#' can either be "increasing", "decreasing").
#' @param SeIn The Standard error, default is program selected
#' however WilliamsTest can take in a different value in the
#' case of repeated measures
#' @return A dataframe with the following columns
#'  trt The treatment level the test statistic corresponds to.
#'  Y.Tilde The amalgamated averages for the treatment level
#'  Se_Diff The standard error.
#'  DF The degrees of freedom.
#'  Will The value of the Williams test statistic
#'  TCrit The critical value of the Williams test statistic,
#'  corresponding to a p-value of 0.05.
#' @export
#' @references \url{https://www.rdocumentation.org/packages/StatCharrms/versions/0.90.96}
#' @references Green J.W., Springer T.A., Holbech H. (2018)
#' Statistical analysis of ecotoxicity studies.
#' First edition, John Wiley & Sons, Inc.
#' @references OECD (2006) Current approaches in the statistical
#' analysis of ecotoxicity data: A guidance to application.
#' Environmental Health and Safety Publications. Series on
#' Testing and Assessment. No. 54. Paris.
#' @references Williams D.A. (1971). A test for differences
#' between treatment means when several dose levels are
#' compared with a zero dose control. Biometrics 27(1):103-117.
#' @examples
#' data(lengthWeightData)
#' SubData <- lengthWeightData[lengthWeightData$Age == "16 week", ]
#' SubData <- SubData[SubData$Generation == "F1", ]
#' SubData <- SubData[SubData$SEX == "M", ]
#' williamsTest(
#'   df = SubData, trt = "Treatment",
#'   resp = "WEIGHT", direction = "decreasing", SeIn = NULL
#' )
williamsTest <- function(df, resp, trt, direction = "decreasing", SeIn = NULL) {
  # Adjust for test direction synonyms
  direction <- toupper(direction)

  if (direction == "DESCENDING") {
    direction <- "DECREASING"
  }

  if (direction == "INCREASING") {
    direction <- "INCREASING"
  }

  if (is.element(direction, c("DECREASING", "INCREASING")) == FALSE) {
    message("Please enter either decreasing or increasing for the
            direction.\n Function ending.")
    return()
  }

  # Load look up table
  DTtable <- xeredar::williamsTestLookUpTable

  # Test for direction

  # get means
  df[[trt]] <- as.factor(df[[trt]])


  # remove missing data
  if (length(which(is.na(df[[trt]]) == TRUE)) > 0) {
    df <- df[-which(is.na(df[[trt]]) == TRUE), ]
  }
  if (length(which(is.na(df[[resp]]) == TRUE)) > 0) {
    df <- df[-which(is.na(df[[resp]]) == TRUE), ]
  }

  #
  # attain means
  Means <- by(df[[resp]], df[[trt]], mean, na.rm = TRUE)
  Means <- as.vector(Means)
  Means <- round(Means, 5)
  # attain counts
  Count <- by(df[[resp]], df[[trt]], length)
  Count <- as.vector(Count)

  MeansTable <- data.frame(Means, Count)


  # get N
  N <- MeansTable$Count
  # Initial conditions
  Y.tilde <- MeansTable$Means

  N.tilde <- N
  DF <- sum(N) - length(N)
  if (DF < 5) {
    message("Error: Williams test is not
            appropriate when DF < 5. \n
            Function ending.")
    return()
  }

  # Loop of y.tilde
  Y0 <- Y.tilde[1]
  Y.tilde <- Y.tilde[-1]
  Amalg <- mat.or.vec(length(Y.tilde), 1)
  End <- FALSE
  while (End == FALSE) {
    Y.tildeUp <- Y.tilde[-1]
    Y.tildeDw <- Y.tilde[-length(Y.tilde)]
    Diff <- Y.tildeUp - Y.tildeDw
    if (direction == "INCREASING") {
      First <- which(Diff < 0) # Changes bases on
    }
    if (direction == "DECREASING") {
      First <- which(Diff > 0) # Changes bases on
    }
    if (length(First) == 0) {
      break()
      End <- TRUE
    }
    First <- First[1] + 1
    if (Amalg[First - 1] == 0) { # Not amalgamated yet
      Amalg[(First - 1):First] <- 1
      Combin <- c((First - 1):First)
    } else { # Amalgamates with every adjacent chain
      # Find the chains+
      Amalg[First] <- 1
      Low <- max(which(Amalg[1:(First - 2)] == 0), 0) + 1
      Combin <- c(Low:First)
    }
    Y.tilde[Combin] <- sum(Y.tilde[Combin] * N.tilde[Combin]) /
      sum(N.tilde[Combin])
  }
  Y.tilde <- c(Y0, Y.tilde)
  # T statistic
  Vars <- by(df[[resp]], as.factor(df[[trt]]), stats::var, na.rm = TRUE)
  # Allows user to specify different se
  if (is.null(SeIn) == TRUE) {
    S <- sqrt(sum(Vars * (N - 1)) / (sum(N) - length(N)))
    Se <- (S * sqrt(1 / N + 1 / N[1]))
  } else {
    Se <- SeIn
  }
  Te <- (Y.tilde[1] - Y.tilde) / Se
  if (direction == "INCREASING") {
    Te <- Te * -1
  }
  # Table look up
  K <- length(N) - 1
  Row <- which(DTtable$df == DF)
  if (length(Row) == 0) { # sets the df to 10000 if the df >= 121
    Row <- 119
  }
  Cols <- 1 + 1:(2 * (K - 1))
  B <- DTtable[Row, Cols[which(Cols %% 2 == 1)]]
  Q <- DTtable[Row, Cols[which(Cols %% 2 == 0)]]

  # Calculate t crit
  W <- N[1] / N[-c(1, 2)]
  Tcrit <- unlist(Q - 10^(-2) * B * (1 - 1 / W))
  QT1 <- stats::qt(0.95, DF)
  Tcrit <- c(QT1, Tcrit)



  # compile table
  Out <- data.frame(1:K, Y.tilde[-1], Y.tilde[1], Se[-1], DF, Te[-1], Tcrit)
  # reverse the tables
  Out <- Out[rev(1:K), ]
  # Lable the table with the levels of the treatment
  colnames(Out) <- c(trt, "Y.Tilde", "Y0", "Se Diff", "DF", "Will", "TCrit")
  Sig <- Out$Will > Out$TCrit

  if (length(which(Sig == TRUE)) > 0) {
    Sig[which(Sig == TRUE)] <- "*"
  }
  if (length(which(Sig == FALSE)) > 0) {
    Sig[which(Sig == FALSE)] <- "."
  }
  Out <- data.frame(Out, Sig)
  colnames(Out)[dim(Out)[2]] <- "Signif"

  # Clean by rounding
  Out$Y.Tilde <- signif(Out$Y.Tilde, 6)
  Out$Se.Diff <- signif(Out$Se.Diff, 4)
  Out$Will <- signif(Out$Will, 6)
  Out$TCrit <- signif(Out$TCrit, 4)
  Out[, 1] <- rev(levels(df[[trt]])[-1]) # Exclude the control

  return(Out)
}
