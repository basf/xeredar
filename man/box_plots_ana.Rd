% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XETAfunctions.R
\name{box_plots_ana}
\alias{box_plots_ana}
\title{box_plots_ana: internal function used by the function ana() to
create boxplots}
\usage{
box_plots_ana(df)
}
\arguments{
\item{df}{A dataframe containing only the original spiked or unspiked
samples without T4 control.'}
}
\value{
A figure
#' @importFrom rlang .data
}
\description{
This function is used by the ana() function to
 create replicate specific boxplots.
}
\examples{
box_plots_ana(df =xeredar::valid_data_xeta[["triac_france_spiked"]])
}
