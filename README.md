
# xeredar <a href="https://dplyr.tidyverse.org"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/basf/xeredar/actions/workflows/Package_check.yml/badge.svg)](https://github.com/basf/xeredar/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check](https://github.com/basf/xeredar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/basf/xeredar/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


Background
==========

The package xeredar is an R-package for analysis of the New Approach
Methodology (NAM) assays of XETA (Xenopus Eleutheroembryonic Thyroid),
RADAR (Rapid Androgen Disruption Activity Reporter) and REACTIV (Rapid
Estrogen ACTivity In Vivo) for assessing endocrine effects of chemicals
on the thyroid, androgen/steroid and estrogen axis. The functionality is
based on the SAS-script recommended in the Annex 13 of OECD test
guideline No. 248 of the XETA assay
([2019](https://www.oecd-ilibrary.org/environment/tg-248-xenopus-eleutheroembryonic-thyroid-assay-xeta_a13f80ee-en)),
written by John Green.

Installation
============

You can install xeredar using one of the followig commands:

    devtools::install_github("basf/xeredar")

    pak::pkg_install("basf/xeredar")

Data requirements
=================

Data frames that are supposed to be analyzed with xeredar needs to
fulfill certain requirements. The data frame or tibble needs to contain
the following column headers:


    knitr::kable(head(xeredar::testDataSpiked))

<table>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">Replicate</th>
<th style="text-align: left;">Treatment</th>
<th style="text-align: left;">Row</th>
<th style="text-align: right;">Fluor</th>
<th style="text-align: left;">Conc</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">11</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">0 + T3</td>
<td style="text-align: left;">4</td>
<td style="text-align: right;">19.768</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="even">
<td style="text-align: left;">12</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">0 + T3</td>
<td style="text-align: left;">4</td>
<td style="text-align: right;">27.928</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="odd">
<td style="text-align: left;">13</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">0 + T3</td>
<td style="text-align: left;">4</td>
<td style="text-align: right;">29.592</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="even">
<td style="text-align: left;">14</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">0 + T3</td>
<td style="text-align: left;">4</td>
<td style="text-align: right;">22.816</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="odd">
<td style="text-align: left;">15</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">0 + T3</td>
<td style="text-align: left;">4</td>
<td style="text-align: right;">26.080</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="even">
<td style="text-align: left;">16</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">0 + T3</td>
<td style="text-align: left;">4</td>
<td style="text-align: right;">25.332</td>
<td style="text-align: left;">0</td>
</tr>
</tbody>
</table>

The type of each column should be accordingly:


    knitr::kable(purrr::map_df(xeredar::testDataSpiked, class))

<table>
<thead>
<tr class="header">
<th style="text-align: left;">Replicate</th>
<th style="text-align: left;">Treatment</th>
<th style="text-align: left;">Row</th>
<th style="text-align: left;">Fluor</th>
<th style="text-align: left;">Conc</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">factor</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">numeric</td>
<td style="text-align: left;">ordered</td>
</tr>
<tr class="even">
<td style="text-align: left;">factor</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">numeric</td>
<td style="text-align: left;">factor</td>
</tr>
</tbody>
</table>

**Replicate** (i.e. run), **Treatment** (i.e. a unique name for each
treatment level in either spiked or unspiked mode) and **Row**
(i.e. exposure vessel) can either be *factor* or *character* columns,
but **Fluor** (i.e. measured fluorescence) must always be *numeric* and
**Conc** (i.e. concentration of test item) must always contain *ordered
factors*. The order of the columns is not relevant.

**Replicate**, **Treatment** and **Row** can either be *factor* or
*character* columns, but **Fluor** must always be *numeric* and **Conc**
must always contain *ordered factors*. The order of the columns is not
relevant. It is important that the decimal separator is a period instead
of a comma.

When simply aiming to use the `data_prep()` function, the data frame
needs to either contain spiked treatments or unspiked treatments. When
still having spiked and unspiked treatments in one data frame they
should be separated. Imagine you have a XETA data frame (dat) which
contains spiked and unspiked treatments as well as the T4 positive
control. The T3 or T4 additions are designated by “T3” and “T4” in the
**Treatment** column. The spiked and unspiked datasets could quickly be
subset using the following code:

    datSpiked <- dat[grepl("T3",dat$Treatment),] # spiked data 

    datUnspiked <- setdiff(dat,datSpiked)
    datUnspiked <- datUnspiked[which(datUnspiked$Treatment != "T4"),] # unspiked data

Running default XETA analysis
=============================

To demonstrate how to run XETA analysis, we will use one of the data
sets by the French lab containing the spiked and unspiked measurements
from the XETA ring test as included in OECD test guideline No. 248 of
the XETA assay
([2019](https://www.oecd-ilibrary.org/environment/tg-248-xenopus-eleutheroembryonic-thyroid-assay-xeta_a13f80ee-en)).

    xeta_spiked <- xeredar::valid_data_xeta[["ptu_france_spiked"]]
    xeta_unspiked <- xeredar::valid_data_xeta[["ptu_france_unspiked"]]

The default XETA analysis can be run using the `data_prep()` function
with either spiked or unspiked data. This function automatically decides
whether trimming, outlier removal and/or transformations are conducted
following the manuscript ([Spyridonov et al. 2025](https://academic.oup.com/etc/advance-article/doi/10.1093/etojnl/vgaf056/8046659?searchresult=1)). 
The actual analysis is carried out by the `ana()` function. The `ana()` function is
called by the `data_prep()` function and does not need to be called
separately. For this dataset, the exposure well ID (Row of the 96 well
plate) is not recorded, therefore, we set the `row` argument to `FALSE`.
In this case, we use the reduced mixed ANOVA model where the exposure
well ID is not included as a random effect. Please specify `row=TRUE` if
the exposure well ID is recorded and you want to use the full mixed
ANOVA model.


    xeta_spiked_result <- xeredar::data_prep(dataframe = xeta_spiked, row = FALSE)
    xeta_unspiked_result <- xeredar::data_prep(dataframe = xeta_unspiked, row= FALSE)

Here we use the spike data as an example to demonstrate the output of
the `data_prep()` function.

The outputs of the `data_prep()` function are lists containing the
following elements:

-   A reasoning for the recommended transformation and trimming. The raw
    data should be used for the analysis because the residuals of the
    mixed ANOVA are normally distributed and show homogeneous variances
    among treatment groups.

<!-- -->

    xeta_spiked_result$Justify
    #> [1] "The raw data should be used for the analysis because\n                  the residuals of the mixed ANOVA are normally\n                  distributed and show homogeneous variances\n                  among treatment groups."

-   A data frame of the processed data (e.g. raw data, trimmed,
    transformed, or outlier removed) used for actual statistical testing
    following the reasoning. The box plots of the processed data per
    run/replicate (i.e. each panel represents each run/replicate) are
    also provided for visual inspection.

<!-- -->

    knitr::kable(head(xeta_spiked_result$ProcessedData))

<table>
<thead>
<tr class="header">
<th style="text-align: left;">Replicate</th>
<th style="text-align: left;">Treatment</th>
<th style="text-align: right;">Fluor</th>
<th style="text-align: left;">Conc</th>
<th style="text-align: left;">Country</th>
<th style="text-align: left;">Substance</th>
<th style="text-align: left;">Spiked</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">FETAXT3</td>
<td style="text-align: right;">4989.533</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">france</td>
<td style="text-align: left;">ptu</td>
<td style="text-align: left;">TRUE</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">FETAXT3</td>
<td style="text-align: right;">5002.533</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">france</td>
<td style="text-align: left;">ptu</td>
<td style="text-align: left;">TRUE</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">FETAXT3</td>
<td style="text-align: right;">6331.533</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">france</td>
<td style="text-align: left;">ptu</td>
<td style="text-align: left;">TRUE</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">FETAXT3</td>
<td style="text-align: right;">4645.533</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">france</td>
<td style="text-align: left;">ptu</td>
<td style="text-align: left;">TRUE</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">FETAXT3</td>
<td style="text-align: right;">4977.533</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">france</td>
<td style="text-align: left;">ptu</td>
<td style="text-align: left;">TRUE</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">FETAXT3</td>
<td style="text-align: right;">6229.533</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">france</td>
<td style="text-align: left;">ptu</td>
<td style="text-align: left;">TRUE</td>
</tr>
</tbody>
</table>

    xeta_spiked_result$BoxPlots

![](User_guide_files/figure-markdown_strict/unnamed-chunk-9-1.png)

-   Summary tables of the processed data (per replicate and overall)

<!-- -->

    knitr::kable(xeta_spiked_result$SummaryDF_Rep, caption="Summary statistics of fluorescence in different concentrations of test item per replicate")

<table>
<caption>Summary statistics of fluorescence in different concentrations of test item per replicate</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Conc</th>
<th style="text-align: left;">Replicate</th>
<th style="text-align: right;">N</th>
<th style="text-align: right;">Mean</th>
<th style="text-align: right;">Standard deviation</th>
<th style="text-align: right;">Coefficient of variation</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">0</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">19</td>
<td style="text-align: right;">4651.060</td>
<td style="text-align: right;">944.7420</td>
<td style="text-align: right;">0.2031240</td>
</tr>
<tr class="even">
<td style="text-align: left;">0</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">4024.300</td>
<td style="text-align: right;">603.6731</td>
<td style="text-align: right;">0.1500070</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">19</td>
<td style="text-align: right;">4952.225</td>
<td style="text-align: right;">1179.3597</td>
<td style="text-align: right;">0.2381475</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">19</td>
<td style="text-align: right;">3854.137</td>
<td style="text-align: right;">828.3944</td>
<td style="text-align: right;">0.2149364</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">4557.867</td>
<td style="text-align: right;">558.4921</td>
<td style="text-align: right;">0.1225337</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">19</td>
<td style="text-align: right;">4720.819</td>
<td style="text-align: right;">619.8992</td>
<td style="text-align: right;">0.1313118</td>
</tr>
<tr class="odd">
<td style="text-align: left;">3</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">19</td>
<td style="text-align: right;">4671.428</td>
<td style="text-align: right;">976.9674</td>
<td style="text-align: right;">0.2091368</td>
</tr>
<tr class="even">
<td style="text-align: left;">3</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">19</td>
<td style="text-align: right;">4486.538</td>
<td style="text-align: right;">783.8641</td>
<td style="text-align: right;">0.1747147</td>
</tr>
<tr class="odd">
<td style="text-align: left;">3</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">5421.433</td>
<td style="text-align: right;">703.9490</td>
<td style="text-align: right;">0.1298456</td>
</tr>
<tr class="even">
<td style="text-align: left;">10</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">4573.083</td>
<td style="text-align: right;">946.2029</td>
<td style="text-align: right;">0.2069070</td>
</tr>
<tr class="odd">
<td style="text-align: left;">10</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">19</td>
<td style="text-align: right;">4329.538</td>
<td style="text-align: right;">827.4898</td>
<td style="text-align: right;">0.1911266</td>
</tr>
<tr class="even">
<td style="text-align: left;">10</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">19</td>
<td style="text-align: right;">5106.849</td>
<td style="text-align: right;">876.5940</td>
<td style="text-align: right;">0.1716507</td>
</tr>
<tr class="odd">
<td style="text-align: left;">30</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">5391.183</td>
<td style="text-align: right;">809.5849</td>
<td style="text-align: right;">0.1501683</td>
</tr>
<tr class="even">
<td style="text-align: left;">30</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">19</td>
<td style="text-align: right;">4556.275</td>
<td style="text-align: right;">694.1698</td>
<td style="text-align: right;">0.1523547</td>
</tr>
<tr class="odd">
<td style="text-align: left;">30</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">19</td>
<td style="text-align: right;">5164.691</td>
<td style="text-align: right;">763.8837</td>
<td style="text-align: right;">0.1479050</td>
</tr>
<tr class="even">
<td style="text-align: left;">100</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">19</td>
<td style="text-align: right;">5181.060</td>
<td style="text-align: right;">858.5454</td>
<td style="text-align: right;">0.1657085</td>
</tr>
<tr class="odd">
<td style="text-align: left;">100</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">19</td>
<td style="text-align: right;">5124.381</td>
<td style="text-align: right;">733.4611</td>
<td style="text-align: right;">0.1431317</td>
</tr>
<tr class="even">
<td style="text-align: left;">100</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">6048.083</td>
<td style="text-align: right;">959.5900</td>
<td style="text-align: right;">0.1586602</td>
</tr>
</tbody>
</table>

    knitr::kable(xeta_spiked_result$SummaryDF, caption="Summary statistics of fluorescence in different concentrations of test item of all replicates")

<table>
<caption>Summary statistics of fluorescence in different concentrations of test item of all replicates</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Conc</th>
<th style="text-align: right;">N</th>
<th style="text-align: right;">Mean</th>
<th style="text-align: right;">Standard deviation</th>
<th style="text-align: right;">Coefficient of variation</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">0</td>
<td style="text-align: right;">58</td>
<td style="text-align: right;">4533.593</td>
<td style="text-align: right;">998.2901</td>
<td style="text-align: right;">0.2201984</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: right;">58</td>
<td style="text-align: right;">4380.715</td>
<td style="text-align: right;">764.2147</td>
<td style="text-align: right;">0.1744498</td>
</tr>
<tr class="odd">
<td style="text-align: left;">3</td>
<td style="text-align: right;">58</td>
<td style="text-align: right;">4869.483</td>
<td style="text-align: right;">910.7568</td>
<td style="text-align: right;">0.1870336</td>
</tr>
<tr class="even">
<td style="text-align: left;">10</td>
<td style="text-align: right;">58</td>
<td style="text-align: right;">4668.156</td>
<td style="text-align: right;">928.9066</td>
<td style="text-align: right;">0.1989879</td>
</tr>
<tr class="odd">
<td style="text-align: left;">30</td>
<td style="text-align: right;">58</td>
<td style="text-align: right;">5043.483</td>
<td style="text-align: right;">825.4425</td>
<td style="text-align: right;">0.1636652</td>
</tr>
<tr class="even">
<td style="text-align: left;">100</td>
<td style="text-align: right;">58</td>
<td style="text-align: right;">5461.466</td>
<td style="text-align: right;">945.7370</td>
<td style="text-align: right;">0.1731654</td>
</tr>
</tbody>
</table>

-   Tables of results evaluated using increasing/decreasing Williams
    test and/or Dunnett’s test, if applicable.

In the the Williams’ test result tables, *Y.Tilde* is the amalgamated
mean of the fluorescence in each treatment group, *Y0* is the mean of
the control fluorescence, *DIFF* is the estimated difference between the
treatment and the control, *SE\_DIFF* is the standard error of the
Williams’ test, *DF* is the degrees of freedom for Williams’ test, *WILL
Incr* or *Will Decr* is the Williams’ test statistic, *crit Val* is the
critical value of Williams distribution, *Sign* suggests if there is
significant difference between the treatment and the control, and
*%Incr* is the percent increase of the fluorescence compared to the
control.

In the Dunnett’s test result table, *Estimate* is the estimated
difference between the treatment and the control, *SE* is the standard
error of the mixed ANOVA model, *t value* is the Dunnett’s test
statistic, *adj p* is the adjusted p value and *%Incr* is the percent
increase of the fluorescence compared to the control.

    knitr::kable(xeta_spiked_result$WilliamsIncrease, caption="Increasing Williams' test")

<table>
<caption>Increasing Williams’ test</caption>
<colgroup>
<col style="width: 16%" />
<col style="width: 5%" />
<col style="width: 8%" />
<col style="width: 9%" />
<col style="width: 10%" />
<col style="width: 9%" />
<col style="width: 3%" />
<col style="width: 11%" />
<col style="width: 9%" />
<col style="width: 6%" />
<col style="width: 10%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">Conc</th>
<th style="text-align: right;">Y.Tilde</th>
<th style="text-align: right;">Y0</th>
<th style="text-align: right;">DIFF Incr</th>
<th style="text-align: right;">SE_DIFF</th>
<th style="text-align: right;">DF</th>
<th style="text-align: right;">WILL Incr</th>
<th style="text-align: right;">crit Val</th>
<th style="text-align: left;">Sign</th>
<th style="text-align: right;">% Incr</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Conc100 - Conc0</td>
<td style="text-align: left;">100</td>
<td style="text-align: right;">5461.47</td>
<td style="text-align: right;">4533.593</td>
<td style="text-align: right;">927.8769</td>
<td style="text-align: right;">249.7868</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">3.7146758</td>
<td style="text-align: right;">1.971</td>
<td style="text-align: left;">TRUE</td>
<td style="text-align: right;">20.466613</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc30 - Conc0</td>
<td style="text-align: left;">30</td>
<td style="text-align: right;">5043.48</td>
<td style="text-align: right;">4533.593</td>
<td style="text-align: right;">509.8869</td>
<td style="text-align: right;">249.7868</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">2.0412886</td>
<td style="text-align: right;">1.965</td>
<td style="text-align: left;">TRUE</td>
<td style="text-align: right;">11.246933</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc10 - Conc0</td>
<td style="text-align: left;">10</td>
<td style="text-align: right;">4768.82</td>
<td style="text-align: right;">4533.593</td>
<td style="text-align: right;">235.2269</td>
<td style="text-align: right;">249.7868</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">0.9417108</td>
<td style="text-align: right;">1.956</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">2.968123</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc3 - Conc0</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">4768.82</td>
<td style="text-align: right;">4533.593</td>
<td style="text-align: right;">235.2269</td>
<td style="text-align: right;">249.7868</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">0.9417108</td>
<td style="text-align: right;">1.940</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">7.408918</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc1 - Conc0</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">4380.72</td>
<td style="text-align: right;">4533.593</td>
<td style="text-align: right;">-152.8731</td>
<td style="text-align: right;">249.7845</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.6120201</td>
<td style="text-align: right;">1.908</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-3.372111</td>
</tr>
</tbody>
</table>

    knitr::kable(xeta_spiked_result$WilliamsDecrease, caption="Decreasing Williams' test")

<table style="width:100%;">
<caption>Decreasing Williams’ test</caption>
<colgroup>
<col style="width: 16%" />
<col style="width: 5%" />
<col style="width: 8%" />
<col style="width: 9%" />
<col style="width: 10%" />
<col style="width: 9%" />
<col style="width: 3%" />
<col style="width: 10%" />
<col style="width: 9%" />
<col style="width: 6%" />
<col style="width: 10%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">Conc</th>
<th style="text-align: right;">Y.Tilde</th>
<th style="text-align: right;">Y0</th>
<th style="text-align: right;">DIFF Decr</th>
<th style="text-align: right;">SE_DIFF</th>
<th style="text-align: right;">DF</th>
<th style="text-align: right;">WILL Decr</th>
<th style="text-align: right;">crit Val</th>
<th style="text-align: left;">Sign</th>
<th style="text-align: right;">% Incr</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Conc100 - Conc0</td>
<td style="text-align: left;">100</td>
<td style="text-align: right;">4884.66</td>
<td style="text-align: right;">4533.593</td>
<td style="text-align: right;">-351.0669</td>
<td style="text-align: right;">249.7868</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-1.405466</td>
<td style="text-align: right;">1.971</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">20.466613</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc30 - Conc0</td>
<td style="text-align: left;">30</td>
<td style="text-align: right;">4884.66</td>
<td style="text-align: right;">4533.593</td>
<td style="text-align: right;">-351.0669</td>
<td style="text-align: right;">249.7868</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-1.405466</td>
<td style="text-align: right;">1.965</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">11.246933</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc10 - Conc0</td>
<td style="text-align: left;">10</td>
<td style="text-align: right;">4884.66</td>
<td style="text-align: right;">4533.593</td>
<td style="text-align: right;">-351.0669</td>
<td style="text-align: right;">249.7868</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-1.405466</td>
<td style="text-align: right;">1.956</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">2.968123</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc3 - Conc0</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">4884.66</td>
<td style="text-align: right;">4533.593</td>
<td style="text-align: right;">-351.0669</td>
<td style="text-align: right;">249.7868</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-1.405466</td>
<td style="text-align: right;">1.940</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">7.408918</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc1 - Conc0</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">4884.66</td>
<td style="text-align: right;">4533.593</td>
<td style="text-align: right;">-351.0669</td>
<td style="text-align: right;">249.7845</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-1.405479</td>
<td style="text-align: right;">1.908</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-3.372111</td>
</tr>
</tbody>
</table>

    knitr::kable(xeta_spiked_result$Dunnetts, caption="Dunnett's test")

<table>
<caption>Dunnett’s test</caption>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: right;">Estimate</th>
<th style="text-align: right;">SE</th>
<th style="text-align: right;">df</th>
<th style="text-align: right;">t value</th>
<th style="text-align: right;">adj p</th>
<th style="text-align: right;">% Incr</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Conc1 - Conc0</td>
<td style="text-align: right;">-160.4067</td>
<td style="text-align: right;">249.7845</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.6421806</td>
<td style="text-align: right;">0.9458404</td>
<td style="text-align: right;">-3.372111</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc3 - Conc0</td>
<td style="text-align: right;">320.1063</td>
<td style="text-align: right;">249.7868</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">1.2815181</td>
<td style="text-align: right;">0.5970718</td>
<td style="text-align: right;">7.408918</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc10 - Conc0</td>
<td style="text-align: right;">128.8281</td>
<td style="text-align: right;">249.7868</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">0.5157522</td>
<td style="text-align: right;">0.9769722</td>
<td style="text-align: right;">2.968123</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc30 - Conc0</td>
<td style="text-align: right;">499.2993</td>
<td style="text-align: right;">249.7868</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">1.9989021</td>
<td style="text-align: right;">0.2367993</td>
<td style="text-align: right;">11.246933</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc100 - Conc0</td>
<td style="text-align: right;">911.7088</td>
<td style="text-align: right;">249.7868</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">3.6499482</td>
<td style="text-align: right;">0.0174131</td>
<td style="text-align: right;">20.466613</td>
</tr>
</tbody>
</table>

-   Further information about the normality test (Shapiro-Wilk), the
    homogeneity of variance test (Levene’s test) of residuals of the
    mixed ANOVA model, the monotonicity test and the model fit.

<!-- -->

    xeta_spiked_result$NormalityTest
    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  stats::resid(mixedaov)
    #> W = 0.99186, p-value = 0.05343
    xeta_spiked_result$LeveneTest
    #> # A tibble: 1 × 4
    #>   statistic p.value    df df.residual
    #>       <dbl>   <dbl> <int>       <int>
    #> 1     0.759   0.580     5         342
    xeta_spiked_result$`Monotonicity Test`
    #>        Test t value Pr(>|t|) Significance
    #> 1    Linear    6.49  <0.0001          ***
    #> 2 Quadratic    2.14   0.0335            *
    xeta_spiked_result$MixedAnova
    #> Linear mixed model fit by REML ['lmerMod']
    #> Formula: 
    #> Fluor ~ Conc + (1 | Replicate) + (1 | Replicate:Conc)
    #>    Data: dataframe
    #> REML criterion at convergence: 5608.294
    #> Random effects:
    #>  Groups         Name        Std.Dev.
    #>  Replicate:Conc (Intercept) 241.1   
    #>  Replicate      (Intercept) 350.5   
    #>  Residual                   827.7   
    #> Number of obs: 348, groups:  
    #> Replicate:Conc, 18; Replicate, 3
    #> Fixed Effects:
    #> (Intercept)       Conc.L       Conc.Q       Conc.C  
    #>      4824.2        758.5        264.5         52.6  
    #>      Conc^4       Conc^5  
    #>       149.8       -270.8

*The list output from running the data\_prep() function can be
summarized with the data\_summary() function.*

    xeredar::data_summary(xeta_spiked_result) |> knitr::kable()

<table>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">1</th>
<th style="text-align: left;">3</th>
<th style="text-align: left;">10</th>
<th style="text-align: left;">30</th>
<th style="text-align: left;">100</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Replicate 1</td>
<td style="text-align: left;">-17.13</td>
<td style="text-align: left;">0.44</td>
<td style="text-align: left;">-1.68</td>
<td style="text-align: left;">15.91</td>
<td style="text-align: left;">11.4</td>
</tr>
<tr class="even">
<td style="text-align: left;">Replicate 2</td>
<td style="text-align: left;">13.26</td>
<td style="text-align: left;">11.49</td>
<td style="text-align: left;">7.58</td>
<td style="text-align: left;">13.22</td>
<td style="text-align: left;">27.34</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Replicate 3</td>
<td style="text-align: left;">-4.67</td>
<td style="text-align: left;">9.47</td>
<td style="text-align: left;">3.12</td>
<td style="text-align: left;">4.29</td>
<td style="text-align: left;">22.13</td>
</tr>
<tr class="even">
<td style="text-align: left;">Pooled</td>
<td style="text-align: left;">-3.37</td>
<td style="text-align: left;">7.41</td>
<td style="text-align: left;">2.97</td>
<td style="text-align: left;">11.25</td>
<td style="text-align: left;">20.47</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Dunnett</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">*</td>
</tr>
<tr class="even">
<td style="text-align: left;">IncreasingWilliams</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">*</td>
<td style="text-align: left;">*</td>
</tr>
<tr class="odd">
<td style="text-align: left;">DecreasingWilliams</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
</tr>
</tbody>
</table>

Running default RADAR analysis
==============================

To demonstrate how to run RADAR analysis, we will use one of the data
sets by the Pos\_mDHT\_Fraunhofer\_RADAR containing the spiked and
unspiked measurements from the RADAR study validation in the lab
Fraunhofer with an androgen axis active chemical.

    radar_spiked <- xeredar::RADAR_valid_data_table_spiked_unspiked[["mDHTFRAUNH_Spiked"]]
    radar_unspiked <-  xeredar::RADAR_valid_data_table_spiked_unspiked[["mDHTFRAUNH_Unspiked"]]

The default radar analysis can be run using the `data_prep()` function
with either spiked or unspiked data. This function automatically decides
whether trimming, outlier removal and/or transformations are conducted
following the manuscript (Spyridonov et al. unpublished). The actual
analysis is carried out by the `ana()` function. The `ana()` function is
called by the `data_prep()` function and does not need to be called
separately. the analysis the RADAR assay follows the description of the
Method 2 (the mixed ANOVA approach) in the Annex 8: methods for the
statistical analysis of RADAR assay data of the OECD TG 251
([2022](https://www.oecd-ilibrary.org/environment/test-no-251-rapid-androgen-disruption-activity-reporter-radar-assay_da264d82-en)).
For this dataset, the exposure well ID (Row of the 96 well plate) is not
recorded, therefore, we set the `row` argument to `FALSE`. In this case,
we use the reduced mixed ANOVA model where the exposure well ID is not
included as a random effect. Please specify `row=TRUE` if the exposure
well ID is recorded and you want to use the full mixed ANOVA model.
Trimming is not required.


    radar_spiked_result <- xeredar::data_prep(dataframe = radar_spiked, row = FALSE, trimming=FALSE)
    radar_unspiked_result <- xeredar::data_prep(dataframe = radar_unspiked, row= FALSE, trimming=FALSE)

Here we use the spike data as an example to demonstrate the output of
the `data_prep()` function.

The outputs of the `data_prep()` function are lists containing the
following elements:

-   A reasoning for the recommended transformation and trimming. The raw
    data (without trimming or outlier removal) where the fluorescence
    values are log transformed should be used for the analysis because
    only after log transformation, the residuals of the mixed ANOVA are
    normally distributed and have homogeneous variances among treatment
    groups.

<!-- -->

    radar_spiked_result$Justify
    #> [1] "The raw data (without trimming or outlier removal) where\n                    the fluorescence values are log transformed should be used\n                    for the analysis because only after log transformation,\n                    the residuals of the mixed ANOVA are normally distributed\n                    and have homogeneous variances among treatment groups."

-   A data frame of the processed data (e.g. raw data, transformed, or
    outlier removed) used for actual statistical testing following the
    reasoning. The box plots of the processed data per run/replicate
    (i.e. each panel represents each run/replicate) are also provided
    for visual inspection.

<!-- -->

    knitr::kable(head(radar_spiked_result$ProcessedData))

<table>
<thead>
<tr class="header">
<th style="text-align: left;">lab</th>
<th style="text-align: left;">Compound</th>
<th style="text-align: left;">Treatment</th>
<th style="text-align: right;">Fluor</th>
<th style="text-align: left;">Conc</th>
<th style="text-align: left;">Replicate</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">FRAUNH</td>
<td style="text-align: left;">mDHT</td>
<td style="text-align: left;">FETAX MT</td>
<td style="text-align: right;">113152</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">1</td>
</tr>
<tr class="even">
<td style="text-align: left;">FRAUNH</td>
<td style="text-align: left;">mDHT</td>
<td style="text-align: left;">mDHT-1µg/L + 17MT</td>
<td style="text-align: right;">708</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">1</td>
</tr>
<tr class="odd">
<td style="text-align: left;">FRAUNH</td>
<td style="text-align: left;">mDHT</td>
<td style="text-align: left;">mDHT-2µg/L + 17MT</td>
<td style="text-align: right;">2498</td>
<td style="text-align: left;">2</td>
<td style="text-align: left;">1</td>
</tr>
<tr class="even">
<td style="text-align: left;">FRAUNH</td>
<td style="text-align: left;">mDHT</td>
<td style="text-align: left;">mDHT-4µg/L + 17MT</td>
<td style="text-align: right;">10152</td>
<td style="text-align: left;">4</td>
<td style="text-align: left;">1</td>
</tr>
<tr class="odd">
<td style="text-align: left;">FRAUNH</td>
<td style="text-align: left;">mDHT</td>
<td style="text-align: left;">mDHT-8µg/L + 17MT</td>
<td style="text-align: right;">356</td>
<td style="text-align: left;">8</td>
<td style="text-align: left;">1</td>
</tr>
<tr class="even">
<td style="text-align: left;">FRAUNH</td>
<td style="text-align: left;">mDHT</td>
<td style="text-align: left;">mDHT-16µg/L + 17MT</td>
<td style="text-align: right;">4621</td>
<td style="text-align: left;">16</td>
<td style="text-align: left;">1</td>
</tr>
</tbody>
</table>

    radar_spiked_result$BoxPlots

![](User_guide_files/figure-markdown_strict/unnamed-chunk-17-1.png)

-   Summary tables of the processed data (per replicate and overall)

<!-- -->

    knitr::kable(radar_spiked_result$SummaryDF_Rep, caption="Summary statistics of fluorescence in different concentrations of test item per replicate")

<table>
<caption>Summary statistics of fluorescence in different concentrations of test item per replicate</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Conc</th>
<th style="text-align: left;">Replicate</th>
<th style="text-align: right;">N</th>
<th style="text-align: right;">Mean</th>
<th style="text-align: right;">Standard deviation</th>
<th style="text-align: right;">Coefficient of variation</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">0</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">23818.471</td>
<td style="text-align: right;">43072.456</td>
<td style="text-align: right;">1.808364</td>
</tr>
<tr class="even">
<td style="text-align: left;">0</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">15009.824</td>
<td style="text-align: right;">37062.085</td>
<td style="text-align: right;">2.469189</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">29657.294</td>
<td style="text-align: right;">37428.116</td>
<td style="text-align: right;">1.262021</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">6505.471</td>
<td style="text-align: right;">10835.321</td>
<td style="text-align: right;">1.665571</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">10271.000</td>
<td style="text-align: right;">14904.156</td>
<td style="text-align: right;">1.451091</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">20555.882</td>
<td style="text-align: right;">29472.508</td>
<td style="text-align: right;">1.433775</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">29385.941</td>
<td style="text-align: right;">47143.811</td>
<td style="text-align: right;">1.604298</td>
</tr>
<tr class="even">
<td style="text-align: left;">2</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">8911.118</td>
<td style="text-align: right;">10565.718</td>
<td style="text-align: right;">1.185678</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">41815.706</td>
<td style="text-align: right;">54205.056</td>
<td style="text-align: right;">1.296285</td>
</tr>
<tr class="even">
<td style="text-align: left;">4</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">15968.941</td>
<td style="text-align: right;">33341.972</td>
<td style="text-align: right;">2.087926</td>
</tr>
<tr class="odd">
<td style="text-align: left;">4</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">14532.765</td>
<td style="text-align: right;">21802.622</td>
<td style="text-align: right;">1.500239</td>
</tr>
<tr class="even">
<td style="text-align: left;">4</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">19419.471</td>
<td style="text-align: right;">28057.724</td>
<td style="text-align: right;">1.444824</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">5178.118</td>
<td style="text-align: right;">8983.787</td>
<td style="text-align: right;">1.734952</td>
</tr>
<tr class="even">
<td style="text-align: left;">8</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">8220.882</td>
<td style="text-align: right;">15084.270</td>
<td style="text-align: right;">1.834872</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">16399.647</td>
<td style="text-align: right;">23315.385</td>
<td style="text-align: right;">1.421700</td>
</tr>
<tr class="even">
<td style="text-align: left;">16</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">6119.000</td>
<td style="text-align: right;">8448.481</td>
<td style="text-align: right;">1.380696</td>
</tr>
<tr class="odd">
<td style="text-align: left;">16</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">7955.706</td>
<td style="text-align: right;">15390.405</td>
<td style="text-align: right;">1.934512</td>
</tr>
<tr class="even">
<td style="text-align: left;">16</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">17665.412</td>
<td style="text-align: right;">19842.708</td>
<td style="text-align: right;">1.123252</td>
</tr>
</tbody>
</table>

    knitr::kable(radar_spiked_result$SummaryDF, caption="Summary statistics of fluorescence in different concentrations of test item of all replicates")

<table>
<caption>Summary statistics of fluorescence in different concentrations of test item of all replicates</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Conc</th>
<th style="text-align: right;">N</th>
<th style="text-align: right;">Mean</th>
<th style="text-align: right;">Standard deviation</th>
<th style="text-align: right;">Coefficient of variation</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">0</td>
<td style="text-align: right;">51</td>
<td style="text-align: right;">22828.529</td>
<td style="text-align: right;">38967.63</td>
<td style="text-align: right;">1.706971</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: right;">51</td>
<td style="text-align: right;">12444.118</td>
<td style="text-align: right;">20556.80</td>
<td style="text-align: right;">1.651929</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2</td>
<td style="text-align: right;">51</td>
<td style="text-align: right;">26704.255</td>
<td style="text-align: right;">43299.93</td>
<td style="text-align: right;">1.621462</td>
</tr>
<tr class="even">
<td style="text-align: left;">4</td>
<td style="text-align: right;">51</td>
<td style="text-align: right;">16640.392</td>
<td style="text-align: right;">27641.60</td>
<td style="text-align: right;">1.661115</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8</td>
<td style="text-align: right;">51</td>
<td style="text-align: right;">9932.882</td>
<td style="text-align: right;">17189.94</td>
<td style="text-align: right;">1.730609</td>
</tr>
<tr class="even">
<td style="text-align: left;">16</td>
<td style="text-align: right;">51</td>
<td style="text-align: right;">10580.039</td>
<td style="text-align: right;">15836.94</td>
<td style="text-align: right;">1.496870</td>
</tr>
</tbody>
</table>

-   Tables of results evaluated using increasing/decreasing Williams
    test and/or Dunnett’s test, if applicable.

In the the Williams’ test result tables, *Y.Tilde* is the amalgamated
mean of the fluorescence in each treatment group, *Y0* is the mean of
the control fluorescence, *DIFF* is the estimated difference between the
treatment and the control, *SE\_DIFF* is the standard error of the
Williams’ test, *DF* is the degrees of freedom for Williams’ test, *WILL
Incr* or *Will Decr* is the Williams’ test statistic, *crit Val* is the
critical value of Williams distribution, *Sign* suggests if there is
significant difference between the treatment and the control, and
*%Incr* is the percent increase of the fluorescence compared to the
control.

In the Dunnett’s test result table, *Estimate* is the estimated
difference between the treatment and the control, *SE* is the standard
error of the mixed ANOVA model, *t value* is the Dunnett’s test
statistic, *adj p* is the adjusted p value and *%Incr* is the percent
increase of the fluorescence compared to the control.

    knitr::kable(radar_spiked_result$WilliamsIncrease, caption="Increasing Williams' test")

<table style="width:100%;">
<caption>Increasing Williams’ test</caption>
<colgroup>
<col style="width: 15%" />
<col style="width: 5%" />
<col style="width: 8%" />
<col style="width: 8%" />
<col style="width: 10%" />
<col style="width: 10%" />
<col style="width: 3%" />
<col style="width: 11%" />
<col style="width: 9%" />
<col style="width: 6%" />
<col style="width: 10%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">Conc</th>
<th style="text-align: right;">Y.Tilde</th>
<th style="text-align: right;">Y0</th>
<th style="text-align: right;">DIFF Incr</th>
<th style="text-align: right;">SE_DIFF</th>
<th style="text-align: right;">DF</th>
<th style="text-align: right;">WILL Incr</th>
<th style="text-align: right;">crit Val</th>
<th style="text-align: left;">Sign</th>
<th style="text-align: right;">% Incr</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Conc16 - Conc0</td>
<td style="text-align: left;">16</td>
<td style="text-align: right;">8.38858</td>
<td style="text-align: right;">8.51927</td>
<td style="text-align: right;">-0.13069</td>
<td style="text-align: right;">0.3478327</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.3757267</td>
<td style="text-align: right;">1.971</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-53.65431</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc8 - Conc0</td>
<td style="text-align: left;">8</td>
<td style="text-align: right;">8.38858</td>
<td style="text-align: right;">8.51927</td>
<td style="text-align: right;">-0.13069</td>
<td style="text-align: right;">0.3478327</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.3757267</td>
<td style="text-align: right;">1.965</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-56.48917</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc4 - Conc0</td>
<td style="text-align: left;">4</td>
<td style="text-align: right;">8.38858</td>
<td style="text-align: right;">8.51927</td>
<td style="text-align: right;">-0.13069</td>
<td style="text-align: right;">0.3478327</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.3757267</td>
<td style="text-align: right;">1.956</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-27.10703</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc2 - Conc0</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">8.38858</td>
<td style="text-align: right;">8.51927</td>
<td style="text-align: right;">-0.13069</td>
<td style="text-align: right;">0.3478327</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.3757267</td>
<td style="text-align: right;">1.940</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">16.97755</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc1 - Conc0</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">8.07433</td>
<td style="text-align: right;">8.51927</td>
<td style="text-align: right;">-0.44494</td>
<td style="text-align: right;">0.3478327</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-1.2791783</td>
<td style="text-align: right;">1.908</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-45.48875</td>
</tr>
</tbody>
</table>

    knitr::kable(radar_spiked_result$WilliamsDecrease, caption="Decreasing Williams' test")

<table style="width:100%;">
<caption>Decreasing Williams’ test</caption>
<colgroup>
<col style="width: 15%" />
<col style="width: 5%" />
<col style="width: 8%" />
<col style="width: 8%" />
<col style="width: 10%" />
<col style="width: 10%" />
<col style="width: 3%" />
<col style="width: 11%" />
<col style="width: 9%" />
<col style="width: 6%" />
<col style="width: 10%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">Conc</th>
<th style="text-align: right;">Y.Tilde</th>
<th style="text-align: right;">Y0</th>
<th style="text-align: right;">DIFF Decr</th>
<th style="text-align: right;">SE_DIFF</th>
<th style="text-align: right;">DF</th>
<th style="text-align: right;">WILL Decr</th>
<th style="text-align: right;">crit Val</th>
<th style="text-align: left;">Sign</th>
<th style="text-align: right;">% Incr</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Conc16 - Conc0</td>
<td style="text-align: left;">16</td>
<td style="text-align: right;">7.99566</td>
<td style="text-align: right;">8.51927</td>
<td style="text-align: right;">0.52361</td>
<td style="text-align: right;">0.3478327</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">1.5053503</td>
<td style="text-align: right;">1.971</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-53.65431</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc8 - Conc0</td>
<td style="text-align: left;">8</td>
<td style="text-align: right;">7.99566</td>
<td style="text-align: right;">8.51927</td>
<td style="text-align: right;">0.52361</td>
<td style="text-align: right;">0.3478327</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">1.5053503</td>
<td style="text-align: right;">1.965</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-56.48917</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc4 - Conc0</td>
<td style="text-align: left;">4</td>
<td style="text-align: right;">8.54577</td>
<td style="text-align: right;">8.51927</td>
<td style="text-align: right;">-0.02650</td>
<td style="text-align: right;">0.3478327</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.0761861</td>
<td style="text-align: right;">1.956</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-27.10703</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc2 - Conc0</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">8.54577</td>
<td style="text-align: right;">8.51927</td>
<td style="text-align: right;">-0.02650</td>
<td style="text-align: right;">0.3478327</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.0761861</td>
<td style="text-align: right;">1.940</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">16.97755</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc1 - Conc0</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">8.54577</td>
<td style="text-align: right;">8.51927</td>
<td style="text-align: right;">-0.02650</td>
<td style="text-align: right;">0.3478327</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.0761861</td>
<td style="text-align: right;">1.908</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-45.48875</td>
</tr>
</tbody>
</table>

    knitr::kable(radar_spiked_result$Dunnetts, caption="Dunnett's test")

<table>
<caption>Dunnett’s test</caption>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: right;">Estimate</th>
<th style="text-align: right;">SE</th>
<th style="text-align: right;">df</th>
<th style="text-align: right;">t value</th>
<th style="text-align: right;">adj p</th>
<th style="text-align: right;">% Incr</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Conc1 - Conc0</td>
<td style="text-align: right;">-0.4449358</td>
<td style="text-align: right;">0.3478327</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-1.279166</td>
<td style="text-align: right;">0.5986692</td>
<td style="text-align: right;">-45.48875</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc2 - Conc0</td>
<td style="text-align: right;">0.4707977</td>
<td style="text-align: right;">0.3478327</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">1.353518</td>
<td style="text-align: right;">0.5520005</td>
<td style="text-align: right;">16.97755</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc4 - Conc0</td>
<td style="text-align: right;">0.0536514</td>
<td style="text-align: right;">0.3478327</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">0.154245</td>
<td style="text-align: right;">0.9999132</td>
<td style="text-align: right;">-27.10703</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc8 - Conc0</td>
<td style="text-align: right;">-0.5776610</td>
<td style="text-align: right;">0.3478327</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-1.660744</td>
<td style="text-align: right;">0.3794252</td>
<td style="text-align: right;">-56.48917</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc16 - Conc0</td>
<td style="text-align: right;">-0.4695496</td>
<td style="text-align: right;">0.3478327</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-1.349930</td>
<td style="text-align: right;">0.5543160</td>
<td style="text-align: right;">-53.65431</td>
</tr>
</tbody>
</table>

-   Further information about the normality test (Shapiro-Wilk), the
    homogeneity of variance test (Levene’s test) of residuals of the
    mixed ANOVA model, the monotonicity test and the model fit.

<!-- -->

    radar_spiked_result$NormalityTest
    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  stats::resid(mixedaov)
    #> W = 0.99071, p-value = 0.05007
    radar_spiked_result$LeveneTest
    #> # A tibble: 1 × 4
    #>   statistic p.value    df df.residual
    #>       <dbl>   <dbl> <int>       <int>
    #> 1     0.474   0.796     5         300
    radar_spiked_result$`Monotonicity Test`
    #>        Test t value Pr(>|t|) Significance
    #> 1    Linear   -1.57   0.1167            .
    #> 2 Quadratic   -1.48   0.1400            .
    radar_spiked_result$MixedAnova
    #> Linear mixed model fit by REML ['lmerMod']
    #> Formula: 
    #> log(Fluor) ~ Conc + (1 | Replicate) + (1 | Replicate:Conc)
    #>    Data: dataframe
    #> REML criterion at convergence: 1218.932
    #> Random effects:
    #>  Groups         Name        Std.Dev.
    #>  Replicate:Conc (Intercept) 0.000   
    #>  Replicate      (Intercept) 0.466   
    #>  Residual                   1.756   
    #> Number of obs: 306, groups:  
    #> Replicate:Conc, 18; Replicate, 3
    #> Fixed Effects:
    #> (Intercept)       Conc.L       Conc.Q       Conc.C  
    #>     8.35799     -0.37806     -0.37347      0.01863  
    #>      Conc^4       Conc^5  
    #>     0.68924     -0.25055  
    #> optimizer (nloptwrap) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings

*The list output from running the data\_prep() function can be
summarized with the data\_summary() function.*

    xeredar::data_summary(radar_spiked_result) |>
      knitr::kable()

<table>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">1</th>
<th style="text-align: left;">2</th>
<th style="text-align: left;">4</th>
<th style="text-align: left;">8</th>
<th style="text-align: left;">16</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Replicate 1</td>
<td style="text-align: left;">-72.69</td>
<td style="text-align: left;">23.37</td>
<td style="text-align: left;">-32.96</td>
<td style="text-align: left;">-78.26</td>
<td style="text-align: left;">-74.31</td>
</tr>
<tr class="even">
<td style="text-align: left;">Replicate 2</td>
<td style="text-align: left;">-31.57</td>
<td style="text-align: left;">-40.63</td>
<td style="text-align: left;">-3.18</td>
<td style="text-align: left;">-45.23</td>
<td style="text-align: left;">-47</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Replicate 3</td>
<td style="text-align: left;">-30.69</td>
<td style="text-align: left;">41</td>
<td style="text-align: left;">-34.52</td>
<td style="text-align: left;">-44.7</td>
<td style="text-align: left;">-40.43</td>
</tr>
<tr class="even">
<td style="text-align: left;">Pooled</td>
<td style="text-align: left;">-45.49</td>
<td style="text-align: left;">16.98</td>
<td style="text-align: left;">-27.11</td>
<td style="text-align: left;">-56.49</td>
<td style="text-align: left;">-53.65</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Dunnett</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
</tr>
<tr class="even">
<td style="text-align: left;">IncreasingWilliams</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
</tr>
<tr class="odd">
<td style="text-align: left;">DecreasingWilliams</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
</tr>
</tbody>
</table>

Running default REACTIV analysis
================================

To demonstrate how to run REACTIV analysis, we will use one artificial
data set containing the spiked and unspiked measurements.

    reactiv_spiked <- xeredar::REACTIV_valid_data_table_spiked_unspiked[["Anastrozole_UK"]]$Spiked 
    reactiv_unspiked <- xeredar::REACTIV_valid_data_table_spiked_unspiked[["Anastrozole_UK"]]$Unspiked 

The default REACTIV analysis can be run using the `data_prep()` function
with either spiked or unspiked data. This function automatically decides
whether trimming, outlier removal and/or transformations are conducted
following the manuscript (Spyridonov et al. unpublished). The actual
analysis is carried out by the `ana()` function. The `ana()` function is
called by the `data_prep()` function and does not need to be called
separately. The analysis the REACTIV assay follows the description of
the Method 2 (the mixed ANOVA approach) in the Annex 8: methods for the
statistical analysis of REACTIV assay data of the Amended Draft new Test
Guideline for the REACTIV assay for second WNT-review
([30.01.2024](https://www.oecd.org/chemicalsafety/testing/amended-draft-new-test-guideline-for-the-rapid-estrogen-ACTivity-in-vivo-assay.pdf)).
For this assay, the `row` argument should be set to `FALSE`. Trimming is
not required. In case there are residuals deviate from normality and
variance homogeneity, outlier removal (e.g. by applying the Tukey rule
(Green et al., 2018) and data transformation (for example log- or
square-root) can be conducted.


    reactiv_spiked_result <- xeredar::data_prep(dataframe = reactiv_spiked, row = FALSE, trimming=FALSE, boxcox = FALSE)
    reactiv_unspiked_result <- xeredar::data_prep(dataframe = reactiv_unspiked, row= FALSE, trimming=FALSE, boxcox = FALSE)

Here we use the spiked data as an example to demonstrate the output of
the `data_prep()` function.

The outputs of the `data_prep()` function are lists containing the
following elements:

-   A reasoning for the recommended transformation and trimming. The
    data from which outliers were removed with the Tukey-rule where the
    fluorescence values are square-root transformed, should be used for
    the analysis because only after outlier removal and sqrt
    transformation, the residuals of the mixed ANOVA are normally
    distributed and have homogeneous variances among treatment groups

<!-- -->

    reactiv_spiked_result$Justify
    #> [1] "The data from which outliers were removed with the\n                    Tukey-rule where the fluorescence values are square-root\n                    transformed, should be used for the analysis because only\n                    after outlier removal and sqrt transformation, the\n                    residuals of the mixed ANOVA are normally distributed\n                    and have homogeneous variances among treatment groups"

-   A data frame of the processed data (e.g. raw data, transformed, or
    outlier removed) used for actual statistical testing following the
    reasoning. The box plots of the processed data per run/replicate
    (i.e. each panel represents each run/replicate) are also provided
    for visual inspection.

<!-- -->

    knitr::kable(head(reactiv_spiked_result$ProcessedData))

<table>
<thead>
<tr class="header">
<th style="text-align: left;">Treatment</th>
<th style="text-align: right;">Fluor</th>
<th style="text-align: left;">Conc</th>
<th style="text-align: left;">Replicate</th>
<th style="text-align: left;">Compound</th>
<th style="text-align: left;">Country</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">AN-0,18mg/L + Testostérone</td>
<td style="text-align: right;">27037.33</td>
<td style="text-align: left;">0.18</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">Anastrozole</td>
<td style="text-align: left;">UK</td>
</tr>
<tr class="even">
<td style="text-align: left;">AN-0,36mg/L + Testostérone</td>
<td style="text-align: right;">201316.67</td>
<td style="text-align: left;">0.36</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">Anastrozole</td>
<td style="text-align: left;">UK</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AN-0,73mg/L + Testostérone</td>
<td style="text-align: right;">34651.00</td>
<td style="text-align: left;">0.73</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">Anastrozole</td>
<td style="text-align: left;">UK</td>
</tr>
<tr class="even">
<td style="text-align: left;">AN-1,45mg/L + Testostérone</td>
<td style="text-align: right;">289984.67</td>
<td style="text-align: left;">1.45</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">Anastrozole</td>
<td style="text-align: left;">UK</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AN-2,9mg/L + Testostérone</td>
<td style="text-align: right;">233271.00</td>
<td style="text-align: left;">2.9</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">Anastrozole</td>
<td style="text-align: left;">UK</td>
</tr>
<tr class="even">
<td style="text-align: left;">AN-0,18mg/L + Testostérone</td>
<td style="text-align: right;">64846.33</td>
<td style="text-align: left;">0.18</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">Anastrozole</td>
<td style="text-align: left;">UK</td>
</tr>
</tbody>
</table>

    reactiv_spiked_result$BoxPlots

![](User_guide_files/figure-markdown_strict/unnamed-chunk-25-1.png)

-   Summary tables of the processed data (per replicate and overall)

<!-- -->

    knitr::kable(reactiv_spiked_result$SummaryDF_Rep, caption="Summary statistics of fluorescence in different concentrations of test item per replicate")

<table>
<caption>Summary statistics of fluorescence in different concentrations of test item per replicate</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Conc</th>
<th style="text-align: left;">Replicate</th>
<th style="text-align: right;">N</th>
<th style="text-align: right;">Mean</th>
<th style="text-align: right;">Standard deviation</th>
<th style="text-align: right;">Coefficient of variation</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">0</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">1417505.4</td>
<td style="text-align: right;">367130.47</td>
<td style="text-align: right;">0.2589976</td>
</tr>
<tr class="even">
<td style="text-align: left;">0</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">5</td>
<td style="text-align: right;">1299380.0</td>
<td style="text-align: right;">241756.84</td>
<td style="text-align: right;">0.1860555</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">1699476.3</td>
<td style="text-align: right;">382759.66</td>
<td style="text-align: right;">0.2252221</td>
</tr>
<tr class="even">
<td style="text-align: left;">0.18</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">216154.0</td>
<td style="text-align: right;">211883.73</td>
<td style="text-align: right;">0.9802445</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0.18</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">149156.5</td>
<td style="text-align: right;">116877.63</td>
<td style="text-align: right;">0.7835909</td>
</tr>
<tr class="even">
<td style="text-align: left;">0.18</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">255036.8</td>
<td style="text-align: right;">162183.70</td>
<td style="text-align: right;">0.6359228</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0.36</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">149004.8</td>
<td style="text-align: right;">96508.01</td>
<td style="text-align: right;">0.6476841</td>
</tr>
<tr class="even">
<td style="text-align: left;">0.36</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">230237.3</td>
<td style="text-align: right;">105711.23</td>
<td style="text-align: right;">0.4591404</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0.36</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">199221.1</td>
<td style="text-align: right;">116329.38</td>
<td style="text-align: right;">0.5839210</td>
</tr>
<tr class="even">
<td style="text-align: left;">0.73</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">169331.4</td>
<td style="text-align: right;">115443.55</td>
<td style="text-align: right;">0.6817610</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0.73</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">187208.8</td>
<td style="text-align: right;">215340.83</td>
<td style="text-align: right;">1.1502710</td>
</tr>
<tr class="even">
<td style="text-align: left;">0.73</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">292332.4</td>
<td style="text-align: right;">139121.01</td>
<td style="text-align: right;">0.4759001</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1.45</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">116438.8</td>
<td style="text-align: right;">102761.27</td>
<td style="text-align: right;">0.8825347</td>
</tr>
<tr class="even">
<td style="text-align: left;">1.45</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">173724.2</td>
<td style="text-align: right;">134626.51</td>
<td style="text-align: right;">0.7749439</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1.45</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">236771.7</td>
<td style="text-align: right;">100056.61</td>
<td style="text-align: right;">0.4225869</td>
</tr>
<tr class="even">
<td style="text-align: left;">2.9</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">159732.2</td>
<td style="text-align: right;">78955.44</td>
<td style="text-align: right;">0.4942987</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2.9</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">234475.2</td>
<td style="text-align: right;">170916.42</td>
<td style="text-align: right;">0.7289317</td>
</tr>
<tr class="even">
<td style="text-align: left;">2.9</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">247387.4</td>
<td style="text-align: right;">127553.16</td>
<td style="text-align: right;">0.5156009</td>
</tr>
</tbody>
</table>

    knitr::kable(reactiv_spiked_result$SummaryDF, caption="Summary statistics of fluorescence in different concentrations of test item of all replicates")

<table>
<caption>Summary statistics of fluorescence in different concentrations of test item of all replicates</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Conc</th>
<th style="text-align: right;">N</th>
<th style="text-align: right;">Mean</th>
<th style="text-align: right;">Standard deviation</th>
<th style="text-align: right;">Coefficient of variation</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">0</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">1482281.8</td>
<td style="text-align: right;">363637.5</td>
<td style="text-align: right;">0.2453228</td>
</tr>
<tr class="even">
<td style="text-align: left;">0.18</td>
<td style="text-align: right;">23</td>
<td style="text-align: right;">209287.9</td>
<td style="text-align: right;">168250.6</td>
<td style="text-align: right;">0.8039193</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0.36</td>
<td style="text-align: right;">22</td>
<td style="text-align: right;">190829.4</td>
<td style="text-align: right;">106636.7</td>
<td style="text-align: right;">0.5588065</td>
</tr>
<tr class="even">
<td style="text-align: left;">0.73</td>
<td style="text-align: right;">23</td>
<td style="text-align: right;">217555.3</td>
<td style="text-align: right;">161918.4</td>
<td style="text-align: right;">0.7442633</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1.45</td>
<td style="text-align: right;">22</td>
<td style="text-align: right;">172953.7</td>
<td style="text-align: right;">118883.2</td>
<td style="text-align: right;">0.6873702</td>
</tr>
<tr class="even">
<td style="text-align: left;">2.9</td>
<td style="text-align: right;">22</td>
<td style="text-align: right;">211991.3</td>
<td style="text-align: right;">126959.5</td>
<td style="text-align: right;">0.5988900</td>
</tr>
</tbody>
</table>

-   Tables of results evaluated using increasing/decreasing Williams
    test and/or Dunnett’s test, if applicable.

In the Williams’ test result tables, *Y.Tilde* is the amalgamated mean
of the fluorescence in each treatment group, *Y0* is the mean of the
control fluorescence, *DIFF* is the estimated difference between the
treatment and the control, *SE\_DIFF* is the standard error of the
Williams’ test, *DF* is the degrees of freedom for Williams’ test, *WILL
Incr* or *Will Decr* is the Williams’ test statistic, *crit Val* is the
critical value of Williams distribution, *Sign* suggests if there is
significant difference between the treatment and the control, and
*%Incr* is the percent increase of the fluorescence compared to the
control.

In the Dunnett’s test result table, *Estimate* is the estimated
difference between the treatment and the control, *SE* is the standard
error of the mixed ANOVA model, *t value* is the Dunnett’s test
statistic, *adj p* is the adjusted p value and *%Incr* is the percent
increase of the fluorescence compared to the control.

    knitr::kable(reactiv_spiked_result$WilliamsIncrease, caption="Increasing Williams' test")

<table style="width:100%;">
<caption>Increasing Williams’ test</caption>
<colgroup>
<col style="width: 17%" />
<col style="width: 5%" />
<col style="width: 8%" />
<col style="width: 9%" />
<col style="width: 10%" />
<col style="width: 9%" />
<col style="width: 3%" />
<col style="width: 10%" />
<col style="width: 9%" />
<col style="width: 6%" />
<col style="width: 10%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">Conc</th>
<th style="text-align: right;">Y.Tilde</th>
<th style="text-align: right;">Y0</th>
<th style="text-align: right;">DIFF Incr</th>
<th style="text-align: right;">SE_DIFF</th>
<th style="text-align: right;">DF</th>
<th style="text-align: right;">WILL Incr</th>
<th style="text-align: right;">crit Val</th>
<th style="text-align: left;">Sign</th>
<th style="text-align: right;">% Incr</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Conc2.9 - Conc0</td>
<td style="text-align: left;">2.9</td>
<td style="text-align: right;">439.658</td>
<td style="text-align: right;">1208.915</td>
<td style="text-align: right;">-769.2567</td>
<td style="text-align: right;">48.94620</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">-15.71637</td>
<td style="text-align: right;">1.933</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-85.69831</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc1.45 - Conc0</td>
<td style="text-align: left;">1.45</td>
<td style="text-align: right;">413.890</td>
<td style="text-align: right;">1208.915</td>
<td style="text-align: right;">-795.0246</td>
<td style="text-align: right;">48.95169</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">-16.24101</td>
<td style="text-align: right;">1.927</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-88.33193</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc0.73 - Conc0</td>
<td style="text-align: left;">0.73</td>
<td style="text-align: right;">413.890</td>
<td style="text-align: right;">1208.915</td>
<td style="text-align: right;">-795.0246</td>
<td style="text-align: right;">48.47768</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">-16.39981</td>
<td style="text-align: right;">1.918</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-85.32295</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc0.36 - Conc0</td>
<td style="text-align: left;">0.36</td>
<td style="text-align: right;">413.890</td>
<td style="text-align: right;">1208.915</td>
<td style="text-align: right;">-795.0246</td>
<td style="text-align: right;">48.95169</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">-16.24101</td>
<td style="text-align: right;">1.903</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-87.12597</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc0.18 - Conc0</td>
<td style="text-align: left;">0.18</td>
<td style="text-align: right;">413.890</td>
<td style="text-align: right;">1208.915</td>
<td style="text-align: right;">-795.0246</td>
<td style="text-align: right;">48.47768</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">-16.39981</td>
<td style="text-align: right;">1.873</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-85.88070</td>
</tr>
</tbody>
</table>

    knitr::kable(reactiv_spiked_result$WilliamsDecrease, caption="Decreasing Williams' test")

<table style="width:100%;">
<caption>Decreasing Williams’ test</caption>
<colgroup>
<col style="width: 17%" />
<col style="width: 5%" />
<col style="width: 8%" />
<col style="width: 9%" />
<col style="width: 10%" />
<col style="width: 9%" />
<col style="width: 3%" />
<col style="width: 10%" />
<col style="width: 9%" />
<col style="width: 5%" />
<col style="width: 10%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">Conc</th>
<th style="text-align: right;">Y.Tilde</th>
<th style="text-align: right;">Y0</th>
<th style="text-align: right;">DIFF Decr</th>
<th style="text-align: right;">SE_DIFF</th>
<th style="text-align: right;">DF</th>
<th style="text-align: right;">WILL Decr</th>
<th style="text-align: right;">crit Val</th>
<th style="text-align: left;">Sign</th>
<th style="text-align: right;">% Incr</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Conc2.9 - Conc0</td>
<td style="text-align: left;">2.9</td>
<td style="text-align: right;">411.240</td>
<td style="text-align: right;">1208.915</td>
<td style="text-align: right;">797.6747</td>
<td style="text-align: right;">48.94620</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">16.29697</td>
<td style="text-align: right;">1.933</td>
<td style="text-align: left;">TRUE</td>
<td style="text-align: right;">-85.69831</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc1.45 - Conc0</td>
<td style="text-align: left;">1.45</td>
<td style="text-align: right;">411.240</td>
<td style="text-align: right;">1208.915</td>
<td style="text-align: right;">797.6747</td>
<td style="text-align: right;">48.95169</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">16.29514</td>
<td style="text-align: right;">1.927</td>
<td style="text-align: left;">TRUE</td>
<td style="text-align: right;">-88.33193</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc0.73 - Conc0</td>
<td style="text-align: left;">0.73</td>
<td style="text-align: right;">424.957</td>
<td style="text-align: right;">1208.915</td>
<td style="text-align: right;">783.9576</td>
<td style="text-align: right;">48.47768</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">16.17152</td>
<td style="text-align: right;">1.918</td>
<td style="text-align: left;">TRUE</td>
<td style="text-align: right;">-85.32295</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc0.36 - Conc0</td>
<td style="text-align: left;">0.36</td>
<td style="text-align: right;">424.957</td>
<td style="text-align: right;">1208.915</td>
<td style="text-align: right;">783.9576</td>
<td style="text-align: right;">48.95169</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">16.01493</td>
<td style="text-align: right;">1.903</td>
<td style="text-align: left;">TRUE</td>
<td style="text-align: right;">-87.12597</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc0.18 - Conc0</td>
<td style="text-align: left;">0.18</td>
<td style="text-align: right;">424.957</td>
<td style="text-align: right;">1208.915</td>
<td style="text-align: right;">783.9576</td>
<td style="text-align: right;">48.47768</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">16.17152</td>
<td style="text-align: right;">1.873</td>
<td style="text-align: left;">TRUE</td>
<td style="text-align: right;">-85.88070</td>
</tr>
</tbody>
</table>

    knitr::kable(reactiv_spiked_result$Dunnetts, caption="Dunnett's test")

<table>
<caption>Dunnett’s test</caption>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: right;">Estimate</th>
<th style="text-align: right;">SE</th>
<th style="text-align: right;">df</th>
<th style="text-align: right;">t value</th>
<th style="text-align: right;">adj p</th>
<th style="text-align: right;">% Incr</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Conc0.18 - Conc0</td>
<td style="text-align: right;">-786.2696</td>
<td style="text-align: right;">48.47768</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">-16.21921</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">-85.88070</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc0.36 - Conc0</td>
<td style="text-align: right;">-786.3430</td>
<td style="text-align: right;">48.95169</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">-16.06365</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">-87.12597</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc0.73 - Conc0</td>
<td style="text-align: right;">-776.5006</td>
<td style="text-align: right;">48.47768</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">-16.01769</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">-85.32295</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc1.45 - Conc0</td>
<td style="text-align: right;">-822.2317</td>
<td style="text-align: right;">48.95169</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">-16.79680</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">-88.33193</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc2.9 - Conc0</td>
<td style="text-align: right;">-769.7654</td>
<td style="text-align: right;">48.94620</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">-15.72677</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">-85.69831</td>
</tr>
</tbody>
</table>

-   Further information about the normality test (Shapiro-Wilk), the
    homogeneity of variance test (Levene’s test) of residuals of the
    mixed ANOVA model, the monotonicity test and the model fit.

<!-- -->

    reactiv_spiked_result$NormalityTest
    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  stats::resid(mixedaov)
    #> W = 0.97621, p-value = 0.0227
    reactiv_spiked_result$LeveneTest
    #> # A tibble: 1 × 4
    #>   statistic p.value    df df.residual
    #>       <dbl>   <dbl> <int>       <int>
    #> 1     0.829   0.532     5         123
    reactiv_spiked_result$`Monotonicity Test`
    #>        Test t value Pr(>|t|) Significance
    #> 1    Linear   -6.34  <0.0001          ***
    #> 2 Quadratic    6.05  <0.0001          ***
    reactiv_spiked_result$MixedAnova
    #> Linear mixed model fit by REML ['lmerMod']
    #> Formula: 
    #> sqrt(Fluor) ~ Conc + (1 | Replicate) + (1 | Replicate:Conc)
    #>    Data: wt_outlier
    #> REML criterion at convergence: 1607.975
    #> Random effects:
    #>  Groups         Name        Std.Dev.
    #>  Replicate:Conc (Intercept)   0.00  
    #>  Replicate      (Intercept)  51.74  
    #>  Residual                   151.56  
    #> Number of obs: 129, groups:  
    #> Replicate:Conc, 18; Replicate, 3
    #> Fixed Effects:
    #> (Intercept)       Conc.L       Conc.Q       Conc.C  
    #>      551.13      -471.74       437.64      -271.05  
    #>      Conc^4       Conc^5  
    #>      175.76       -30.96  
    #> optimizer (nloptwrap) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings

*The list output from running the data\_prep() function can be
summarized with the data\_summary() function.*

    xeredar::data_summary(reactiv_spiked_result) |>
      knitr::kable()

<table>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">0.18</th>
<th style="text-align: left;">0.36</th>
<th style="text-align: left;">0.73</th>
<th style="text-align: left;">1.45</th>
<th style="text-align: left;">2.9</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Replicate 1</td>
<td style="text-align: left;">-84.75</td>
<td style="text-align: left;">-89.49</td>
<td style="text-align: left;">-88.05</td>
<td style="text-align: left;">-91.79</td>
<td style="text-align: left;">-88.73</td>
</tr>
<tr class="even">
<td style="text-align: left;">Replicate 2</td>
<td style="text-align: left;">-88.52</td>
<td style="text-align: left;">-82.28</td>
<td style="text-align: left;">-85.59</td>
<td style="text-align: left;">-86.63</td>
<td style="text-align: left;">-81.95</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Replicate 3</td>
<td style="text-align: left;">-84.99</td>
<td style="text-align: left;">-88.28</td>
<td style="text-align: left;">-82.8</td>
<td style="text-align: left;">-86.07</td>
<td style="text-align: left;">-85.44</td>
</tr>
<tr class="even">
<td style="text-align: left;">Pooled</td>
<td style="text-align: left;">-85.88</td>
<td style="text-align: left;">-87.13</td>
<td style="text-align: left;">-85.32</td>
<td style="text-align: left;">-88.33</td>
<td style="text-align: left;">-85.7</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Dunnett</td>
<td style="text-align: left;">*</td>
<td style="text-align: left;">*</td>
<td style="text-align: left;">*</td>
<td style="text-align: left;">*</td>
<td style="text-align: left;">*</td>
</tr>
<tr class="even">
<td style="text-align: left;">IncreasingWilliams</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
</tr>
<tr class="odd">
<td style="text-align: left;">DecreasingWilliams</td>
<td style="text-align: left;">*</td>
<td style="text-align: left;">*</td>
<td style="text-align: left;">*</td>
<td style="text-align: left;">*</td>
<td style="text-align: left;">*</td>
</tr>
</tbody>
</table>

The shiny app
=============

xeredar contains an integrated shiny app that is available to the users
by writing `run_app()` into the console. The only requirement to use the
app is the successful installation of xeredar. When the app started, the
user has a couple of options to analyse the data. These options can be
adjusted by clicking on the little gear sign next to *Inputs*. The
following options are available:

-   **Use d’Agostino test?**
    -   The default here is that this is not selected meaning the
        Shapiro-Wilk test is utilized to check for residual normality.
        However, the RADAR TG mentions the d’Agostino test which is why
        it is also available to the users.
-   **Apply 10% Trimming**
    -   The default here is that this is selected, meaning that 10%
        Trimming is conducted when the ANOVA assumptions are not
        fulfilled by the raw data. Please be aware that this does not
        mean that 10% Trimming is always conducted. It is only
        conducted, when the raw data is violating the residual normality
        and variance homogeneity assumptions, tested with the respective
        test with an adjustable alpha level.
-   **Remove outliers**
    -   The default here is that this is selected, meaning that outlier
        removal is conducted when the ANOVA assumptions are not
        fulfilled by the raw data or by 10% Trimming, if it is selected
        above.
-   **Try Box-Cox transformation**
    -   The default here is that this is selected, meaning that box-cox
        transformation is carried out when the ANOVA assumptions are not
        fulfilled by the raw or processed data (10% Trimming or outlier
        removal) nor by log- or square-root transformation. Box-Cox
        transformation is not mentioned in any of the TGs which is why
        it is left to the choice of the user.
-   **alpha for residual normality and variance homogeneity tests**
    -   The default here is 0.05. In the TGs the alpha level is
        discussed so the user is adviced to inspect the requirements of
        the respective study to analyze.

The next option in the little box on the top-left of the app is to set a
hook or remove the hook to decide whether the exposure well ID is
regarded as random term in the underlying mixed ANOVA model. In case a
REACTIV study is investigated the hook should be removed. For RADAR and
XETA, the hook should be placed as long as information about the
exposure well ID was documented. Of course, reducing the complexity of
the random term might also make sense for XETA and RADAR studies in case
no variance is explained by the exposure well-ID. However, this choice
is left to the user.

When own data is supposed to be analyzed xlsx files with either spiked
or unspiked data can be uploaded. Please make sure that the uploaded
data fulfills the Data requirements explained above. In case of
insecurity, inspect the uploaded data structure of the pre-loaded
example data.

The app contains several output boxes about the required data
processing, a conclusion table, the uploaded data, residual diagnostics
plots, boxplots, a data summary table, the output of the residual
normality and homogeneity tests, the monotonicity test result, the
Dunnett’s and increasing and decreasing Williams test result tables
along with the summary table of the underlying mixed ANOVA model.

The depicted information can be downloaded in a simple report by
clicking on the button \*\* Word report\*\*. It takes a couple of
seconds until the final docx file is produced and ready to download.
Avoid clicking the button several times as this can lead to long waiting
times and the production of several reports.

References
==========

OECD. 2019a. Validation Report of the Xenopus Eleutheroembryonic Thyroid
Signaling Assay (XETA) for the Detection of Thyroid Active Substances.
OECD.

OECD. 2019b. TG 248: Xenopus Eleutheroembryonic Thyroid Assay (XETA).
OECD.
<a href="https://doi.org/10.1787/a13f80ee-en" class="uri">https://doi.org/10.1787/a13f80ee-en</a>.

OECD. 2022b. Test No. 251: Rapid Androgen Disruption Activity Reporter
(RADAR) Assay. OECD.
<a href="https://doi.org/10.1787/da264d82-en" class="uri">https://doi.org/10.1787/da264d82-en</a>.

OECD. 2022a. Rapid Estrogen Activity in Vivo (REACTIV) Assay (OECD Draft
TG): Guideline for the Testing of Chemicals, Section 2: Effects on
Biotic System. OECD.

Inka Marie Spyridonov, Lijuan Yan, Eduard Szöcs, Ana Filipa Pereira Miranda, Carsten Lange, 
Andrew Tindall, David Du Pasquier, Gregory Lemkine, Lennart Weltje, Maike Habekost, 
Pernille Thorbek. 2025. Xeredar: An open-source R-package for the statistical analysis
of endocrine new approach methods (NAMs) using fish or amphibian eleutheroembryos, 
Environmental Toxicology and Chemistry. 
<a href="https://doi.org/10.1093/etojnl/vgaf056" class="uri">https://doi.org/10.1093/etojnl/vgaf056</a>
