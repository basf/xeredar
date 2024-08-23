# xeredar <a href="https://dplyr.tidyverse.org"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/basf/xeredar/actions/workflows/Package_check.yml/badge.svg)](https://github.com/basf/xeredar/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Background

The package xeredar is an R-package for analysis of the New Approach
Methodology (NAM) assays of XETA (Xenopus Eleutheroembryonic Thyroid),
RADAR (Rapid Androgen Disruption Activity Reporter) and REACTIV (Rapid
Estrogen ACTivity In Vivo) for assessing endocrine effects of chemicals
on the thyroid, androgen/steroid and estrogen axis. The functionality is
based on the SAS-script recommended in the Annex 13 of OECD test
guideline No. 248 of the XETA assay
([2019](https://www.oecd-ilibrary.org/environment/tg-248-xenopus-eleutheroembryonic-thyroid-assay-xeta_a13f80ee-en)),
written by John Green.

# Data requirements

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
factors*. The order of the columns is not relevant. It is important that
the decimal separator is a period instead of a comma.

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

# Running default XETA analysis

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
following the manuscript (Spyridonov et al. unpublished). The actual
analysis is carried out by the `ana()` function. The `ana()` function is
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

![](README_files/figure-markdown_strict/unnamed-chunk-8-1.png)

-   Summary tables of the processed data (per replicate and overall)

<!-- -->

    knitr::kable(xeta_spiked_result$SummaryDF_Rep, caption="Summary statistics of fluorescence in different concentrations of test item per replicate")

<table>
<caption>Summary statistics of fluorescence in different concentrations
of test item per replicate</caption>
<colgroup>
<col style="width: 7%" />
<col style="width: 14%" />
<col style="width: 4%" />
<col style="width: 12%" />
<col style="width: 26%" />
<col style="width: 35%" />
</colgroup>
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

Summary statistics of fluorescence in different concentrations of test
item per replicate

    knitr::kable(xeta_spiked_result$SummaryDF, caption="Summary statistics of fluorescence in different concentrations of test item of all replicates")

<table>
<caption>Summary statistics of fluorescence in different concentrations
of test item of all replicates</caption>
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

Summary statistics of fluorescence in different concentrations of test
item of all replicates

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

Increasing Williams’ test

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

Decreasing Williams’ test

    knitr::kable(xeta_spiked_result$Dunnetts, caption="Dunnett's test")

<table>
<caption>Dunnett’s test</caption>
<colgroup>
<col style="width: 23%" />
<col style="width: 14%" />
<col style="width: 13%" />
<col style="width: 4%" />
<col style="width: 15%" />
<col style="width: 14%" />
<col style="width: 14%" />
</colgroup>
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

Dunnett’s test

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
    #> Formula: Fluor ~ Conc + (1 | Replicate) + (1 | Replicate:Conc)
    #>    Data: dataframe
    #> REML criterion at convergence: 5608.294
    #> Random effects:
    #>  Groups         Name        Std.Dev.
    #>  Replicate:Conc (Intercept) 241.1   
    #>  Replicate      (Intercept) 350.5   
    #>  Residual                   827.7   
    #> Number of obs: 348, groups:  Replicate:Conc, 18; Replicate, 3
    #> Fixed Effects:
    #> (Intercept)       Conc.L       Conc.Q       Conc.C       Conc^4       Conc^5  
    #>      4824.2        758.5        264.5         52.6        149.8       -270.8

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

# Running default RADAR analysis

To demonstrate how to run RADAR analysis, we will use one of the data
sets by the Pos\_mDHT\_Fraunhofer\_RADAR containing the spiked and
unspiked measurements from the RADAR study validation in the lab
Fraunhofer with an androgen axis active chemical.

    radar_spiked <- xeredar::Pos_mDHT_Fraunhofer_RADAR_Spiked
    radar_unspiked <- xeredar::Pos_mDHT_Fraunhofer_RADAR_Unspiked

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
    values are box-cox transformed should be used for the analysis
    because only after box-cox transformation, the residuals of the
    mixed ANOVA are normally distributed and have homogeneous variances
    among treatment groups.

<!-- -->

    radar_spiked_result$Justify
    #> [1] "The raw data (without trimming or outlier removal)\n                    where the fluorescence values are box-cox transformed\n                    should be used for the analysis because only after\n                    box-cox transformation, the residuals of the mixed\n                    ANOVA are normally distributed and have homogeneous\n                    variances among treatment groups."

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
<th style="text-align: left;">Replicate</th>
<th style="text-align: left;">Treatment</th>
<th style="text-align: right;">Fluor</th>
<th style="text-align: left;">Conc</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">MT_0</td>
<td style="text-align: right;">113152</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">mDHT-1µg/L+MT</td>
<td style="text-align: right;">708</td>
<td style="text-align: left;">1</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">mDHT-2µg/L+MT</td>
<td style="text-align: right;">2498</td>
<td style="text-align: left;">2</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">mDHT-4µg/L+MT</td>
<td style="text-align: right;">10152</td>
<td style="text-align: left;">4</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">mDHT-8µg/L+MT</td>
<td style="text-align: right;">356</td>
<td style="text-align: left;">8</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">mDHT-16µg/L+MT</td>
<td style="text-align: right;">4621</td>
<td style="text-align: left;">16</td>
</tr>
</tbody>
</table>

    radar_spiked_result$BoxPlots

![](README_files/figure-markdown_strict/unnamed-chunk-16-1.png)

-   Summary tables of the processed data (per replicate and overall)

<!-- -->

    knitr::kable(radar_spiked_result$SummaryDF_Rep, caption="Summary statistics of fluorescence in different concentrations of test item per replicate")

<table>
<caption>Summary statistics of fluorescence in different concentrations
of test item per replicate</caption>
<colgroup>
<col style="width: 7%" />
<col style="width: 14%" />
<col style="width: 4%" />
<col style="width: 12%" />
<col style="width: 26%" />
<col style="width: 35%" />
</colgroup>
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
<td style="text-align: right;">20</td>
<td style="text-align: right;">21472.45</td>
<td style="text-align: right;">39991.342</td>
<td style="text-align: right;">1.862449</td>
</tr>
<tr class="even">
<td style="text-align: left;">0</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">13337.15</td>
<td style="text-align: right;">34300.658</td>
<td style="text-align: right;">2.571813</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">26313.45</td>
<td style="text-align: right;">35370.046</td>
<td style="text-align: right;">1.344181</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">7711.10</td>
<td style="text-align: right;">10559.058</td>
<td style="text-align: right;">1.369332</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">10044.70</td>
<td style="text-align: right;">13997.692</td>
<td style="text-align: right;">1.393540</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">21</td>
<td style="text-align: right;">18764.48</td>
<td style="text-align: right;">27176.392</td>
<td style="text-align: right;">1.448289</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">29476.80</td>
<td style="text-align: right;">44428.181</td>
<td style="text-align: right;">1.507225</td>
</tr>
<tr class="even">
<td style="text-align: left;">2</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">8471.85</td>
<td style="text-align: right;">10013.423</td>
<td style="text-align: right;">1.181964</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">21</td>
<td style="text-align: right;">37028.38</td>
<td style="text-align: right;">50456.149</td>
<td style="text-align: right;">1.362634</td>
</tr>
<tr class="even">
<td style="text-align: left;">4</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">16336.90</td>
<td style="text-align: right;">31480.267</td>
<td style="text-align: right;">1.926943</td>
</tr>
<tr class="odd">
<td style="text-align: left;">4</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">14077.65</td>
<td style="text-align: right;">20278.502</td>
<td style="text-align: right;">1.440475</td>
</tr>
<tr class="even">
<td style="text-align: left;">4</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">22364.45</td>
<td style="text-align: right;">32556.358</td>
<td style="text-align: right;">1.455719</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">5425.90</td>
<td style="text-align: right;">8478.898</td>
<td style="text-align: right;">1.562671</td>
</tr>
<tr class="even">
<td style="text-align: left;">8</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">8867.10</td>
<td style="text-align: right;">14750.623</td>
<td style="text-align: right;">1.663523</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">21992.45</td>
<td style="text-align: right;">30246.253</td>
<td style="text-align: right;">1.375302</td>
</tr>
<tr class="even">
<td style="text-align: left;">16</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">6758.70</td>
<td style="text-align: right;">8394.210</td>
<td style="text-align: right;">1.241986</td>
</tr>
<tr class="odd">
<td style="text-align: left;">16</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">7988.10</td>
<td style="text-align: right;">14300.281</td>
<td style="text-align: right;">1.790198</td>
</tr>
<tr class="even">
<td style="text-align: left;">16</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">15274.35</td>
<td style="text-align: right;">19124.161</td>
<td style="text-align: right;">1.252044</td>
</tr>
</tbody>
</table>

Summary statistics of fluorescence in different concentrations of test
item per replicate

    knitr::kable(radar_spiked_result$SummaryDF, caption="Summary statistics of fluorescence in different concentrations of test item of all replicates")

<table>
<caption>Summary statistics of fluorescence in different concentrations
of test item of all replicates</caption>
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
<td style="text-align: right;">60</td>
<td style="text-align: right;">20374.35</td>
<td style="text-align: right;">36413.54</td>
<td style="text-align: right;">1.787224</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: right;">61</td>
<td style="text-align: right;">12281.48</td>
<td style="text-align: right;">19154.08</td>
<td style="text-align: right;">1.559591</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2</td>
<td style="text-align: right;">61</td>
<td style="text-align: right;">25189.66</td>
<td style="text-align: right;">40666.43</td>
<td style="text-align: right;">1.614410</td>
</tr>
<tr class="even">
<td style="text-align: left;">4</td>
<td style="text-align: right;">60</td>
<td style="text-align: right;">17593.00</td>
<td style="text-align: right;">28378.41</td>
<td style="text-align: right;">1.613052</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8</td>
<td style="text-align: right;">60</td>
<td style="text-align: right;">12095.15</td>
<td style="text-align: right;">20967.68</td>
<td style="text-align: right;">1.733561</td>
</tr>
<tr class="even">
<td style="text-align: left;">16</td>
<td style="text-align: right;">60</td>
<td style="text-align: right;">10007.05</td>
<td style="text-align: right;">14855.60</td>
<td style="text-align: right;">1.484513</td>
</tr>
</tbody>
</table>

Summary statistics of fluorescence in different concentrations of test
item of all replicates

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

<table>
<caption>Increasing Williams’ test</caption>
<colgroup>
<col style="width: 15%" />
<col style="width: 5%" />
<col style="width: 8%" />
<col style="width: 9%" />
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
<td style="text-align: right;">11.1995</td>
<td style="text-align: right;">11.24223</td>
<td style="text-align: right;">-0.04273</td>
<td style="text-align: right;">0.5230995</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.0816862</td>
<td style="text-align: right;">1.971</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-50.88408</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc8 - Conc0</td>
<td style="text-align: left;">8</td>
<td style="text-align: right;">11.1995</td>
<td style="text-align: right;">11.24223</td>
<td style="text-align: right;">-0.04273</td>
<td style="text-align: right;">0.5230995</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.0816862</td>
<td style="text-align: right;">1.965</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-40.63541</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc4 - Conc0</td>
<td style="text-align: left;">4</td>
<td style="text-align: right;">11.1995</td>
<td style="text-align: right;">11.24223</td>
<td style="text-align: right;">-0.04273</td>
<td style="text-align: right;">0.5230995</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.0816862</td>
<td style="text-align: right;">1.956</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-13.65123</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc2 - Conc0</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">11.1995</td>
<td style="text-align: right;">11.24223</td>
<td style="text-align: right;">-0.04273</td>
<td style="text-align: right;">0.5209615</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.0820214</td>
<td style="text-align: right;">1.940</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">23.63416</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc1 - Conc0</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">10.7566</td>
<td style="text-align: right;">11.24223</td>
<td style="text-align: right;">-0.48563</td>
<td style="text-align: right;">0.5209615</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.9321802</td>
<td style="text-align: right;">1.908</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-39.72090</td>
</tr>
</tbody>
</table>

Increasing Williams’ test

    knitr::kable(radar_spiked_result$WilliamsDecrease, caption="Decreasing Williams' test")

<table>
<caption>Decreasing Williams’ test</caption>
<colgroup>
<col style="width: 15%" />
<col style="width: 5%" />
<col style="width: 8%" />
<col style="width: 9%" />
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
<td style="text-align: right;">10.4996</td>
<td style="text-align: right;">11.24223</td>
<td style="text-align: right;">0.74263</td>
<td style="text-align: right;">0.5230995</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">1.4196726</td>
<td style="text-align: right;">1.971</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-50.88408</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc8 - Conc0</td>
<td style="text-align: left;">8</td>
<td style="text-align: right;">10.6478</td>
<td style="text-align: right;">11.24223</td>
<td style="text-align: right;">0.59443</td>
<td style="text-align: right;">0.5230995</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">1.1363613</td>
<td style="text-align: right;">1.965</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-40.63541</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc4 - Conc0</td>
<td style="text-align: left;">4</td>
<td style="text-align: right;">11.4661</td>
<td style="text-align: right;">11.24223</td>
<td style="text-align: right;">-0.22387</td>
<td style="text-align: right;">0.5230995</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.4279683</td>
<td style="text-align: right;">1.956</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-13.65123</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc2 - Conc0</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">11.4661</td>
<td style="text-align: right;">11.24223</td>
<td style="text-align: right;">-0.22387</td>
<td style="text-align: right;">0.5209615</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.4297247</td>
<td style="text-align: right;">1.940</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">23.63416</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc1 - Conc0</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">11.4661</td>
<td style="text-align: right;">11.24223</td>
<td style="text-align: right;">-0.22387</td>
<td style="text-align: right;">0.5209615</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.4297247</td>
<td style="text-align: right;">1.908</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-39.72090</td>
</tr>
</tbody>
</table>

Decreasing Williams’ test

    knitr::kable(radar_spiked_result$Dunnetts, caption="Dunnett's test")

<table style="width:100%;">
<caption>Dunnett’s test</caption>
<colgroup>
<col style="width: 21%" />
<col style="width: 15%" />
<col style="width: 14%" />
<col style="width: 4%" />
<col style="width: 15%" />
<col style="width: 14%" />
<col style="width: 14%" />
</colgroup>
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
<td style="text-align: right;">-0.4981949</td>
<td style="text-align: right;">0.5209615</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-0.9562989</td>
<td style="text-align: right;">0.8003229</td>
<td style="text-align: right;">-39.72090</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc2 - Conc0</td>
<td style="text-align: right;">0.8271841</td>
<td style="text-align: right;">0.5209615</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">1.5878028</td>
<td style="text-align: right;">0.4159647</td>
<td style="text-align: right;">23.63416</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc4 - Conc0</td>
<td style="text-align: right;">0.3056907</td>
<td style="text-align: right;">0.5230995</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">0.5843835</td>
<td style="text-align: right;">0.9619664</td>
<td style="text-align: right;">-13.65123</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc8 - Conc0</td>
<td style="text-align: right;">-0.5944222</td>
<td style="text-align: right;">0.5230995</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-1.1363464</td>
<td style="text-align: right;">0.6892705</td>
<td style="text-align: right;">-40.63541</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc16 - Conc0</td>
<td style="text-align: right;">-0.7426002</td>
<td style="text-align: right;">0.5230995</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">-1.4196155</td>
<td style="text-align: right;">0.5114138</td>
<td style="text-align: right;">-50.88408</td>
</tr>
</tbody>
</table>

Dunnett’s test

-   Further information about the normality test (Shapiro-Wilk), the
    homogeneity of variance test (Levene’s test) of residuals of the
    mixed ANOVA model, the monotonicity test and the model fit.

<!-- -->

    radar_spiked_result$NormalityTest
    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  stats::resid(mixedaov)
    #> W = 0.99162, p-value = 0.03852
    radar_spiked_result$LeveneTest
    #> # A tibble: 1 × 4
    #>   statistic p.value    df df.residual
    #>       <dbl>   <dbl> <int>       <int>
    #> 1     0.328   0.896     5         356
    radar_spiked_result$`Monotonicity Test`
    #>        Test t value Pr(>|t|) Significance
    #> 1    Linear   -1.49   0.1372            .
    #> 2 Quadratic   -2.10   0.0364            *
    radar_spiked_result$MixedAnova
    #> Linear mixed model fit by REML ['lmerMod']
    #> Formula: (Fluor^lambda - 1)/lambda ~ Conc + (1 | Replicate) + (1 | Replicate:Conc)
    #>    Data: dataframe
    #> REML criterion at convergence: 1790.386
    #> Random effects:
    #>  Groups         Name        Std.Dev.
    #>  Replicate:Conc (Intercept) 0.000   
    #>  Replicate      (Intercept) 0.708   
    #>  Residual                   2.865   
    #> Number of obs: 362, groups:  Replicate:Conc, 18; Replicate, 3
    #> Fixed Effects:
    #> (Intercept)       Conc.L       Conc.Q       Conc.C       Conc^4       Conc^5  
    #>    11.12517     -0.54062     -0.78033     -0.07106      0.90730     -0.34498  
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
<td style="text-align: left;">-64.09</td>
<td style="text-align: left;">37.28</td>
<td style="text-align: left;">-23.92</td>
<td style="text-align: left;">-74.73</td>
<td style="text-align: left;">-68.52</td>
</tr>
<tr class="even">
<td style="text-align: left;">Replicate 2</td>
<td style="text-align: left;">-24.69</td>
<td style="text-align: left;">-36.48</td>
<td style="text-align: left;">5.55</td>
<td style="text-align: left;">-33.52</td>
<td style="text-align: left;">-40.11</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Replicate 3</td>
<td style="text-align: left;">-28.69</td>
<td style="text-align: left;">40.72</td>
<td style="text-align: left;">-15.01</td>
<td style="text-align: left;">-16.42</td>
<td style="text-align: left;">-41.95</td>
</tr>
<tr class="even">
<td style="text-align: left;">Pooled</td>
<td style="text-align: left;">-39.72</td>
<td style="text-align: left;">23.63</td>
<td style="text-align: left;">-13.65</td>
<td style="text-align: left;">-40.64</td>
<td style="text-align: left;">-50.88</td>
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

# Running default REACTIV analysis

To demonstrate how to run REACTIV analysis, we will use one artificial
data set containing the spiked and unspiked measurements.

    reactiv_spiked <- reactiv_data |>
      dplyr::filter(Spiked == TRUE)
    reactiv_unspiked <- reactiv_data |>
      dplyr::filter(Spiked == FALSE)

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
<th style="text-align: left;">Replicate</th>
<th style="text-align: left;">Conc</th>
<th style="text-align: right;">Fluor</th>
<th style="text-align: left;">Treatment</th>
<th style="text-align: left;">Spiked</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">0</td>
<td style="text-align: right;">1801520</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">TRUE</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">0</td>
<td style="text-align: right;">1517719</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">TRUE</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">0</td>
<td style="text-align: right;">2517533</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">TRUE</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">0</td>
<td style="text-align: right;">3744706</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">TRUE</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">0</td>
<td style="text-align: right;">2744990</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">TRUE</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">0</td>
<td style="text-align: right;">2889488</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">TRUE</td>
</tr>
</tbody>
</table>

    reactiv_spiked_result$BoxPlots

![](README_files/figure-markdown_strict/unnamed-chunk-24-1.png)

-   Summary tables of the processed data (per replicate and overall)

<!-- -->

    knitr::kable(reactiv_spiked_result$SummaryDF_Rep, caption="Summary statistics of fluorescence in different concentrations of test item per replicate")

<table>
<caption>Summary statistics of fluorescence in different concentrations
of test item per replicate</caption>
<colgroup>
<col style="width: 7%" />
<col style="width: 14%" />
<col style="width: 4%" />
<col style="width: 12%" />
<col style="width: 26%" />
<col style="width: 35%" />
</colgroup>
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
<td style="text-align: right;">16</td>
<td style="text-align: right;">2486847</td>
<td style="text-align: right;">1154357.6</td>
<td style="text-align: right;">0.4641853</td>
</tr>
<tr class="even">
<td style="text-align: left;">0</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">16</td>
<td style="text-align: right;">3341118</td>
<td style="text-align: right;">797137.2</td>
<td style="text-align: right;">0.2385839</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">16</td>
<td style="text-align: right;">2783708</td>
<td style="text-align: right;">1049011.8</td>
<td style="text-align: right;">0.3768398</td>
</tr>
<tr class="even">
<td style="text-align: left;">0.16</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">2748986</td>
<td style="text-align: right;">550546.7</td>
<td style="text-align: right;">0.2002727</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0.16</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">2382941</td>
<td style="text-align: right;">636560.2</td>
<td style="text-align: right;">0.2671321</td>
</tr>
<tr class="even">
<td style="text-align: left;">0.16</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">2705488</td>
<td style="text-align: right;">981794.9</td>
<td style="text-align: right;">0.3628901</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0.8</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">2683131</td>
<td style="text-align: right;">1093419.6</td>
<td style="text-align: right;">0.4075163</td>
</tr>
<tr class="even">
<td style="text-align: left;">0.8</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">2567366</td>
<td style="text-align: right;">412800.9</td>
<td style="text-align: right;">0.1607877</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0.8</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">2860645</td>
<td style="text-align: right;">499925.0</td>
<td style="text-align: right;">0.1747595</td>
</tr>
<tr class="even">
<td style="text-align: left;">4</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">2885645</td>
<td style="text-align: right;">850608.9</td>
<td style="text-align: right;">0.2947725</td>
</tr>
<tr class="odd">
<td style="text-align: left;">4</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">3581072</td>
<td style="text-align: right;">837331.5</td>
<td style="text-align: right;">0.2338214</td>
</tr>
<tr class="even">
<td style="text-align: left;">4</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">2870297</td>
<td style="text-align: right;">644055.0</td>
<td style="text-align: right;">0.2243862</td>
</tr>
<tr class="odd">
<td style="text-align: left;">20</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">6389817</td>
<td style="text-align: right;">1455541.4</td>
<td style="text-align: right;">0.2277908</td>
</tr>
<tr class="even">
<td style="text-align: left;">20</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">6142859</td>
<td style="text-align: right;">1499704.2</td>
<td style="text-align: right;">0.2441378</td>
</tr>
<tr class="odd">
<td style="text-align: left;">20</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">6533288</td>
<td style="text-align: right;">1256422.3</td>
<td style="text-align: right;">0.1923109</td>
</tr>
<tr class="even">
<td style="text-align: left;">100</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">5</td>
<td style="text-align: right;">10533255</td>
<td style="text-align: right;">1668866.7</td>
<td style="text-align: right;">0.1584379</td>
</tr>
<tr class="odd">
<td style="text-align: left;">100</td>
<td style="text-align: left;">2</td>
<td style="text-align: right;">4</td>
<td style="text-align: right;">10248429</td>
<td style="text-align: right;">1982668.2</td>
<td style="text-align: right;">0.1934607</td>
</tr>
<tr class="even">
<td style="text-align: left;">100</td>
<td style="text-align: left;">3</td>
<td style="text-align: right;">4</td>
<td style="text-align: right;">9080113</td>
<td style="text-align: right;">1215680.9</td>
<td style="text-align: right;">0.1338839</td>
</tr>
</tbody>
</table>

Summary statistics of fluorescence in different concentrations of test
item per replicate

    knitr::kable(reactiv_spiked_result$SummaryDF, caption="Summary statistics of fluorescence in different concentrations of test item of all replicates")

<table>
<caption>Summary statistics of fluorescence in different concentrations
of test item of all replicates</caption>
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
<td style="text-align: right;">48</td>
<td style="text-align: right;">2870558</td>
<td style="text-align: right;">1052304.4</td>
<td style="text-align: right;">0.3665854</td>
</tr>
<tr class="even">
<td style="text-align: left;">0.16</td>
<td style="text-align: right;">24</td>
<td style="text-align: right;">2612472</td>
<td style="text-align: right;">732636.7</td>
<td style="text-align: right;">0.2804381</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0.8</td>
<td style="text-align: right;">24</td>
<td style="text-align: right;">2703714</td>
<td style="text-align: right;">712021.2</td>
<td style="text-align: right;">0.2633493</td>
</tr>
<tr class="even">
<td style="text-align: left;">4</td>
<td style="text-align: right;">24</td>
<td style="text-align: right;">3112338</td>
<td style="text-align: right;">821286.1</td>
<td style="text-align: right;">0.2638807</td>
</tr>
<tr class="odd">
<td style="text-align: left;">20</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">6353596</td>
<td style="text-align: right;">1339757.0</td>
<td style="text-align: right;">0.2108659</td>
</tr>
<tr class="even">
<td style="text-align: left;">100</td>
<td style="text-align: right;">13</td>
<td style="text-align: right;">9998496</td>
<td style="text-align: right;">1643685.7</td>
<td style="text-align: right;">0.1643933</td>
</tr>
</tbody>
</table>

Summary statistics of fluorescence in different concentrations of test
item of all replicates

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

<table>
<caption>Increasing Williams’ test</caption>
<colgroup>
<col style="width: 17%" />
<col style="width: 5%" />
<col style="width: 8%" />
<col style="width: 9%" />
<col style="width: 11%" />
<col style="width: 9%" />
<col style="width: 3%" />
<col style="width: 11%" />
<col style="width: 9%" />
<col style="width: 6%" />
<col style="width: 11%" />
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
<td style="text-align: right;">3152.32</td>
<td style="text-align: right;">1661.366</td>
<td style="text-align: right;">1490.95383</td>
<td style="text-align: right;">97.89606</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">15.2299684</td>
<td style="text-align: right;">1.998</td>
<td style="text-align: left;">TRUE</td>
<td style="text-align: right;">248.311979</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc20 - Conc0</td>
<td style="text-align: left;">20</td>
<td style="text-align: right;">2506.80</td>
<td style="text-align: right;">1661.366</td>
<td style="text-align: right;">845.43383</td>
<td style="text-align: right;">87.04570</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">9.7125278</td>
<td style="text-align: right;">1.991</td>
<td style="text-align: left;">TRUE</td>
<td style="text-align: right;">121.336658</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc4 - Conc0</td>
<td style="text-align: left;">4</td>
<td style="text-align: right;">1748.86</td>
<td style="text-align: right;">1661.366</td>
<td style="text-align: right;">87.49383</td>
<td style="text-align: right;">83.38750</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">1.0492440</td>
<td style="text-align: right;">1.981</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">8.422775</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc0.8 - Conc0</td>
<td style="text-align: left;">0.8</td>
<td style="text-align: right;">1629.52</td>
<td style="text-align: right;">1661.366</td>
<td style="text-align: right;">-31.84617</td>
<td style="text-align: right;">83.38750</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">-0.3819058</td>
<td style="text-align: right;">1.965</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-5.812226</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc0.16 - Conc0</td>
<td style="text-align: left;">0.16</td>
<td style="text-align: right;">1598.55</td>
<td style="text-align: right;">1661.366</td>
<td style="text-align: right;">-62.81617</td>
<td style="text-align: right;">83.38750</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">-0.7533044</td>
<td style="text-align: right;">1.931</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-8.990784</td>
</tr>
</tbody>
</table>

Increasing Williams’ test

    knitr::kable(reactiv_spiked_result$WilliamsDecrease, caption="Decreasing Williams' test")

<table>
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
<col style="width: 6%" />
<col style="width: 11%" />
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
<td style="text-align: right;">2007.29</td>
<td style="text-align: right;">1661.366</td>
<td style="text-align: right;">-345.9238</td>
<td style="text-align: right;">97.89606</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">-3.533583</td>
<td style="text-align: right;">1.998</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">248.311979</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc20 - Conc0</td>
<td style="text-align: left;">20</td>
<td style="text-align: right;">2007.29</td>
<td style="text-align: right;">1661.366</td>
<td style="text-align: right;">-345.9238</td>
<td style="text-align: right;">87.04570</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">-3.974048</td>
<td style="text-align: right;">1.991</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">121.336658</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc4 - Conc0</td>
<td style="text-align: left;">4</td>
<td style="text-align: right;">2007.29</td>
<td style="text-align: right;">1661.366</td>
<td style="text-align: right;">-345.9238</td>
<td style="text-align: right;">83.38750</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">-4.148390</td>
<td style="text-align: right;">1.981</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">8.422775</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc0.8 - Conc0</td>
<td style="text-align: left;">0.8</td>
<td style="text-align: right;">2007.29</td>
<td style="text-align: right;">1661.366</td>
<td style="text-align: right;">-345.9238</td>
<td style="text-align: right;">83.38750</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">-4.148390</td>
<td style="text-align: right;">1.965</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-5.812226</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc0.16 - Conc0</td>
<td style="text-align: left;">0.16</td>
<td style="text-align: right;">2007.29</td>
<td style="text-align: right;">1661.366</td>
<td style="text-align: right;">-345.9238</td>
<td style="text-align: right;">83.38750</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">-4.148390</td>
<td style="text-align: right;">1.931</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">-8.990784</td>
</tr>
</tbody>
</table>

Decreasing Williams’ test

    knitr::kable(reactiv_spiked_result$Dunnetts, caption="Dunnett's test")

<table style="width:100%;">
<caption>Dunnett’s test</caption>
<colgroup>
<col style="width: 23%" />
<col style="width: 15%" />
<col style="width: 12%" />
<col style="width: 4%" />
<col style="width: 15%" />
<col style="width: 13%" />
<col style="width: 15%" />
</colgroup>
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
<td style="text-align: left;">Conc0.16 - Conc0</td>
<td style="text-align: right;">-62.81884</td>
<td style="text-align: right;">83.38750</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">-0.7533364</td>
<td style="text-align: right;">0.9231737</td>
<td style="text-align: right;">-8.990784</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc0.8 - Conc0</td>
<td style="text-align: right;">-31.84356</td>
<td style="text-align: right;">83.38750</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">-0.3818746</td>
<td style="text-align: right;">0.9953441</td>
<td style="text-align: right;">-5.812226</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc4 - Conc0</td>
<td style="text-align: right;">87.49635</td>
<td style="text-align: right;">83.38750</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">1.0492743</td>
<td style="text-align: right;">0.7798328</td>
<td style="text-align: right;">8.422775</td>
</tr>
<tr class="even">
<td style="text-align: left;">Conc20 - Conc0</td>
<td style="text-align: right;">845.49890</td>
<td style="text-align: right;">87.04570</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">9.7132754</td>
<td style="text-align: right;">0.0000051</td>
<td style="text-align: right;">121.336658</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Conc100 - Conc0</td>
<td style="text-align: right;">1489.69562</td>
<td style="text-align: right;">97.89606</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">15.2171158</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">248.311979</td>
</tr>
</tbody>
</table>

Dunnett’s test

-   Further information about the normality test (Shapiro-Wilk), the
    homogeneity of variance test (Levene’s test) of residuals of the
    mixed ANOVA model, the monotonicity test and the model fit.

<!-- -->

    reactiv_spiked_result$NormalityTest
    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  stats::resid(mixedaov)
    #> W = 0.98187, p-value = 0.04164
    reactiv_spiked_result$LeveneTest
    #> # A tibble: 1 × 4
    #>   statistic p.value    df df.residual
    #>       <dbl>   <dbl> <int>       <int>
    #> 1      1.59   0.167     5         147
    reactiv_spiked_result$`Monotonicity Test`
    #>        Test t value Pr(>|t|) Significance
    #> 1    Linear   12.67  <0.0001          ***
    #> 2 Quadratic    6.66  <0.0001          ***
    reactiv_spiked_result$MixedAnova
    #> Linear mixed model fit by REML ['lmerMod']
    #> Formula: sqrt(Fluor) ~ Conc + (1 | Replicate) + (1 | Replicate:Conc)
    #>    Data: wt_outlier
    #> REML criterion at convergence: 2090.548
    #> Random effects:
    #>  Groups         Name        Std.Dev. 
    #>  Replicate:Conc (Intercept)  58.87516
    #>  Replicate      (Intercept)   0.01916
    #>  Residual                   272.54703
    #> Number of obs: 153, groups:  Replicate:Conc, 18; Replicate, 3
    #> Fixed Effects:
    #> (Intercept)       Conc.L       Conc.Q       Conc.C       Conc^4       Conc^5  
    #>     2049.37      1230.22       703.01        45.68      -141.18      -117.07  
    #> optimizer (nloptwrap) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings

*The list output from running the data\_prep() function can be
summarized with the data\_summary() function.*

    xeredar::data_summary(reactiv_spiked_result) |>
      knitr::kable()

<table>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">0.16</th>
<th style="text-align: left;">0.8</th>
<th style="text-align: left;">4</th>
<th style="text-align: left;">20</th>
<th style="text-align: left;">100</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Replicate 1</td>
<td style="text-align: left;">10.54</td>
<td style="text-align: left;">7.89</td>
<td style="text-align: left;">16.04</td>
<td style="text-align: left;">156.94</td>
<td style="text-align: left;">323.56</td>
</tr>
<tr class="even">
<td style="text-align: left;">Replicate 2</td>
<td style="text-align: left;">-28.68</td>
<td style="text-align: left;">-23.16</td>
<td style="text-align: left;">7.18</td>
<td style="text-align: left;">83.86</td>
<td style="text-align: left;">206.74</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Replicate 3</td>
<td style="text-align: left;">-2.81</td>
<td style="text-align: left;">2.76</td>
<td style="text-align: left;">3.11</td>
<td style="text-align: left;">134.7</td>
<td style="text-align: left;">226.19</td>
</tr>
<tr class="even">
<td style="text-align: left;">Pooled</td>
<td style="text-align: left;">-8.99</td>
<td style="text-align: left;">-5.81</td>
<td style="text-align: left;">8.42</td>
<td style="text-align: left;">121.34</td>
<td style="text-align: left;">248.31</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Dunnett</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">ns</td>
<td style="text-align: left;">*</td>
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

# Validation

In order to validate the package, we will analyze all 36 XETA ring test
studies and produce a table that can be compared with the table in the
validation report. The validation studies are available in the `xeredar`
package. In case the produced table is in line with the table
description, then the package is doing exactly what it is supposed to do
and analyzed all 36 XETA ring test studies correctly.




    out <-  !stringr::str_detect(names(xeredar::valid_data_xeta), "_combined$" )
    list_dfs <- xeredar::valid_data_xeta[out]
    data_prep_trim <- function(df){
      df <- df |> dplyr::filter(!is.na(Fluor))
      dat_trim <- xeredar::trim(df)$dat_trim
      results <- xeredar::data_prep(dat_trim, row= FALSE, trim=FALSE, outlier = FALSE)
      return(results)
    }
    results <- purrr::map(list_dfs, data_prep_trim,  .progress = TRUE)
    worked <- !purrr::map_vec(results, is.list)

    difficult_dfs <- list_dfs[worked]

    changing_alpha <- c(c(1,5) %o% 10^-(3:17), 0)

    decreasing_alpha <- function(df, vector){
      for(i in vector){
        df <- df |> filter(!is.na(Fluor))
        dat_trim <- xeredar::trim(df)$dat_trim
        result <- data_prep(df, row=FALSE, alpha = i, trim=FALSE, outlier = FALSE)
        if(is.list(result)){
          break
        } else{
          next
        }
      }
      result[["alpha"]] <- i
      return(result)
    }


    result_difficult_dfs <- purrr::map(.x=difficult_dfs, .f= decreasing_alpha, vector = changing_alpha)
    results_alpha_decrease <- results[map_vec(results, is.list)]

    results <- append(results_alpha_decrease, result_difficult_dfs)
    check_positive <- function(df){
      if(df[["Monotonicity Test"]]$`Pr(>|t|)`[1] >= 0.01 &
         df[["Monotonicity Test"]]$`Pr(>|t|)`[2] <= 0.01 ) {
        return(df[["Dunnetts"]])
      } else if (length(unique(df$WilliamsIncrease$Y.Tilde)) - 1 > floor(nrow(df$WilliamsIncrease)/2) | 
                 length(unique(df$WilliamsDecrease$Y.Tilde)) - 1 > floor(nrow(df$WilliamsDecrease)/2)) { 
        return(list(WilliamsIncrease = df[["WilliamsIncrease"]], WilliamsDecrease = df[["WilliamsDecrease"]])) 
      } else{
        return(df[["Dunnetts"]])
        } 
    }

    results_2 <- purrr::map(results, check_positive)
    Williams <- !purrr::map_vec(results_2, is.data.frame)

    significances <- function(df, Williams= FALSE){
      if(Williams) {
        if(length(unique(df$WilliamsIncrease$Y.Tilde)) - 1 > 
           floor(nrow(df$WilliamsIncrease)/2)){
          TRUE %in% df[["WilliamsIncrease"]]$Sign
        } else{
          TRUE %in% df[["WilliamsDecrease"]]$Sign
        }
      } else{
        df[["adj p"]][length(df[["adj p"]])] < 0.05
      }
    }

    prelim_thyroid_active <- purrr::map2_vec(results_2, Williams, significances)

    country_substance_spiked <- names(results_2) |> stringr::str_split("\\_") |> as.data.frame() |> t() |> dplyr::as_tibble() |> dplyr::rename("substance" = "V1", "country" = "V2", "spiked" = "V3") |> dplyr::mutate(Sign = prelim_thyroid_active)


    increase_h12 <- results |> purrr::map_vec(function(x) {(x[["SummaryDF"]]$Mean[nrow(x[["SummaryDF"]])] / x[["SummaryDF"]]$Mean[1])-1 })


    thyroid_active <- country_substance_spiked |> dplyr::mutate(Increase = increase_h12,
                                                                thyroid_active = ifelse(Increase >= 0.12 & Sign == TRUE & spiked == "unspiked" |
                                                                                          abs(Increase) >= 0.12 & Sign == TRUE & spiked == "spiked" , "Thyroid active", "Thyroid inactive")) |> 
      dplyr::select(!Sign, !Increase) |> 
      dplyr::summarise(thyroid_active = ifelse("Thyroid active" %in% thyroid_active, "Thyroid active", "Thyroid inactive"), .by= c(substance, country)) |> 
      dplyr::mutate(
        country = ifelse(country== "japan" & substance %in% c("e2", "nh3") |
                           country == "japan2", "japan lab2", country)) |>
      pivot_wider(names_from = country, values_from = thyroid_active) |>
      dplyr::arrange(match(substance, c("t3", "ptu", "t4", "triac", "cefuroxime","linuron", "nh3", "testosterone","e2", "abamectine", "acetone", "isophorone", "metholmyl"))) |> 
      mutate(`Expected classification`= c("Thyroid active", "Thyroid active", "Thyroid active", "Thyroid active", "Thyroid inactive", "Thyroid active", "Thyroid active", "Unclear", "Thyroid inactive", "Thyroid inactive","Thyroid inactive","Thyroid inactive","Thyroid inactive"))|>
      dplyr::relocate(c(substance,`Expected classification`, france,japan, usa, `japan lab2`, belgium, portugal ))  |> 
      dplyr::rename(USA = usa)

    gt::gt(thyroid_active, rowname_col = "substance") |>
      gt::tab_options(column_labels.text_transform = "capitalize",
                  stub.text_transform = "uppercase") |>
      gt::data_color(columns= -substance, na_color = "white", palette = c("#c2cd94", "#b4c6e7", "white")) |>
      gt::tab_stubhead(label = "Chemical")|>
      gt::tab_spanner(
        label = "Laboratory",
        columns = c(france, japan, USA, 'japan lab2', belgium, portugal)
      ) |>
        gt::tab_style(
        style = list(
          gt::cell_text(color = "#D9654B", weight= "bold")
        ),
        locations = gt::cells_body(
          columns = c(france, belgium), # not needed if coloring all columns
          rows = 8)
      )  |> 
       gt::tab_header(
        title = "Summary table of results from statistical analysis of all 36 validation studies of the ring test of the XETA test guideline. This table shows the results of the automatic analysis carried out with the data_prep() function of the xeredar R-package. Furthermore, this table summarises the results of the original analysis carried out by John Green using SAS. As both approaches led to exactly the same conclusions, only one table is shown. In case a substance was identified to be Thyroid active, the respective cell is colored green, whereas when a substance was identified to be Thyroid inactive, the cell was colored blue. For the study applying testosterone in the french lab, the writing is colored red, because in the original table in the validation document, this substance was incorrectly classified as Thyroid active due to a data transfer mistake. Furthermore, for the study applying testosterone in the belgium lab, the writing is colored red, as well, because in the validation document, a slip in the column led to incorrectly categorizing the respective results as carried out by the japanese lab2.")


</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="8" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Summary table of results from statistical analysis of all 36 validation studies of the ring test of the XETA test guideline. This table shows the results of the automatic analysis carried out with the data_prep() function of the xeredar R-package. Furthermore, this table summarises the results of the original analysis carried out by John Green using SAS. As both approaches led to exactly the same conclusions, only one table is shown. In case a substance was identified to be Thyroid active, the respective cell is colored green, whereas when a substance was identified to be Thyroid inactive, the cell was colored blue. For the study applying testosterone in the french lab, the writing is colored red, because in the original table in the validation document, this substance was incorrectly classified as Thyroid active due to a data transfer mistake. Furthermore, for the study applying testosterone in the belgium lab, the writing is colored red, as well, because in the validation document, a slip in the column led to incorrectly categorizing the respective results as carried out by the japanese lab2.</td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="Chemical">Chemical</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="Expected classification">Expected classification</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="6" scope="colgroup" id="Laboratory">
        <span class="gt_column_spanner">Laboratory</span>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="france">france</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="japan">japan</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="USA">USA</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="japan lab2">japan lab2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="belgium">belgium</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="portugal">portugal</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub">t3</th>
<td headers="stub_1_1 Expected classification" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_1 france" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_1 japan" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_1 USA" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_1 japan lab2" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_1 belgium" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_1 portugal" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub">ptu</th>
<td headers="stub_1_2 Expected classification" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_2 france" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_2 japan" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_2 USA" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_2 japan lab2" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_2 belgium" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_2 portugal" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_left gt_stub">t4</th>
<td headers="stub_1_3 Expected classification" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_3 france" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_3 japan" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_3 USA" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_3 japan lab2" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_3 belgium" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_3 portugal" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td></tr>
    <tr><th id="stub_1_4" scope="row" class="gt_row gt_left gt_stub">triac</th>
<td headers="stub_1_4 Expected classification" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_4 france" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_4 japan" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_4 USA" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_4 japan lab2" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_4 belgium" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_4 portugal" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td></tr>
    <tr><th id="stub_1_5" scope="row" class="gt_row gt_left gt_stub">cefuroxime</th>
<td headers="stub_1_5 Expected classification" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td>
<td headers="stub_1_5 france" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td>
<td headers="stub_1_5 japan" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td>
<td headers="stub_1_5 USA" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td>
<td headers="stub_1_5 japan lab2" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_5 belgium" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_5 portugal" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td></tr>
    <tr><th id="stub_1_6" scope="row" class="gt_row gt_left gt_stub">linuron</th>
<td headers="stub_1_6 Expected classification" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_6 france" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_6 japan" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_6 USA" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_6 japan lab2" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_6 belgium" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_6 portugal" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td></tr>
    <tr><th id="stub_1_7" scope="row" class="gt_row gt_left gt_stub">nh3</th>
<td headers="stub_1_7 Expected classification" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_7 france" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_7 japan" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_7 USA" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_7 japan lab2" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td>
<td headers="stub_1_7 belgium" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_7 portugal" class="gt_row gt_left" style="background-color: #C2CD94; color: #000000;">Thyroid active</td></tr>
    <tr><th id="stub_1_8" scope="row" class="gt_row gt_left gt_stub">testosterone</th>
<td headers="stub_1_8 Expected classification" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">Unclear</td>
<td headers="stub_1_8 france" class="gt_row gt_left" style="background-color: #B4C6E7; color: #D9654B; font-weight: bold;">Thyroid inactive</td>
<td headers="stub_1_8 japan" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_8 USA" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_8 japan lab2" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_8 belgium" class="gt_row gt_left" style="background-color: #B4C6E7; color: #D9654B; font-weight: bold;">Thyroid inactive</td>
<td headers="stub_1_8 portugal" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td></tr>
    <tr><th id="stub_1_9" scope="row" class="gt_row gt_left gt_stub">e2</th>
<td headers="stub_1_9 Expected classification" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td>
<td headers="stub_1_9 france" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td>
<td headers="stub_1_9 japan" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_9 USA" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_9 japan lab2" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td>
<td headers="stub_1_9 belgium" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td>
<td headers="stub_1_9 portugal" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td></tr>
    <tr><th id="stub_1_10" scope="row" class="gt_row gt_left gt_stub">acetone</th>
<td headers="stub_1_10 Expected classification" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td>
<td headers="stub_1_10 france" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td>
<td headers="stub_1_10 japan" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_10 USA" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_10 japan lab2" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_10 belgium" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_10 portugal" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td></tr>
    <tr><th id="stub_1_11" scope="row" class="gt_row gt_left gt_stub">isophorone</th>
<td headers="stub_1_11 Expected classification" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td>
<td headers="stub_1_11 france" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td>
<td headers="stub_1_11 japan" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_11 USA" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_11 japan lab2" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_11 belgium" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_11 portugal" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td></tr>
    <tr><th id="stub_1_12" scope="row" class="gt_row gt_left gt_stub">abamactine</th>
<td headers="stub_1_12 Expected classification" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td>
<td headers="stub_1_12 france" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td>
<td headers="stub_1_12 japan" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_12 USA" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_12 japan lab2" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_12 belgium" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_12 portugal" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td></tr>
    <tr><th id="stub_1_13" scope="row" class="gt_row gt_left gt_stub">methomyl</th>
<td headers="stub_1_13 Expected classification" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td>
<td headers="stub_1_13 france" class="gt_row gt_left" style="background-color: #B4C6E7; color: #000000;">Thyroid inactive</td>
<td headers="stub_1_13 japan" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_13 USA" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_13 japan lab2" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_13 belgium" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td>
<td headers="stub_1_13 portugal" class="gt_row gt_left" style="background-color: #FFFFFF; color: #000000;">NA</td></tr>
  </tbody>
  
  
</table>
</div>

# The shiny app

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

# References

OECD. 2019a. Validation Report of the Xenopus Eleutheroembryonic Thyroid
Signaling Assay (XETA) for the Detection of Thyroid Active Substances.
OECD.

OECD. 2019b. TG 248: Xenopus Eleutheroembryonic Thyroid Assay (XETA).
OECD. <https://doi.org/10.1787/a13f80ee-en>.

OECD. 2022b. Test No. 251: Rapid Androgen Disruption Activity Reporter
(RADAR) Assay. OECD. <https://doi.org/10.1787/da264d82-en>.

OECD. 2022a. Rapid Estrogen Activity in Vivo (REACTIV) Assay (OECD Draft
TG): Guideline for the Testing of Chemicals, Section 2: Effects on
Biotic System. OECD.

Spyridonov I, Yan L, Szoecs E, Miranda AFP, Lange C, Tindall A, Du
Pasquier D, Lemkine G, Weltje L, Habekost M, Thorbek P. unpublished.
“xeredar: An Open-Source R-Package for the Statistical Analysis of
Endocrine New Approach Methods (NAMs) using Fish or Amphibian
Eleutheroembryos”.
