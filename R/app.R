#' Start the shiny application for xeredar
#' @export
#' @importFrom shiny runApp
run_app <- function() {
  shiny::runApp(system.file("app", package = "xeredar"))
}

#' UI function for shiny App
#' @param request request to app (for bookmarking)
#' @import shinydashboard shiny shinyWidgets
app_ui <- function(request) {
  shinydashboard::dashboardPage(
    skin = "green",
    title = "xeredar",
    shinydashboard::dashboardHeader(title = "xeredar"),
    shinydashboard::dashboardSidebar(disable = TRUE, collapsed = TRUE),
    shinydashboard::dashboardBody(
      tags$head(
        tags$style(HTML("
          .sw-dropdown-content {
            z-index: 1000;
          }
        "))
      ),
      fluidRow(
        column(2,
               box(title = div(
                 div_inline(HTML("Inputs")),
                 div_inline(dropdown(
                   prettySwitch("dagostino",
                                label = "Use d'Agostino test?",
                                status = "success",
                                fill = TRUE,
                                value = FALSE
                   ),
                   prettySwitch("trim",
                                label = "Apply 10% Trimming?",
                                status = "success",
                                fill = TRUE,
                                value = TRUE
                   ),
                   prettySwitch("outlier",
                                label = "Remove outliers?",
                                status = "success",
                                fill = TRUE,
                                value = TRUE
                   ),
                   prettySwitch("boxcox",
                                label = "Try BoxCox transformation?",
                                status = "success",
                                fill = TRUE,
                                value = TRUE
                   ),
                   sliderInput("alpha",
                               label = "alpha for residual normality and variance homogeneity tests",
                               min = 0,
                               max = 0.1,
                               value = 0.05,
                               step = 0.01
                   ),
                   status = "success",
                   icon = icon("cog"),
                   width = "350px",
                   tooltip = tooltipOptions(title = "Click to change settings !")
                 ))
               ),
               width = NULL,
               awesomeCheckbox("row",
                               label = "Exposure well ID (Row column) as random term?",
                               value = TRUE
               ),

               selectInput("own_data", label = "Do you want to upload own data
                          or do you want to load example data?",
                           choices = c("upload", "example"), selected = "example"
               ),

               conditionalPanel(
                 condition = "input.own_data == 'upload'",
                 bslib::tooltip("path",
                                title = "The dataframe should only contain the original
                              spiked or unspiked samples without positive control. The
                              decimal separator has got to be a period. The
                              following columns are required:Fluor: where all fluorescence
                              measurements are stored which are NOT NORMALIZED; Conc: containing
                              the treatment concentrations WITHOUT UNIT. It is important that
                              the rows of the controls measurements have Conc = 0; Treatment:
                              containing the treatment information; Replicate: containing the
                              number of the Replicates, also referred to as Run.",
                                placement = "top",
                                trigger = "Data requirements",
                                options = list(container = "body")),
                 fileInput("path", "Choose xlsx File", accept = ".xlsx")
               )
               ,
               downloadButton("download_report", "Word report")


               )),
        column(10,
               box(title = "Information about data processing",
                   width = NULL, shiny::textOutput("justify")),
               box(title = "Conclusion", width = NULL,
                   gt::gt_output("conclusion_summary"),
                   box(title = "Uploaded data",
                       width = NULL, collapsible = TRUE, collapsed = TRUE,
                       gt::gt_output("data")
                   ),
                   box(title = "Residual diagnostics plots", subtitle= "Residual diagnostics plots using
           randomized quantile residuals calculated from the R-package DHARMa.",  width = NULL,
                       plotOutput("residual_diag_plots"),
                       collapsed = TRUE
                   ),
                   box(title = "Boxplots", width = NULL,
                       plotOutput("boxplot")
                   ),
                   box(title = "Data summary among treatment groups", width = NULL,
                       gt::gt_output("summary")
                   ),

               ),
               box(title = "Test for residual normality",
                   width = NULL, collapsible = TRUE, collapsed = TRUE,
                   shiny::verbatimTextOutput("normality")
               ),
               box(title = "Test for variance homogeneity among treatment groups",
                   width = NULL, collapsible = TRUE, collapsed = TRUE,
                   gt::gt_output("levene")
               ),
               box(title = "Monotonicity test result" ,
                   width = NULL, collapsible = TRUE, collapsed = TRUE,
                   gt::gt_output("monotonicity")
               ),
               box(title = "Dunnett's two-sided test result",
                   width = NULL, collapsible = TRUE, collapsed = FALSE,
                   gt::gt_output("dunnett")
               ),
               box(title = "Increasing Williams trend test result",
                   width = NULL, collapsible = TRUE, collapsed = FALSE,
                   gt::gt_output("williamsincrease")
               ),
               box(title = "Decreasing Williams trend test result",
                   width = NULL, collapsible = TRUE, collapsed = FALSE,
                   gt::gt_output("williamsdecrease")
               ),
               box(title = "Summary table of Mixed ANOVA model",
                   width = NULL, collapsible = TRUE, collapsed = FALSE,
                   gt::gt_output("mixedmodel")
               ),
        )
      )
    ))
}


#' Server function
#' @param input internal
#' @param output internal
#' @param session internal
app_server <- function(input, output, session) {

  data <- reactive({

    req(!is.null(input$row))
    req(input$own_data)

    if (input$own_data == "example") {
      if (input$row) {
        return(xeredar::testDataSpiked)
      } else {
        blub <- xeredar::valid_data_xeta[["triac_japan_unspiked"]] |>
          dplyr::select(Replicate, Treatment, Fluor, Conc)
        return(blub)
      }
    } else {
      if (is.null(input$path)) {
        return(NULL)
      }
      # Read the data from the uploaded file
      else if(input$row){
        print(input$path$datapath)
        blub <- readxl::read_excel(input$path$datapath) |>
          dplyr::mutate_at(c("Replicate", "Treatment"), as.character) |>
          dplyr::mutate_at(c("Fluor"), as.numeric) |>
          dplyr::mutate_at(c("Conc", "Row"), factor, ordered = TRUE)
        return(blub)
      } else{
        blub <- readxl::read_excel(input$path$datapath) |>
          dplyr::mutate_at(c("Replicate", "Treatment"), as.character) |>
          dplyr::mutate_at(c("Fluor"), as.numeric) |>
          dplyr::mutate_at(c("Conc"), factor, ordered = TRUE)
        return(blub)
      }

    }
  })


  # validate user input
  validated_data <- reactive({
    req(!is.null(input$row))
    req(data())

    valid <- data() |>
      validate_dfxeta(row = input$row)

    if (!inherits(valid, "try-error")) {
      out <- data()
      return(out)

    } else {
      shiny::showNotification(
        paste0("The data you supplied is not valid: ", attr(valid, "condition")$message),
        type = "error",
        duration = 5
      )
      return(NULL)
    }
  })



  # compute results
  results <- reactive({
    req(validated_data())
    req(!is.null(input$row))

    try(xeredar::data_prep(validated_data(),
                           row = input$row,
                           dagostino = input$dagostino,
                           trimming = input$trim,
                           outlier = input$outlier,
                           boxcox = input$boxcox,
                           artc = FALSE,
                           alpha = input$alpha))
  })

  summary <- reactive({
    req(results())
    try(xeredar::data_summary(results()) |>
          tibble::rownames_to_column("Concentrations:") |>
          dplyr::mutate(`Concentrations:` = dplyr::case_when(
            `Concentrations:` == "IncreasingWilliams" ~ "Increasing Williams",
            `Concentrations:` == "DecreasingWilliams" ~ "Decreasing Williams",
            `Concentrations:` == "Dunnett" ~ "Dunnett's two-sided",
            .default = `Concentrations:`
          )) )
  })


  # # Outputs -------------------------------------------
  output$justify <- renderText({
    results()[["Justify"]]
  })
  output$data <- gt::render_gt({
    shiny::validate(shiny::need(!is.null(data()), "No data uploaded yet."))

    result <- data() |>
      dplyr::mutate(Removed =
                      !paste(
                        data()$Replicate,
                        data()$Conc,
                        data()$Fluor)
                    %in%
                      paste(
                        results()[["ProcessedData"]]$Replicate,
                        results()[["ProcessedData"]]$Conc,
                        results()[["ProcessedData"]]$Fluor)
      ) |>
      gt::gt() |>
      gt::tab_header(title= "",
                     subtitle= HTML("<b>Uploaded data</b>. <small> The
          column <b>Removed</b> indicates which measurements
          were removed for further analysis due to potentially required
          trimming or outlier removal for fulfilling the ANOVA
          assumptions of normally distributed residuals and homogeneous
          variances among treatment groups.</small>"))

    result

  })

  # # Return values ----------------------------------------------------------

  output$conclusion_summary <- gt::render_gt({
    validate_results(
      summary()

    )
    summary() |>
      gt::gt() |>
      gt::tab_header(title= " ",subtitle=HTML("<b>Conclusion table summarizing the percent effect per
       Treatment and the results of the post-hoc tests.</b> <small>The table displays the
       % fluorescence induction in respect to the control. The respective
       post-hoc test should be selected based on on the dose-response trend in
       the corresponding data. The monotonicity test, the information about amalgamation of
       treatment means in the Williams test and the summary table of the mixed ANOVA model
       can be a support for deciding about the presence or absence of a dose-response trend.
       (<b>ns</b> not significant,<b>.</b> marginally significant
       ( 0.05 < p > 0.1) ,<b>*</b> significant
       (p &le; 0.05),<b> n.d.</b> not determined)</small>"),
                     tags$style(".gt_column_header { font-weight: bold; }"))
  })
  output$residual_diag_plots <- renderPlot({
    norm <- plot(performance::check_normality(results()[["MixedAnova"]]), type = "density")
    hetero <-  plot(performance::check_heteroscedasticity(results()[["MixedAnova"]]))

    ggpubr::ggarrange(norm, hetero, ncol = 2)
  })


  output$boxplot <- renderPlot({
    validate_results(results())

    results()[["BoxPlots"]]
  })


  output$summary <- gt::render_gt({
    validate_results(results())
    df <- results()[["SummaryDF"]] |>
      dplyr::mutate_if(is.numeric, round, 2) |>
      gt::gt() |>
      gt::tab_header(title= " ",subtitle=HTML("<b>Data summary per treatment group</b>.<small>
       The table displays the number of observations (N), the mean, the standard
       deviation and the Coefficient of variation
       per treatment. Conc refers to the applied
       treatment concentration.</small>)"))

    df
  })


  output$normality <- shiny::renderPrint({
    validate_results(results())

    results()[["NormalityTest"]]

  })

  output$levene <- gt::render_gt({
    validate_results(results())

    df <- results()[["LeveneTest"]] |>
      dplyr::rename(`p-value` = p.value,
                    `Residual df` = df.residual) |>
      gt::gt() |>
      gt::tab_header(title= " ",
                     subtitle = "Levene's test result for for
        variance homogeneity among treatment groups.")  |>
      gt::fmt_number(columns = 1:3, decimals = 2)

    df
  })

  output$monotonicity <- gt::render_gt({
    validate_results(results())

    df <- results()[["Monotonicity Test"]] |>
      dplyr::mutate_if(is.numeric, round, 2) |>
      gt::gt() |>
      gt::tab_header(title= " ", subtitle= HTML("<b>Monotonicity test result
        specifying the significance of linear and quadratic contrasts
        on the rank transformed data</b>. <small>The test results together with
        the information about the amalgamation of treatment means
        available in the increasing and decreasing Williams test results,
        as well as the summary output of the mixed ANOVA model which also
        provides information about linear and quadratic trends in the data,
        can additionally serve as support to decide about the presence and absence
        of a monototonic treatment response reslationship.</small>"))

    df
  })

  output$dunnett <- gt::render_gt({
    validate_results(results())

    df <- results()[["Dunnetts"]] |>
      tibble::rownames_to_column("Contrasts") |>
      dplyr::mutate_if(is.numeric, round, 2) |>
      gt::gt() |>
      gt::tab_header(title=" ", subtitle = HTML("<b>Result table of the dunnett's
       two-sided  many-to-one contrast test result.</b>
       <small>The table displays the estimated difference (<b>Estimate</b>) and
       standard error (<b> SE </b>) between the control and the respective
        treatment group. The Kenward-Rogers degrees of freedom (<b>df</b>),
        the t-value and the corresponding adjusted p-value (<b>p</b>)
        are also displayed.</small>"))

    df
  })

  output$williamsincrease <- gt::render_gt({
    validate_results(results())

    df <- results()[["WilliamsIncrease"]] |>
      dplyr::mutate_if(is.numeric, round, 2) |>
      gt::gt() |>
      gt::tab_header(title=" ", subtitle = HTML("<b>Result table of the increasing Williams trend test</b>.<small>
      <b>Y.Tilde</b>: The amalgamated means for the respective treatment level;  <b>Y0</b>: The control mean;
      <b>DIFF Incr</b>: Difference between Y.Tilde and Y0; <b>SE_DIFF</b>: The standard error of the difference;
      <b>DF</b>: Degrees of freedom; <b>WILL Incr</b>: The value of the increasing Williams test statistic;
      <b>crit Val</b>: The critical value for the increasing Williams test statistic at alpha = 0.05;
      <b>Sign</b>: In case the value of the increasing Williams test statistic is greater than the critical value,
      the result is significant (Sign = TRUE), otherwise not significant (Sign = FALSE); <b>% Incr</b>: The %
      increase in the treatment groups in comparison to the control groups.</small>"))

    df

  })

  output$williamsdecrease <- gt::render_gt({
    validate_results(results())

    df <- results()[["WilliamsDecrease"]] |>
      dplyr::mutate_if(is.numeric, round, 2) |>
      gt::gt() |>
      gt::tab_header(title=" ", subtitle = HTML("<b>Result table of the decreasing Williams trend test</b>.<small>
      <b>Y.Tilde</b>: The amalgamated means for the respective treatment level; <b>Y0</b>: The control mean;
      <b>DIFF Incr</b>: Difference between Y.Tilde and Y0; <b>SE_DIFF</b>: The standard error of the difference;
      <b>DF</b>: Degrees of freedom; <b>WILL Decr</b>: The value of the decreasing Williams test statistic;
      <b>crit Val</b>: The critical value for the decreasing Williams test statistic at alpha = 0.05;
      <b>Sign</b>: In case the value of the decreasing Williams test statistic is greater than the critical value,
      the result is significant (Sign = TRUE), otherwise not significant (Sign = FALSE);<b>% Incr</b>: The %
      increase in the treatment groups in comparison to the control groups. When a decrease is observed
      this value turns negative.</small>"))

    df
  })

  output$mixedmodel<- gt::render_gt({
    validate_results(results())

    df <- results()[["MixedAnova"]] |>
      parameters::model_parameters(ci_method = "kenward") |>
      gt::gt() |>
      gt::tab_header(title=" ", subtitle = HTML("<b>Summary table of the mixed ANOVA model</b>.<small>
       <b>Parameters and Coefficient</b>: The fixed and random model parameters. (Intercept) represents
       in this case the average of all fluorescence measurements.
       Conc.L denotes the linear trend of the data over the increasing treatment groups.
       Conc.Q represents the quadratic trend, which shows whether data follows a curved pattern.
       (When this trend is significant serious doubt exists whether a monotonic treatment response is present.)
       Conc.C represents the cubic trend. A significant cubic trend could indicate, for example, that the response
       variable first increases, then decreases, and then increases again as you move through the ordered levels
       of the factor (When this trend is significant serious doubt exists whether a monotonic treatment response
      is present.). SD(Intercept) denote the estimate of the standard deviation of the respective random term;
      <b>SE</b>: standard error; <b>CI</b> Confidence level; <b>CI_low</b>: lower level
       of the 95% Confidence Interval;<b> CI_high</b>: higher level of the 95% Confidence Interval;
       <b>t</b>: value of the t statistic; <b>df_error</b>: Kenward-Rogers degrees of freedom;
       <b>p</b>: corresponding p-value.</small>")) |>
      gt::fmt_number(columns = 1:10, decimals = 3)

    df
  })
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Report", ".docx", sep = "")
    },
    content = function(file) {
      params <- list(resultList = results(),
                     conclusion_summary = summary(),
                     data = data())
      rmarkdown::render(system.file("R/download_report.rmd", package = "xeredar"),
                        output_file = file,
                        params  = params)

    }
  )
}

div_inline <- function(x) {
  div(style = "display:inline-block", x)
}

validate_results <- function(x) {
  shiny::validate(shiny::need(isTruthy(x), "No results"))
  shiny::validate(shiny::need(!is.character(x), x))
}
