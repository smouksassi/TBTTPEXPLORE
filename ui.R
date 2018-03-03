tagList(
  shinyjs::useShinyjs(),

  tags$head(tags$link(href = "app.css", rel = "stylesheet")),

  navbarPage(
    title = "Plot and Compare TTP",
    id = "mainNav",
    inverse = TRUE,
    fluid = FALSE,
    collapsible = TRUE,

    tabPanel(
      "TB Study Description",
      value = "TBStudyDescription",
      icon = icon("info"),
      class = "fade in",

      includeHTML(file.path("text", "TBStudyDescription.html"))
    ),

    tabPanel(
      "How to",
      value = "howto",
      icon = icon("info"),
      class = "fade in",

      includeHTML(file.path("text", "howto.html"))
    ),

    tabPanel(
      "Plots",
      value = "plots",
      icon = icon("bar-chart"),
      class = "fade in",

      fluidPage(
        column(
          6,
          tags$strong("Reference Treatment"),
          selectInput(
            "reference_treat",
            NULL,
            width = 500,
            choices = c("", ref_drug_list_all),
            selected = ""
          ),
          bsTooltip(
            "reference_treat",
            "Select a Reference Treatment. To cancel a selection hit Backspace.",
            "top",
            options = list(container = "body")
          ) ,

          tags$strong("Test Treatment"),
          checkboxInput("upload_custom", "Upload TTP data file for test treatment",
                        FALSE),
          bsTooltip(
            "upload_custom",
            HTML(
              paste(
                "Check to upload your own data from a .csv file. Refer to the<br>How to ",
                icon("info"),
                " tab for more information about required format and columns names.",
                sep = ""
              )
            ),
            "bottom",
            options = list(container = "body")
          ) ,
          conditionalPanel(
            "input.upload_custom",
            fileInput("custom_file", NULL),
            shinyjs::hidden(div(id = "upload_error"))
          ),

          selectInput(
            "test_treat",
            NULL,
            width = 500,
            choices = c("", ref_drug_list_all),
            selected = ""
          ) ,
          bsTooltip(
            "test_treat",
            "Select a Test Treatment. To cancel a selection hit Backspace.",
            "right",
            options = list(container = "body")
          )

        ),
        column(
          6,
          tags$strong("Subset by study (optional)"),
          selectInput(
            "subset_by_study",
            NULL,
            width = 500,
            choices = c("", study_list),
            selected = ""
          )

          ,
          bsTooltip(
            "subset_by_study",
            "When a study is selected the Reference Treatment and Test Treatment dropdowns will be automaticallly updated to those specific to the study. To cancel a selection hit Backspace.",
            "bottom",
            options = list(container = "body")
          )

        )
      ),
      div(
        id = "plot-main-opts",
        checkboxInput("opt_overlay", "Overlay data", value = FALSE),

        bsTooltip(
          "opt_overlay",
          "Check to plot both treatments side by side",
          "left",
          options = list(container = "body")
        ),

        checkboxInput("opt_median", "Show median value", value = FALSE),

        bsTooltip(
          "opt_median",
          "Check to show the numeric median value",
          "top",
          options = list(container = "body")
        ) ,

        checkboxInput("opt_samplesize", "Show sample size", value = FALSE),
        bsTooltip(
          "opt_samplesize",
          "Check to show the Number of patients",
          "top",
          options = list(container = "body")
        ) ,

        checkboxInput(
          "opt_timedays",
          "Time in days (default: weeks)",
          value = FALSE,
          width = 300
        ),
        bsTooltip(
          "opt_timedays",
          "Check to transform time from days into weeks",
          "right",
          options = list(container = "body")
        ) ,
        checkboxInput(
          "opt_pred",
          "Overlay Model Pred",
          value = FALSE,
          width = 300
        ) ,

        bsTooltip(
          "opt_pred",
          "Check to add the Model Population Predictions if the chosen Treatment(s) were modeled",
          "left",
          options = list(container = "body")
        ) ,



        checkboxInput(
          "exclude42",
          "Exclude Censored TTP from observed",
          value = FALSE,
          width = 300
        ) ,

        bsTooltip(
          "exclude42",
          HTML(
            "Check to exclude <em>(censored)</em> TTP &ge; 42. This is useful to visualize the effect of missingness and to render the visual comparison between Model Pred and observed data Medians possible."
          ),
          "right",
          options = list(container = "body")
        )


      ),
      plotOutput("tpp_boxplot", width = "100%", height = "500px")
    ),
    #tabPanelPlots
    tabPanel(
      "Gompertz Model",
      value = "plots",
      icon = icon("line-chart"),
      class = "fade in",

      fluidPage(
        withMathJax(),
        column(
          12,
          bsButton("helpmodal", "Model Details and how to use this Page", style =
                     "info")

        ),
        column(
          12,
          column(8,
                 plotOutput(outputId = "gompertzcurve"),
                 hr()),


          column(
            4,
            radioButtons(
              "trtcov",
              label = "Treatments",
              choices = c(
                "TMC207",
                "Placebo (Background Regimen)",
                "MICRONUTRIENT/RHZE",
                "PA-824/PZ/M",
                "PHZE",
                "MRZE",
                "RHZE",
                "User Sim"
              ),
              inline = FALSE,
              selected = "RHZE"
            ),
            radioTooltip(
              "trtcov",
              choice = "Placebo (Background Regimen)",
              title = "Placebo (Background regimen) consisted of preferred five-drug, second-line anti-TB background regimen (e.g., aminoglycosides, fluoroquinolones, ethionamide or protionamide, pyrazinamide, ethambutol, cycloserine or terizidone)",
              placement = "right",
              options = list(container = "body")
            ),
            radioTooltip(
              "trtcov",
              choice = "User Sim",
              title = "User can specify Baseline TTP and custom Treatment effects on various parameters",
              placement = "right",
              options = list(container = "body")
            ),

            selectInput(
              "trtcovbackground",
              label = "Compare to:",
              choices = c(
                "TMC207",
                "Placebo (Background Regimen)",
                "MICRONUTRIENT/RHZE",
                "PA-824/PZ/M",
                "PHZE",
                "MRZE",
                "RHZE"
              ),
              selected = "Placebo (Background Regimen)"
            )

          ),
          #column4
          column(
            12,
            conditionalPanel(
              "input.trtcov == 'User Sim' ",

              column(
                3,
                sliderInput(
                  "offsettrtslider",
                  label = "TRT on Offset",
                  min = 1 / 10,
                  max = 10,
                  value = 1,
                  step = 0.005
                )
              ),
              column(
                3,
                sliderInput(
                  "alphatrtslider",
                  label = "TRT on Alpha",
                  min = 1 / 10,
                  max = 10,
                  value = 1,
                  step = 0.005
                )
              ),
              column(
                3,
                sliderInput(
                  "betatrtslider",
                  label = "TRT on Beta",
                  min = 1 / 10,
                  max = 10,
                  value = 1,
                  step = 0.005
                )
              ),
              column(
                3,
                sliderInput(
                  "gammatrtslider",
                  label = "TRT on Gamma",
                  min = 1 / 10,
                  max = 10,
                  value = 1,
                  step = 0.005
                )
              ),
              column(
                4,
                sliderInput(
                  "baselinettpslider",
                  label = "Baseline TTP(days)",
                  min = 1,
                  max = 42,
                  value = 6.50,
                  step = 0.001
                )
              ),
              column(
                4,
                selectInput(
                  "slidersinitials",
                  label = "Set Simulation Sliders to:",
                  choices = c(
                    "",
                    "TMC207",
                    "Placebo (Background Regimen)",
                    "MICRONUTRIENT/RHZE",
                    "PA-824/PZ/M",
                    "PHZE",
                    "MRZE",
                    "RHZE"
                  ),
                  selected = ""
                )
              ),
              column(
                4,
                checkboxInput("changepopparams", "Change Population Values TTP?", FALSE)
              )

            ),
            conditionalPanel(
              "input.changepopparams && input.trtcov == 'User Sim' ",
              column(
                12,
                column(
                  3,
                  sliderInput(
                    "offsetslider",
                    label = "Population Offset",
                    min = 4.33843 / 10,
                    max = 4.33843 * 10,
                    value = 4.33843,
                    step = 0.005
                  )
                ),
                column(
                  3,
                  sliderInput(
                    "alphaslider",
                    label = "Population Alpha",
                    min = 22.4067 / 10,
                    max = 22.4067 * 10,
                    value = 22.4067,
                    step = 0.002
                  )
                ),
                column(
                  3,
                  sliderInput(
                    "betaslider",
                    label = "Population Beta",
                    min = 2.22748 / 10,
                    max = 2.22748 * 10,
                    value = 2.22748,
                    step = 0.001
                  )
                ),
                column(
                  3,
                  sliderInput(
                    "gammaslider",
                    label = "Population Gamma",
                    min = 0.049461 / 10,
                    max = 10 * 0.049461,
                    value = 0.049461,
                    step = 0.001
                  )
                ),
                actionButton(
                  "resetpop",
                  "Reset Baseline TTP and  Population Parameters to Initial Values"
                )
              )
            )
          ),



          column(
            12,
            tags$strong("Primary Curve Parameters"),
            tableOutput('table1')
          )	,
          column(
            12,
            tags$strong("Secondary Curve Parameters"),
            tableOutput('table2')
          )
        )#column
      )#fluidpage
    )#tabpanel Gompertz Model
  )#navbarpage
)
