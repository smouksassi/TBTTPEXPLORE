# Module for the plots tab in the TTP app

source("modules/plots_helpers.R")

plots_tab_module_ui <- function(id,
                                ref_drug_list_all_ = ref_drug_list_all,
                                study_list_ = study_list) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        6,
        tags$strong("Reference Treatment"),
        selectInput(
          ns("reference_treat"),
          NULL,
          width = 500,
          choices = c("", ref_drug_list_all_),
          selected = ""
        ),
        bsTooltip(
          ns("reference_treat"),
          "Select a Reference Treatment. To cancel a selection hit Backspace.",
          "top",
          options = list(container = "body")
        ),

        tags$strong("Test Treatment"),
        checkboxInput(ns("upload_custom"),
                      "Upload TTP data file for test treatment",
                      FALSE),
        bsTooltip(
          ns("upload_custom"),
          HTML(
            paste(
              "Check to upload your own data from a .csv file. Refer to the<br>Help ",
              icon("question-circle"),
              " Plots tab for more information about required format and columns names.",
              sep = ""
            )
          ),
          "bottom",
          options = list(container = "body")
        ) ,
        conditionalPanel(
          condition = "input.upload_custom",
          ns = ns,
          fileInput(ns("custom_file"), NULL),
          shinyjs::hidden(div(id = ns("upload_error")))
        ),

        selectInput(
          ns("test_treat"),
          NULL,
          width = 500,
          choices = c("", ref_drug_list_all_),
          selected = ""
        ) ,
        bsTooltip(
          ns("test_treat"),
          "Select a Test Treatment. To cancel a selection hit Backspace.",
          "bottom",
          options = list(container = "body")
        )

      ),
      column(
        6,
        tags$strong("Subset by study (optional)"),
        selectInput(
          ns("subset_by_study"),
          NULL,
          width = 500,
          choices = c("", study_list_),
          selected = ""
        ),
        bsTooltip(
          ns("subset_by_study"),
          "When a study is selected the Reference Treatment and Test Treatment dropdowns will be automaticallly updated to those specific to the study. To cancel a selection hit Backspace.",
          "bottom",
          options = list(container = "body")
        )

      )
    ),
    fluidRow(
      column(
        4,
        checkboxInput(ns("opt_overlay"), "Overlay data", value = FALSE),
        bsTooltip(
          ns("opt_overlay"),
          "Check to plot both treatments side by side",
          "top",
          options = list(container = "body")
        ),

        checkboxInput(
          ns("opt_timedays"),
          "Time in days (default: weeks)",
          value = FALSE,
          width = 300
        ),
        bsTooltip(
          ns("opt_timedays"),
          "Check to transform time from days into weeks",
          "top",
          options = list(container = "body")
        )
      ),
      column(
        4,
        checkboxInput(ns("opt_median"), "Show median value", value = FALSE),
        bsTooltip(
          ns("opt_median"),
          "Check to show the numeric median value",
          "top",
          options = list(container = "body")
        ),

        checkboxInput(
          ns("opt_pred"),
          "Overlay Model Pred",
          value = FALSE,
          width = 300
        ),
        bsTooltip(
          ns("opt_pred"),
          "Check to add the Model Population Predictions if the chosen Treatment(s) were modeled",
          "top",
          options = list(container = "body")
        )
      ),
      column(
        4,
        checkboxInput(ns("opt_samplesize"), "Show sample size", value = FALSE),
        bsTooltip(
          ns("opt_samplesize"),
          "Check to show the Number of patients",
          "top",
          options = list(container = "body")
        ),
        checkboxInput(
          ns("exclude42"),
          "Exclude Censored TTP from observed",
          value = FALSE,
          width = 300
        ),

        bsTooltip(
          ns("exclude42"),
          HTML(
            "Check to exclude <em>(censored)</em> TTP &ge; 42. This is useful to visualize the effect of missingness and to render the visual comparison between Model Pred and observed data Medians possible."
          ),
          "top",
          options = list(container = "body")
        )
      )
    ),
    fluidRow(
      column(12,
             plotOutput(ns("tpp_boxplot"), width = "100%", height = "500px"))
    )
  )
}

plots_tab_module <- function(input, output, session,
                             ttp_data_ = ttp_data) {
  ref_data <- reactive({
    # If a specific study is requested, subset by study
    if (input$subset_by_study != "") {
      validate(need(
        input$subset_by_study %in% ttp_data_$STUDY,
        "Study not in dataset"
      ))
      subset(ttp_data_, STUDY == input$subset_by_study)
    } else {
      ttp_data_
    }
  })

  test_data <- reactive({
    if (using_custom_file()) {
      custom_data()
    } else {
      ref_data()
    }
  })

  ref_drug_list <- reactive({
    sort(as.character(unique(ref_data()$TRTDOSE)))
  })

  test_drug_list <- reactive({
    sort(as.character(unique(test_data()$TRTDOSE)))
  })

  # Make sure the reference and test treatment dropdowns don't let you select
  # the same drug
  observeEvent(input$reference_treat, {
    if (using_custom_file())
      return()
    updateSelectInput(
      session,
      "test_treat",
      choices = c("", setdiff(
        ref_drug_list(), input$reference_treat
      )),
      selected = input$test_treat
    )
  })
  observeEvent(input$test_treat, {
    if (using_custom_file()) return()
    updateSelectInput(
      session,
      "reference_treat",
      choices = c("", setdiff(ref_drug_list(), input$test_treat)),
      selected = input$reference_treat
    )
  })
  observeEvent(input$subset_by_study, {
    updateSelectInput(
      session,
      "reference_treat",
      choices = c("", setdiff(ref_drug_list(), input$test_treat)),
      selected = ""
    )
    if (!using_custom_file()) {
      updateSelectInput(session,
                        "test_treat",
                        choices = c("", setdiff(
                          ref_drug_list(), input$reference_treat
                        )),
                        selected = "")
    }
  })

  # Create the main plot
  output$tpp_boxplot <- renderPlot({
    # abort if no treatments are chosen
    if (input$reference_treat == "" || input$test_treat == "") {
      return()
    }

    # abort if the user wants to upload a file but hasn't uploaded one yet
    if (!validate_data_input()) {
      return()
    }

    # abort if the selected study doesn't exist in the data (this can
    # happen if this function is called too fast because the dropdown updates)
    if (!(input$subset_by_study %in% c("", unique(ref_data()$STUDY)))) {
      return()
    }

    # abort if the selected treatments don't exist in the data (this can
    # happen if this function is called too fast because the dropdown updates)
    if (!(input$reference_treat %in% unique(ref_data()$TRTDOSE))) {
      return()
    }
    if (!(input$test_treat %in% unique(test_data()$TRTDOSE))) {
      return()
    }

    # Get data only for the two chosen drugs, and make sure the reference
    # comes before the treatment so that in the box plot it'll be on the left
    # (it will happen because "R" comes before "T")
    ref_rows <- subset(ref_data(), TRTDOSE == input$reference_treat)
    ref_rows <-
      ref_rows[, c("DAY_BIN", "WEEK_BIN", "TTP", "TRTDOSE", "STUDY", "PRED")]
    ref_rows$TRTDOSE_NAME <-
      paste0("Reference: ", input$reference_treat)
    test_rows <- subset(test_data(), TRTDOSE == input$test_treat)
    # If using a custom file, there may not be any study information
    if (is.null(test_rows$STUDY)) {
      test_rows$STUDY <- "Unspecified"
    }
    test_rows <-
      test_rows[, c("DAY_BIN", "WEEK_BIN", "TTP", "TRTDOSE", "STUDY", "PRED")]
    test_rows$TRTDOSE_NAME <- paste0("Test: ", input$test_treat)

    # If using a custom TTP file, ensure the last bin category is the same
    # for both datasets
    if (using_custom_file()) {
      levels(ref_rows$WEEK_BIN)[nlevels(ref_rows$WEEK_BIN)] <-
        levels(test_rows$WEEK_BIN)[nlevels(test_rows$WEEK_BIN)]
      levels(ref_rows$DAY_BIN)[nlevels(ref_rows$DAY_BIN)] <-
        levels(test_rows$DAY_BIN)[nlevels(test_rows$DAY_BIN)]
    }

    data <- rbind(ref_rows, test_rows)

    if (input$opt_timedays) {
      data$TIME_BIN <- data$DAY_BIN
      xlab <- "Time (Days)"
    } else {
      data$TIME_BIN <- data$WEEK_BIN
      xlab <- "Time (Weeks)"
    }
    data$STUDY_NAME <- paste0("Study: ", data$STUDY)

    # And finally - plot!
    if (input$exclude42) {
      data <- data[data$TTP < 42, ]
    }
    plot <- ggplot(data, aes(TIME_BIN, TTP)) +
      aes(color = TRTDOSE, group = TRTDOSE) +
      geom_point(alpha = 0.5,
                 shape = 16,
                 size = 1) +
      geom_boxplot(aes(group = NULL), varwidth = FALSE,
                   notch = FALSE) +
      stat_summary(
        aes(
          y = TTP,
          x = TIME_BIN,
          group = TRTDOSE,
          linetype = "Median (Obs)",
          color = TRTDOSE
        ),
        fun = median,
        geom = "line",
        inherit.aes = FALSE,
        size = 1.5
      ) +
      xlab(xlab) +
      ylab("TTP (Days)") +
      theme_bw(base_size = 16) +
      theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.direction = "horizontal",
        axis.text.x = ggplot2::element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5
        ),
        legend.title = element_blank()
      )

    # Customize the plot based on user options
    if (!input$opt_overlay) {
      if (input$subset_by_study != "") {
        plot <- plot +
          facet_grid(. ~ STUDY_NAME + TRTDOSE_NAME)
      } else {
        plot <- plot +
          facet_grid(. ~ TRTDOSE_NAME)
      }
    } else {
      if (input$subset_by_study != "") {
        plot <- plot +
          facet_grid(. ~ STUDY_NAME)
      }
    }
    if (input$opt_samplesize) {
      give.n <- function(x) {
        return(c(y = min(x) * 1.05,  label = length(x)))
      }
      plot <- plot +
        stat_summary(
          fun.data = give.n,
          aes(group = NULL),
          geom = "label",
          alpha = 0.1,
          fun = median,
          fontface = "bold",
          fill = "white",
          show.legend = FALSE,
          size = 6,
          position = position_dodge(width = 0.8)
        )
    }
    if (input$opt_median) {
      median.n <- function(x) {
        return(c(
          y = ifelse(median(x) < 0, median(x), median(x)),
          label = round(median(x), 1)
        ))
      }
      plot <- plot +
        stat_summary(
          fun.data = median.n,
          aes(group = NULL),
          geom = "label",
          alpha = 0.1,
          fun.y = median,
          fontface = "bold",
          fill = "white",
          show.legend = FALSE,
          size = 6,
          position = position_dodge(width = 0.8)
        )
    }

    if (input$opt_pred) {
      plot <- plot +
        stat_summary(
          aes(
            y = PRED,
            x = TIME_BIN,
            group = TRTDOSE,
            linetype = "PRED",
            color = TRTDOSE
          ),
          fun.y = median,
          geom = "line",
          inherit.aes = FALSE,
          size = 1.5
        )
    }

    plot +
      guides(linetype = guide_legend(order = 2))
  })

  # Read the custom TTP data file
  custom_data <- reactive({
    if (is.null(input$custom_file)) {
      return()
    }

    shinyjs::hide("upload_error")

    data <- try(read.csv(
      input$custom_file$datapath,
      na.strings = c("", " ", ".", "NA", "na"),
      stringsAsFactors = FALSE
    ),
    silent = TRUE)
    if (inherits(data, "try-error")) {
      shinyjs::html("upload_error", "Could not read the file")
      shinyjs::show("upload_error")
      return()
    }

    validate_res <- validate_dataset(data)
    if (!isTRUE(validate_res)) {
      shinyjs::html("upload_error",
                    paste0("There is a problem with the file: ", validate_res))
      shinyjs::show("upload_error")
      return()
    }
    data <- clean_dataset_weeks(data)
    if (is.null(data$PRED)) {
      data$PRED <- NA
    }
    data
  })

  # Whether or not the user uploaded a custom TTP file for test treatment
  using_custom_file <- reactive({
    input$upload_custom == TRUE &&
      !is.null(input$custom_file) &&
      !is.null(custom_data())
  })

  # When a custom file is uploaded, change the available drug dropdowns
  observeEvent(using_custom_file(), {
    if (!using_custom_file()) {
      updateSelectInput(
        session,
        "reference_treat",
        choices = c("", setdiff(ref_drug_list(), input$test_treat)),
        selected = input$reference_treat
      )
      updateSelectInput(
        session,
        "test_treat",
        choices = c("", setdiff(
          ref_drug_list(), input$reference_treat
        )),
        selected = input$test_treat
      )
      return()
    } else {
      updateSelectInput(
        session,
        "reference_treat",
        choices = c("", ref_drug_list()),
        selected = input$reference_treat
      )

      test_drugs <- as.character(unique(custom_data()$TRTDOSE))
      updateSelectInput(
        session,
        "test_treat",
        choices = c("", test_drugs),
        selected = input$test_treat
      )
    }
  })

  # Determine if there is a valid dataset for the test treatment
  validate_data_input <- reactive({
    !input$upload_custom || !is.null(custom_data())
  })

  # Don't show the test treatment dropdown if no valid file is uploaded
  observe({
    shinyjs::toggle(id = "test_treat", condition = validate_data_input())
  })
}
