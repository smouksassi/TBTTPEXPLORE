# Module for the gompertz model tab in the TTP app

source("modules/gompertz_helpers.R")

gompertz_tab_module_ui <- function(id,
                                   tvOffset_ = tvOffsetDefault,
                                   tvAlpha_ = tvAlphaDefault,
                                   tvBeta_ = tvBetaDefault,
                                   tvGamma_ = tvGammaDefault,
                                   treatments_default_ = treatments_default,
                                   treatment_descriptions_ = treatment_descriptions) {
  ns <- NS(id)

  tagList(
    shinyalert::useShinyalert(),
    fluidRow(
      column(
        8,
        plotOutput(outputId = ns("gompertzcurve"))
      ),
      column(
        4,
        selectInput(
          ns("trtcov"),
          label = "Treatments",
          choices = list(
            "Default Treatments" = treatments_default_,
            "Live Simulation" = list("Live Simulation")
          ),
          selected = "RHZE"
        ),
        lapply(
          names(treatment_descriptions_),
          function(treatment) {
            conditionalPanel(
              condition = paste0("input.trtcov == '", treatment, "'"),
              ns = ns,
              helpText(treatment_descriptions_[[treatment]])
            )
          }
        ),
        selectInput(
          ns("trtcovbackground"),
          label = "Compare to:",
          choices = list(
            "Default Treatments" = treatments_default_
          ),
          selected = "Placebo (Background Regimen)"
        ),
        lapply(
          names(treatment_descriptions_),
          function(treatment) {
            conditionalPanel(
              condition = paste0("input.trtcovbackground == '", treatment, "'"),
              ns = ns,
              helpText(treatment_descriptions_[[treatment]])
            )
          }
        )
      )
    ),
    fluidRow(
      column(12, hr())
    ),
    fluidRow(
      column(
        12,
        h2("Custom Simulation")
      )
    ),
    fluidRow(
      column(
        3,
        sliderInput(
          ns("offsettrtslider"),
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
          ns("alphatrtslider"),
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
          ns("betatrtslider"),
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
          ns("gammatrtslider"),
          label = "TRT on Gamma",
          min = 1 / 10,
          max = 10,
          value = 1,
          step = 0.005
        )
      )
    ),
    fluidRow(
      column(
        4,
        sliderInput(
          ns("baselinettpslider"),
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
          ns("slidersinitials"),
          label = "Set Simulation Sliders to:",
          choices = c("", treatments_default_),
          selected = ""
        )
      ),
      column(
        4,
        checkboxInput(ns("changepopparams"),
                      "Change Population Values TTP?", FALSE)
      )
    ),
    conditionalPanel(
      condition = "input.changepopparams",
      ns = ns,
      fluidRow(
        id = ns("popparams-inputs"),
        column(
          3,
          sliderInput(
            ns("offsetslider"),
            label = "Population Offset",
            min = tvOffset_ / 10,
            max = tvOffset_ * 10,
            value = tvOffset_,
            step = 0.005
          )
        ),
        column(
          3,
          sliderInput(
            ns("alphaslider"),
            label = "Population Alpha",
            min = tvAlpha_ / 10,
            max = tvAlpha_ * 10,
            value = tvAlpha_,
            step = 0.002
          )
        ),
        column(
          3,
          sliderInput(
            ns("betaslider"),
            label = "Population Beta",
            min = tvBeta_ / 10,
            max = tvBeta_ * 10,
            value = tvBeta_,
            step = 0.001
          )
        ),
        column(
          3,
          sliderInput(
            ns("gammaslider"),
            label = "Population Gamma",
            min = tvGamma_ / 10,
            max = 10 * tvGamma_,
            value = tvGamma_,
            step = 0.001
          )
        )
      ),
      fluidRow(
        column(
          12,
          actionButton(
            ns("resetpop"),
            "Reset Baseline TTP and Population Parameters to Initial Values"
          ),
          br(), br()
        )
      )
    ),
    fluidRow(
      column(
        12,
        inlineInput(textInput(
          ns("custom_sim_name"),
          label = NULL,
          placeholder = "Custom simulation name"
        )),
        actionButton(
          ns("create_custom_sim"),
          "Create Custom Simulation",
          class = "btn-primary"
        ),
        actionButton(
          ns("clear_custom_sims"),
          "Clear Saved Simulations"
        )
      ),
      column(
        12,
        div(
          id = ns("name_exists_warning"),
          class = "alert alert-warning",
          textOutput(ns("existing_name"), inline = TRUE),
          " already exists as a default treatment name.  Please choose another name."
        )
      )
    ),
    fluidRow(
      column(12, hr())
    ),
    fluidRow(
      column(
        12,
        tags$strong("Primary Curve Parameters"),
        tableOutput(ns('table1'))
      )
    ),
    fluidRow(
      column(
        12,
        tags$strong("Secondary Curve Parameters"),
        tableOutput(ns('table2'))
      )
    ),
    fluidRow(
      column(
        12,
        h3("Download data"),
        radioButtons(ns("download_size"), NULL,
                     choices = c("Condensed data" = "short", "Full data" = "full")),
        downloadButton(ns("download_btn"), "Download")
      )
    )
  )
}

gompertz_tab_module <- function(input, output, session,
                                BASELINETTP_ = BASELINETTPDefault,
                                treatments_default_ = treatments_default) {


  custom_sims <- reactiveVal(list())

  live_sim <- reactive({
    sim_inputs <- list(
      tvOffset = input$offsetslider,
      tvAlpha = input$alphaslider,
      tvBeta = input$betaslider,
      tvGamma = input$gammaslider,
      BASELINETTP = input$baselinettpslider,
      TRT = input$custom_sim_name,
      dE0dTRTSIM = input$offsettrtslider,
      dAlphadTRTSIM = input$alphatrtslider,
      dBetadTRTSIM = input$betatrtslider,
      dGamdTRTSIM = input$gammatrtslider
    )

    modifyList(gompertz_default_args, sim_inputs)
  })

  # collect input values for custom simulation into list
  custom_sim_inputs <- reactive({
    req(notempty(input$custom_sim_name))

    live_sim()
  })

  # Do not allow empty custom simulation name or a name that exists as a default
  # treatment
  observeEvent(input$custom_sim_name, {
    custom_sim_name <- input[["custom_sim_name"]]

    name_is_new <- !(custom_sim_name %in% treatments_default_)
    valid_name <- notempty(custom_sim_name) && name_is_new
    shinyjs::toggleState(id = "create_custom_sim", condition = valid_name)
    shinyjs::toggle(id = "name_exists_warning", condition = !name_is_new)
  })

  output$existing_name <- renderText({
    input[["custom_sim_name"]]
  })

  observeEvent(custom_sims(), {
    cond <- length(custom_sims()) > 0

    shinyjs::toggle(
      id = "clear_custom_sims",
      condition = cond
    )
  })

  observeEvent(input$clear_custom_sims, {
    custom_sims(list())

    shinyalert::shinyalert(
      title = "All saved custom simulations were removed",
      type = "success",
      closeOnClickOutside = TRUE
    )
  })

  # store custom simulation inputs in reactiveVal
  observeEvent(input$create_custom_sim, {
    sims <- custom_sims()

    # append new custom sim to `custom_sims` reactive
    custom_inputs <- custom_sim_inputs()
    custom_name <- custom_inputs[['TRT']]
    sims[[custom_name]] <- custom_inputs

    custom_sims(sims)

    shinyalert::shinyalert(
      title = paste0('Custom Simulation for treatment "', custom_name, '" created'),
      type = "success",
      closeOnClickOutside = TRUE
    )
  })

  treatment_choices <- reactive({
    list(
      defaults = treatments_default_,
      custom = names(custom_sims())
    )
  })



  # Update the "Treatments" dropdown to include custom parameter sets
  observeEvent(treatment_choices(), {
    choices <- treatment_choices()

    if (input[["trtcov"]] == "Live Simulation") {
      new_choice <- input$trtcov
    } else if (input[["trtcov"]] %in% unlist(choices)) {
      new_choice <- input$trtcov
    } else {
      new_choice <- choices[["defaults"]][1]
    }

    choices_out <- list(
      "Default Treatments" = choices[["defaults"]],
      "Live Simulation" = list("Live Simulation")
    )

    if (length(choices[["custom"]]) > 0) {
      choices_out <- c(choices_out, list("Custom Simulations" = as.list(choices[["custom"]])))
    }

    updateSelectInput(
      session,
      "trtcov",
      choices = choices_out,
      selected = new_choice
    )
  })

  # Update the "compare to" dropdown so it doesn't have the Treatments variable
  observeEvent(list(input[["trtcov"]], treatment_choices()), {
    choices <- treatment_choices()

    choices[["defaults"]] <- setdiff(choices[["defaults"]], input[["trtcov"]])
    choices[["custom"]] <- setdiff(choices[["custom"]], input[["trtcov"]])
    if (input[["trtcovbackground"]] %in% unlist(choices)) {
      new_choice <- input[["trtcovbackground"]]
    } else {
      new_choice <- choices[["defaults"]][1]
    }
    if (length(choices) == 2) {
      choices[["custom"]] <- as.list(choices[["custom"]])
      names(choices) <- c("Default Treatments", "Custom Simulations")
    } else if (length(choices) == 1) {
      names(choices) <- "Default Treatements"
    }

    updateSelectInput(session, "trtcovbackground",
                      choices = choices, selected = new_choice)
  })

  # prep sim names for all simulations "Treatments" and "Compare to" that will be
  # shown in table and download output
  other_sim_names <- reactive({
    custom_sim_names <- names(custom_sims())
    sel_sim_names <- c(input[["trtcov"]], input[["trtcovbackground"]])


    # remove custom sim name if it is selected in input$trtcov and/or
    # input$trtcovbackground are custom sims names
    setdiff(custom_sim_names, sel_sim_names)
  })


  plotdata_func <- function(trtcov) {

    if (trtcov == "Live Simulation") {
      model_params <- live_sim()
      model_params[["TRT"]] <- "Live Simulation"
    } else if (trtcov %in% treatments_default_) {
      model_params <- list(TRT = trtcov)
    } else {
      req(trtcov %in% names(custom_sims()))
      model_params <- custom_sims()[[trtcov]]
    }

    plotdata <- do.call(makegompertzModelCurve, args = model_params)

    plotdata
  }

  refcurve <- reactive({
    plotdata_func(input[["trtcov"]])
  })

  comparetourve <- reactive({
    plotdata_func(input$trtcovbackground)
  })

  # any custom simulation parameter sets that are not in "Treatment" or "Compare to"
  other_curves <- reactive({
    custom_sims <- custom_sims()

    lapply(other_sim_names(), function(nm) plotdata_func(nm))
  })

  output$gompertzcurve <- renderPlot({
    plotdata <- refcurve()
    plotdata$TRT <- as.factor(plotdata$TRT)
    gompertzdataparams <- plotdata[1, ]
    backdata <- comparetourve()
    backdata$TRT <- as.factor(backdata$TRT)
    backdataparams <- backdata[1, ]
    alldata <- rbind(backdata, plotdata)
    alldataparams <- rbind(backdataparams, gompertzdataparams)
    alldataparamsrepel1 <-
      alldataparams[, c("TRT", "TIMEEFFMAX50", "EEFFMAX50", "TTP0", "TTPINF")]
    alldataparamsrepel1$parameter <- "Eff(50)"
    alldataparamsrepel2 <-
      alldataparams[, c("TRT", "TIMETTPMAX50", "TTPMAX50", "TTP0", "TTPINF")]
    alldataparamsrepel2$parameter <- "TTP(50)"
    names(alldataparamsrepel1) <-
      names(alldataparamsrepel2)  <-
      c("TRT", "TIME", "PARAM", "TTP0", "TTPINF", "parameter")
    alldataparamsrepel <-
      rbind(alldataparamsrepel1, alldataparamsrepel2)
    alldataparamsrepelinf <-
      alldataparamsrepel[!duplicated(alldataparamsrepel$TRT), ]
    p <-  ggplot(alldata, aes(Time, TTPPRED))
    p <- p +
      geom_hline(data = alldataparamsrepelinf,
                 aes(yintercept = TTP0, linetype = TRT),
                 show.legend = FALSE) +
      geom_hline(data = alldataparamsrepelinf,
                 aes(yintercept = TTPINF, linetype = TRT),
                 show.legend = FALSE) +
      geom_segment(
        data = alldataparams,
        aes(
          x = TIMETTPMAX50,
          y = TTP0,
          linetype = TRT,
          xend = TIMETTPMAX50,
          yend = TTPMAX50
        ),
        show.legend = FALSE
      ) +
      geom_segment(
        data = alldataparams,
        aes(
          x = TIMEEFFMAX50,
          y = TTP0,
          linetype = TRT,
          xend = TIMEEFFMAX50,
          yend = EEFFMAX50
        ),
        show.legend = FALSE
      ) +
      geom_segment(
        data = alldataparams,
        aes(
          x = Time,
          y = TTPMAX50,
          linetype = TRT,
          xend = TIMETTPMAX50,
          yend = TTPMAX50
        ),
        show.legend = FALSE
      ) +
      geom_segment(
        data = alldataparams,
        aes(
          x = Time,
          y = EEFFMAX50,
          linetype = TRT,
          xend = TIMEEFFMAX50,
          yend = EEFFMAX50
        ),
        show.legend = FALSE
      ) +
      geom_line(aes(linetype = TRT), size = 1.5, alpha = 0.5) +
      geom_point(
        data = alldataparamsrepel,
        aes(
          x = TIME,
          y = PARAM,
          shape = parameter,
          fill = TRT
        ),
        size = 5,
        alpha = 0.8
      ) +
      geom_label_repel(
        data = alldataparamsrepel,
        aes(
          x = 0,
          y = PARAM,
          fill = TRT,
          label = paste(round(PARAM, 1))
        ),
        direction = "y",
        alpha = 0.6,
        show.legend = FALSE
      ) +

      geom_label_repel(
        data = alldataparamsrepel,
        aes(
          x = TIME,
          y = TTP0 * 0.9,
          fill = TRT,
          label = paste(round(TIME, 1))
        ),
        direction = "both",
        alpha = 0.6,
        show.legend = FALSE
      ) +

      geom_label_repel(data = alldataparamsrepelinf, aes(
        x = +Inf,
        y = TTP0 * 1.05,
        fill = TRT,
        label = paste("TTP(0) =", round(TTP0, 1))
      )) +
      geom_label_repel(data = alldataparamsrepelinf, aes(
        x = +Inf,
        y = TTPINF * 1.05,
        fill = TRT,
        label = paste("TTP(\u221E) =", round(TTPINF, 1))
      )) +


      labs(
        y = "TTP (t), Days",
        x = "Days Post Start of Treatment",
        linetype = "",
        colour = "",
        shape = ""
      ) +
      scale_shape_manual(values = c(23, 24),
                         labels = c(expression(Eff[50]), expression(TTP[50]))) +
      scale_fill_manual(values = c("darkgray", "white")) +

      scale_x_continuous(breaks = c(0, 30, 60, 90, 120)) +
      theme_bw(base_size = 20) +
      guides(
        linetype = guide_legend(order = 1),
        shape = guide_legend(order = 2, reverse = TRUE),
        fill = FALSE
      ) +
      coord_cartesian(xlim = c(0, 120)) +
      theme(
        legend.key.width  = unit(2, "cm"),
        legend.position = "bottom",
        legend.box = "vertical"
      )

    print(p)
  })

  cols_table_1 <- c("TRT", "BASELINETTP", "Offset", "Alpha", "Beta", "Gamma")
  cols_table_2 <- c("TRT", "TTP0", "TTPMAX50", "EEFFMAX50", "TTPINF", "DeltaTTP",
                    "TIMETTPMAX50", "TIMEEFFMAX50", "TTPAUC1", "TTPAUC3")

  table_subset <- function(cols_to_keep = c(), first_row_only = TRUE) {
    # merge curves into 1 list
    curves <- c(
      list(
        "refcurve" = refcurve(),
        "comparetourve" = comparetourve()
      ),
      other_curves()
    )

    if (first_row_only) {
      curves <- lapply(curves, function(curve) {
        curve[1, ]
      })
    }
    df <- dplyr::bind_rows(curves)
    if (length(cols_to_keep) > 0) {
      df <- df[, cols_to_keep]
    }
    df
  }

  output$table1 <- renderTable({
    table_subset(cols_table_1)
  }, include.rownames = FALSE)

  output$table2 <- renderTable({
    table_subset(cols_table_2)
  }, include.rownames = FALSE)


  observeEvent(input$resetpop, ignoreNULL = FALSE, {
    shinyjs::reset("popparams-inputs")
    shinyjs::reset("baselinettpslider")
  })

  observe({
    trtcov <- input$slidersinitials
    if (trtcov != "") {
      slidersdata <- makegompertzModelCurve(TRT = trtcov)[1, ]
      updateSliderInput(session, "offsettrtslider",
                        value = slidersdata[["OffsetTRT"]])
      updateSliderInput(session, "alphatrtslider",
                        value = slidersdata[["AlphaTRT"]])
      updateSliderInput(session, "betatrtslider",
                        value = slidersdata[["BetaTRT"]])
      updateSliderInput(session, "gammatrtslider",
                        value = slidersdata[["GammaTRT"]])
      updateSliderInput(session, "baselinettpslider",
                        value = slidersdata[["BASELINETTP"]])
    }
  })

  output$download_btn <- downloadHandler(
    filename = "gompertz-data.csv",
    content = function(file) {
      if (input$download_size == "full") {
        data <- table_subset(first_row_only = FALSE)
      } else {
        data <- table_subset(first_row_only = TRUE)
      }
      write.csv(data, file, row.names = FALSE)
    }
  )
}

