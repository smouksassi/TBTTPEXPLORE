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
        12,
        #plotOutput(outputId = ns("gompertzcurve"))
        plotlyOutput(outputId = ns("gompertzcurveplotly"),height="500px")
      ),
      column(12,
                 helpText("The legend Items are interactive and plot elements can be toggled on/off by pressing on the appropriate item.")
      ),
      column(
        6,
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
        )
        ),
      column(
        6,
        selectInput(
          ns("trtcovbackground"),
          label = "Compare to:",
          choices = list(
            "Default Treatments" = treatments_default_
          ),
          selected = "Placebo"
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
      column(
        12,
        h2("Custom Simulation"),
        helpText("Please Select Live Simulation in the Treatments dropdown. Then the simulated curve will update live according to the sliders selections.")      )
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
          "Save Custom Simulation",
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


  output$gompertzcurveplotly <- renderPlotly({
    plotdata <- refcurve()
    plotdata$TRT <- as.factor(plotdata$TRT)
    gompertzdataparams <- plotdata[1,]
    backdata <- comparetourve()
    backdata$TRT <- as.factor(backdata$TRT)
    backdataparams <- backdata[1,]
    alldata <- rbind(backdata, plotdata)
    #alldata <-makegompertzModelCurve()
    alldata <- alldata %>%
      group_by(TRT) %>%
      mutate(
        TIMETTPFLAG = ifelse(Time >= TIMETTPMAX50, 1, 0) ,
        cumsumttpflag = cumsum(TIMETTPFLAG)
      )

    pointsdata <- alldata %>%
      group_by(TRT) %>%
      filter(cumsumttpflag == 1)


    pointsdata2 <- alldata %>%
      group_by(TRT) %>%
      filter(Time == TIMEEFFMAX50)

    alldataauc<- alldata[alldata$Time<=28,]

    p <-
      plot_ly(
        alldata,
        x = ~ Time,
        y = ~ TTPPRED,
        type = 'scatter',
        mode = 'lines',
        name = 'TTP(t)',
        legendgroup = "TTP(t)",
        line = list(color = toRGB("black", alpha = 0.5), width = 5),
        linetype =  ~ TRT,
        color = I('black'),
        hoverinfo = 'text',
        text = ~ paste(
          "</br> Treatment: ",
          TRT,
          '</br> Time: ',
          round(Time, 1),
          '</br> TTP(t): ',
          round(TTPPRED, 1)
        )
      ) %>%
      add_ribbons(
        data = alldataauc,
        x =  ~ Time ,
        ymin = 0,
        ymax =  ~ TTPPRED,
        name = 'AUC28',
        legendgroup = "AUC<sub>28</sub>",
        fillcolor = ~ TRT ,
        line = list(color = 'blue'),
        linetype =  ~ TRT,
        opacity = 0.2,
        hoverinfo = 'none'
      ) %>%
      add_markers(
        data = pointsdata2 ,
        x = 0,
        y = ~ TTP0,
        type = 'scatter',
        mode = 'markers',
        marker  = list(
          color = toRGB("black", alpha = 0.7),
          size = 20,
          symbol = "circle"
        ),
        name = "TTP(0)",
        legendgroup = "TTP(0)",
        inherit = FALSE,
        hoverinfo = 'text',
        text = ~ paste("</br> Treatment: ", TRT,
                       '</br> TTP(0): ', round(TTP0, 1))

      ) %>%

      add_markers(
        data = pointsdata ,
        x =  ~ TIMETTPMAX50,
        y = ~ TTPMAX50,
        type = 'scatter',
        mode = 'markers',
        marker  = list(
          color = toRGB("black", alpha = 0.5),
          size = 20,
          symbol = "triangle-up"
        ),
        name = "TTP<sub>50</sub>",
        legendgroup = "TTP<sub>50</sub>",
        inherit = FALSE,
        hoverinfo = 'text',
        text = ~ paste(
          "</br> Treatment: ",
          TRT,
          '</br> Time to TTP<sub>50</sub>: ',
          round(TIMETTPMAX50, 1),
          '</br> TTP<sub>50</sub>: ',
          round(TTPMAX50, 1)
        )

      ) %>%

      add_markers(
        data = pointsdata2 ,
        x =  ~ TIMEEFFMAX50,
        y = ~ EEFFMAX50,
        type = 'scatter',
        mode = 'markers',
        marker  = list(
          color = toRGB("black", alpha = 0.3),
          size = 20,
          symbol = "diamond"
        ),
        name = "EFF<sub>50</sub>",
        legendgroup = "EFF<sub>50</sub>",
        inherit = FALSE,
        hoverinfo = 'text',
        text = ~ paste(
          "</br> Treatment: ",
          TRT,
          '</br> Time to EFF<sub>50</sub>: ',
          round(TIMEEFFMAX50, 1),
          '</br> EFF<sub>50</sub>: ',
          round(EEFFMAX50, 1)
        )

      ) %>%
      add_lines(
        data = alldata ,
        x = ~ Time,
        y = ~ TTPINF,
        type = 'scatter',
        mode = 'lines',
        name = 'TTP(\u221E)',
        line = list(color = toRGB("black", alpha = 0.5), width = 5),
        linetype =  ~ TRT,
        color = I('black'),
        legendgroup = "TTP(\u221E)",
        inherit = FALSE,
        hoverinfo = 'text',
        text = ~ paste("</br> Treatment: ", TRT,
                       '</br> TTP(\u221E) ', round(TTPINF, 1))
      ) %>%

      layout(
        xaxis = list(
          title = 'Days Post Start of Treatment',
          ticks = "outside",
          autotick = TRUE,
          ticklen = 5,
          range = c(-2, 121),
          gridcolor = toRGB("gray50"),
          showline = TRUE
        ) ,
        yaxis = list (
          title = 'TTP (t), Days'               ,
          ticks = "outside",
          autotick = TRUE,
          ticklen = 5,
          gridcolor = toRGB("gray50"),
          showline = TRUE)
        # ) ,
        # legend = list(
        #   orientation = 'h',
        #   xanchor = "center",
        #   y = -0.25,
        #   x = 0.5
        # )
      )
    p2 <-
      plot_ly(
    alldata,
    x = ~ Time,
    y = ~ Ft,
    type = 'scatter',
    mode = 'lines',
    name = 'F(t)-AUC28',
    legendgroup = "F(t)-AUC28",
    line = list(color = toRGB("blue", alpha = 0.2), width = 5),
    linetype =  ~ TRT,
    color = I('black'),
    hoverinfo = 'text',
    text = ~ paste(
      "</br> Treatment: ",
      TRT,
      '</br> Time: ',
      round(Time, 1),
      '</br> F(t): ',
      round(Ft, 1)
    )
  ) %>%
  layout(
    xaxis = list(
      title = 'Days Post Start of Treatment',
      ticks = "outside",
      autotick = TRUE,
      ticklen = 5,
      range = c(-2, 121),
      gridcolor = toRGB("gray50"),
      showline = TRUE
    ) ,
    yaxis = list (
      title = 'F(t), Probability'               ,
      ticks = "outside",
      autotick = TRUE,
      ticklen = 5,
      gridcolor = toRGB("gray50"),
      showline = TRUE
    ) ,
    legend = list(
      orientation = 'h',
      xanchor = "center",
      y = -0.25,
      x = 0.5
    )
  )
subplot(p, p2,nrows = 2,shareX = TRUE,shareY = FALSE,titleX = TRUE,titleY = TRUE)

  })

  cols_table_1 <- c("TRT", "BASELINETTP", "Offset", "Alpha", "Beta", "Gamma")
  cols_table_2 <- c("TRT", "TTP0", "TTPMAX50", "EEFFMAX50", "TTPINF", "DeltaTTP",
                    "TIMETTPMAX50", "TIMEEFFMAX50", "TTPAUC1", "TTPAUC3","TTPAUC14","TTPAUC28","TTPAUC56")

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

