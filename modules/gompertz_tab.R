# Module for the gompertz model tab in the TTP app

gompertz_tab_module_ui <- function(id, tvOffset_, tvAlpha_, tvBeta_, tvGamma_) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        12,
        actionButton(ns("helpmodal"),
                     "Model Details and how to use this Page",
                     class = "btn-primary")
      )
    ),
    fluidRow(
      column(
        8,
        plotOutput(outputId = ns("gompertzcurve"))
      ),
      column(
        4,
        radioButtons(
          ns("trtcov"),
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
          ns("trtcov"),
          choice = "Placebo (Background Regimen)",
          title = "Placebo (Background regimen) consisted of preferred five-drug, second-line anti-TB background regimen (e.g., aminoglycosides, fluoroquinolones, ethionamide or protionamide, pyrazinamide, ethambutol, cycloserine or terizidone)",
          placement = "right",
          options = list(container = "body")
        ),
        radioTooltip(
          ns("trtcov"),
          choice = "User Sim",
          title = "User can specify Baseline TTP and custom Treatment effects on various parameters",
          placement = "right",
          options = list(container = "body")
        ),

        selectInput(
          ns("trtcovbackground"),
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
      )
    ),
    fluidRow(
      column(12, hr())
    ),
    conditionalPanel(
      condition = "input.trtcov == 'User Sim' ",
      ns = ns,
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
          checkboxInput(ns("changepopparams"),
                        "Change Population Values TTP?", FALSE)
        )
      ),
      conditionalPanel(
        condition = "input.changepopparams && input.trtcov == 'User Sim' ",
        ns = ns,
        fluidRow(
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
              "Reset Baseline TTP and  Population Parameters to Initial Values"
            ),
            br(), br()
          )
        )
      )
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
    )
  )
}

gompertz_tab_module <- function(input, output, session) {
  observeEvent(input$trtcov, {
    choices = c(
      "Placebo (Background Regimen)",
      "TMC207",
      "MICRONUTRIENT/RHZE",
      "PA-824/PZ/M",
      "PHZE",
      "MRZE",
      "RHZE"
    )[c(
      "Placebo (Background Regimen)",
      "TMC207",
      "MICRONUTRIENT/RHZE",
      "PA-824/PZ/M",
      "PHZE",
      "MRZE",
      "RHZE"
    ) != input$trtcov]
    updateSelectInput(session, "trtcovbackground",
                      choices = choices)
  })

  observeEvent(input$helpmodal, {
    showModal(
      modalDialog(
        title = "Gompertz Model",
        withMathJax(includeHTML(file.path("text", "gompertz.html"))),
        easyClose = TRUE
      )
    )
  })

  refcurve <- reactive({
    trtcov <- input$trtcov
    BASELINETTP <- input$baselinettpslider
    E0TRT <- input$offsettrtslider
    ATRT <- input$alphatrtslider
    BTRT <- input$betatrtslider
    GTRT <- input$gammatrtslider

    E0 <- input$offsetslider
    A <- input$alphaslider
    B <- input$betaslider
    G <- input$gammaslider

    REFBASELINETTP <- 6.295
    if (trtcov == "User Sim") {
      plotdata <- makegompertzModelCurve(
        tvOffset = E0,
        tvAlpha = A,
        tvBeta = B,
        tvGamma = G,
        TimeStartDays = 0,
        TimeEndDays = 120,
        TimebyDays = 0.1,
        BASELINETTP = BASELINETTP,
        REFBASELINETTP = REFBASELINETTP,
        TRT = trtcov,
        dE0dTRTSIM   = E0TRT,
        dAlphadTRTSIM = ATRT,
        dBetadTRTSIM = BTRT,
        dGamdTRTSIM  = GTRT
      )
    }
    if (trtcov != "User Sim") {
      plotdata <- makegompertzModelCurve(
        TimeStartDays = 0,
        TimeEndDays = 120,
        TimebyDays = 0.1,
        REFBASELINETTP = REFBASELINETTP,
        TRT = trtcov
      )
    }



    plotdata
  })




  comparetourve <- reactive({
    trtcov <- input$trtcovbackground
    plotdata <- makegompertzModelCurve(TRT = trtcov)
    plotdata
  })

  output$gompertzcurve <- renderPlot({
    plotdata <- refcurve()
    gompertzdataparams <- plotdata[1, ]
    backdata <- comparetourve()
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

  output$table1 <- renderTable({
    plotdata <- refcurve()
    gompertzdataparams <- plotdata[1, ]
    comparetourve <- comparetourve()[1, ]
    df <- rbind(gompertzdataparams, comparetourve)
    df <- df[, c("TRT", "BASELINETTP", "Offset", "Alpha", "Beta", "Gamma")]
    df
  }, include.rownames = FALSE)

  output$table2 <- renderTable({
    plotdata <- refcurve()
    gompertzdataparams <- plotdata[1, ]
    comparetourve <- comparetourve()[1, ]
    df <- rbind(gompertzdataparams, comparetourve)
    df <- df[, c(
      "TRT",
      "TTP0",
      "TTPMAX50",
      "EEFFMAX50",
      "TTPINF",
      "DeltaTTP",
      "TIMETTPMAX50",
      "TIMEEFFMAX50",
      "TTPAUC1",
      "TTPAUC3"
    )]
    df
  }, include.rownames = FALSE)


  observeEvent({
    input$resetpop
  }
  , ignoreNULL = FALSE, {
    shinyjs::reset("offsetslider")
    shinyjs::reset("alphaslider")
    shinyjs::reset("betaslider")
    shinyjs::reset("gammaslider")
    shinyjs::reset("baselinettpslider")
  })


  observe({
    trtcov <- input$slidersinitials
    if (trtcov != "") {
      slidersdata <- makegompertzModelCurve(TRT = trtcov)[1, ]
      updateSliderInput(session, "offsettrtslider",
                        value = slidersdata[, "OffsetTRT"])
      updateSliderInput(session, "alphatrtslider",
                        value = slidersdata[, "AlphaTRT"])
      updateSliderInput(session, "betatrtslider",
                        value = slidersdata[, "BetaTRT"])
      updateSliderInput(session, "gammatrtslider",
                        value = slidersdata[, "GammaTRT"])
      updateSliderInput(session, "baselinettpslider",
                        value = slidersdata[, "BASELINETTP"])
    }

  })
}
