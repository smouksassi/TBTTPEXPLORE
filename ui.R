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

      includeMarkdown(file.path("text", "TBStudyDescription.md"))
    ),

    tabPanel(
      "Plots",
      value = "plots",
      icon = icon("bar-chart"),
      class = "fade in",

      plots_tab_module_ui("plots")
    ),

    tabPanel(
      "Gompertz Model",
      value = "plots",
      icon = icon("line-chart"),
      class = "fade in",

      gompertz_tab_module_ui("model")
    ),

    tabPanel(
      "Help",
      value = "howto",
      icon = icon("question-circle"),
      class = "fade in",

      tabsetPanel(
        type = "pills",
        tabPanel(
          "Plots",
          includeMarkdown(file.path("text", "plots-help.md"))
        ),
        tabPanel(
          "Gompertz Model",
          withMathJax(includeHTML(file.path("text", "gompertz.html")))
        )
      )
    )
  )
)
