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
      "How to",
      value = "howto",
      icon = icon("info"),
      class = "fade in",

      includeMarkdown(file.path("text", "howto.md"))
    ),

    tabPanel(
      "Plots",
      value = "plots",
      icon = icon("bar-chart"),
      class = "fade in",

      plots_tab_module_ui(
        "ttp",
        ref_drug_list_all_ = ref_drug_list_all,
        study_list_ = study_list
      )
    ),

    tabPanel(
      "Gompertz Model",
      value = "plots",
      icon = icon("line-chart"),
      class = "fade in",

      gompertz_tab_module_ui(
        "ttp",
        tvOffset_ = tvOffsetDefault,
        tvAlpha_ = tvAlphaDefault,
        tvBeta_ = tvBetaDefault,
        tvGamma_ = tvGammaDefault
      )
    )
  )
)
