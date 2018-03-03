function(input, output, session) {
  # Initialize the plots tab
  callModule(plots_tab_module, "ttp")

  # Initialize the Gompertz Model tab
  callModule(gompertz_tab_module, "ttp")
}
