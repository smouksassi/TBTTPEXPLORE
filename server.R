function(input, output, session) {
  # Initialize the plots tab
  callModule(plots_tab_module, "plots")

  # Initialize the Gompertz Model tab
  callModule(gompertz_tab_module, "model")
}
