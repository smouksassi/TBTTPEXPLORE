function(input, output, session) {
  # Initialize the plots tab
  callModule(plots_tab_module, "ttp", ttp_data_ = ttp_data)

  # Initialize the Gompertz Model tab
  callModule(gompertz_tab_module, "ttp",
             BASELINETTP_ = BASELINETTPDefault,
             treatments_default_ = treatments_default)
}
