function(input, output, session) {
  callModule(plots_tab_module, "ttp", ttp_data_ = ttp_data)
  callModule(gompertz_tab_module, "ttp")
}
