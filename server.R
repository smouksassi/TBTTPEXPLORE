function(input, output, session) {

  ref_data <- reactive({
    # If a specific study is requested, subset by study
    if (input$subset_by_study != "") {
      validate(need(input$subset_by_study %in% ttp_data$STUDY, "Study not in dataset"))
      subset(ttp_data, STUDY == input$subset_by_study)
    } else {
      ttp_data
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
    if (using_custom_file()) return()
    updateSelectInput(
      session, "test_treat",
      choices = c("", setdiff(ref_drug_list(), input$reference_treat)),
      selected = input$test_treat
    )
  })
  observeEvent(input$test_treat, {
    if (using_custom_file()) return()
    updateSelectInput(
      session, "reference_treat",
      choices = c("", setdiff(ref_drug_list(), input$test_treat)),
      selected = input$reference_treat
    )
  })
  observeEvent(input$subset_by_study, {
    updateSelectInput(
      session, "reference_treat",
      choices = c("", setdiff(ref_drug_list(), input$test_treat)),
      selected = ""
    )
    if (!using_custom_file()) {
      updateSelectInput(
        session, "test_treat",
        choices = c("", setdiff(ref_drug_list(), input$reference_treat)),
        selected = ""
      )
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
    ref_rows <- ref_rows[, c("DAY_BIN", "WEEK_BIN", "TTP", "TRTDOSE", "STUDY","PRED")]
    ref_rows$TRTDOSE_NAME <- paste0("Reference: ", input$reference_treat)
    test_rows <- subset(test_data(), TRTDOSE == input$test_treat)
    # If using a custom file, there may not be any study information
    if (is.null(test_rows$STUDY)) {
      test_rows$STUDY <- "Unspecified"
    }
    test_rows <- test_rows[, c("DAY_BIN", "WEEK_BIN", "TTP", "TRTDOSE", "STUDY","PRED")]
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
if(input$exclude42){
  data<- data[data$TTP<42,]
}
    plot <- ggplot(data, aes(TIME_BIN, TTP)) +
      aes(color = TRTDOSE, group = TRTDOSE) + 
      geom_point(alpha = 0.5, shape = 16, size = 1) +
      geom_boxplot(aes(group = NULL), varwidth = FALSE,
                   notch = FALSE) + 
      stat_summary(aes(y=TTP,x=TIME_BIN,group=TRTDOSE,linetype="Median (Obs)",color = TRTDOSE), fun.y = median,
                   geom = "line",inherit.aes = FALSE,size=1.5)+
      xlab(xlab) +
      ylab("TTP (Days)") +
      theme_bw(base_size = 16) +
      theme(legend.position = "bottom",
            legend.box = "horizontal", legend.direction = "horizontal",
            axis.text.x = ggplot2::element_text(angle = 90,
                                                hjust = 1, vjust = 0.5),
            legend.title = element_blank())
    
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
      give.n <- function(x){
        return(c(y = min(x)*1.05,  label = length(x))) 
      }
      plot <- plot +
        stat_summary(fun.data = give.n,
                     aes(group = NULL), geom = "label", alpha = 0.1,
                     fun.y = median, fontface = "bold", fill = "white",
                     show.legend = FALSE, size = 6,
                     position = position_dodge(width = 0.8))
    }
    if (input$opt_median) {
      median.n <- function(x){
        return(c(y = ifelse(median(x) < 0,median(x),median(x)),
                 label = round(median(x),1))) 
      }
      plot <- plot + 
        stat_summary(fun.data = median.n,
                     aes(group = NULL), geom = "label", alpha = 0.1,
                     fun.y = median, fontface = "bold", fill = "white",
                     show.legend = FALSE, size = 6,
                     position = position_dodge(width = 0.8))
    }
    
    if (input$opt_pred) {
      plot <- plot + 
        stat_summary(aes(y=PRED,x=TIME_BIN,group=TRTDOSE,linetype="PRED",color = TRTDOSE), fun.y = median,
                     geom = "line",inherit.aes = FALSE,size=1.5) 
    }
    
    plot+
      guides(linetype=guide_legend(order=2))
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
    ), silent = TRUE)
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
      data$PRED<- NA
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
        session, "reference_treat",
        choices = c("", setdiff(ref_drug_list(), input$test_treat)),
        selected = input$reference_treat)
      updateSelectInput(
        session, "test_treat",
        choices = c("", setdiff(ref_drug_list(), input$reference_treat)),
        selected = input$test_treat)
      return()
    } else {
      updateSelectInput(
        session, "reference_treat",
        choices = c("", ref_drug_list()),
        selected = input$reference_treat)
      
      test_drugs <- as.character(unique(custom_data()$TRTDOSE))
      updateSelectInput(
        session, "test_treat",
        choices = c("", test_drugs),
        selected = input$test_treat)
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
  
  observeEvent(input$trtcov, {
    choices = c("Placebo (Background Regimen)","TMC207",
                "MICRONUTRIENT/RHZE","PA-824/PZ/M",
                "PHZE","MRZE","RHZE")[c("Placebo (Background Regimen)","TMC207",
                                        "MICRONUTRIENT/RHZE","PA-824/PZ/M",
                                        "PHZE","MRZE","RHZE")!= input$trtcov]
    updateSelectInput(
      session, "trtcovbackground",
       choices = choices)
  })
  
  
  
  observeEvent(input$helpmodal, {
    showModal(modalDialog(
      title = "Gompertz Model",
      withMathJax(),
      includeHTML(file.path("text", "gompertz.html")),
      easyClose = TRUE
    ))
  })
  
  refcurve<- reactive({
    trtcov<- input$trtcov
    BASELINETTP<- input$baselinettpslider
    E0TRT <- input$offsettrtslider
    ATRT<- input$alphatrtslider
    BTRT<- input$betatrtslider
    GTRT<- input$gammatrtslider
    
    E0<- input$offsetslider
    A<- input$alphaslider
    B<- input$betaslider
    G<- input$gammaslider
    
    REFBASELINETTP<- 6.295
    if(trtcov=="User Sim"){
      plotdata<- makegompertzModelCurve(
        tvOffset=E0,tvAlpha=A, tvBeta=B,tvGamma=G,
        TimeStartDays=0,TimeEndDays=120,TimebyDays=0.1,
        BASELINETTP=BASELINETTP,
        REFBASELINETTP=REFBASELINETTP,
        TRT=trtcov,
        dE0dTRTSIM   =E0TRT,
        dAlphadTRTSIM=ATRT,
        dBetadTRTSIM =BTRT,
        dGamdTRTSIM  =GTRT)
    }
    if(trtcov!="User Sim"){
      plotdata<- makegompertzModelCurve(
        TimeStartDays=0,TimeEndDays=120,TimebyDays=0.1,
        REFBASELINETTP=REFBASELINETTP,
        TRT=trtcov)
    }
    
    
    
    plotdata
  })
  
  
  
  
comparetourve<- reactive({
    trtcov<- input$trtcovbackground
    plotdata<- makegompertzModelCurve(TRT=trtcov)
    plotdata
  })
  
  output$gompertzcurve<-renderPlot({
    plotdata<- refcurve()
    gompertzdataparams<- plotdata[1,]
    backdata<- comparetourve()
    backdataparams<- backdata[1,]
    alldata<- rbind(backdata,plotdata)
    alldataparams<- rbind(backdataparams,gompertzdataparams)
    alldataparamsrepel1<- alldataparams[,c("TRT","TIMEEFFMAX50","EEFFMAX50","TTP0","TTPINF")]
    alldataparamsrepel1$parameter<- "Eff(50)"
    alldataparamsrepel2<- alldataparams[,c("TRT","TIMETTPMAX50","TTPMAX50","TTP0","TTPINF")]
    alldataparamsrepel2$parameter<- "TTP(50)"
    names(alldataparamsrepel1) <- names(alldataparamsrepel2)  <- c("TRT","TIME","PARAM","TTP0","TTPINF","parameter")
    alldataparamsrepel<- rbind(alldataparamsrepel1,alldataparamsrepel2)
    alldataparamsrepelinf<- alldataparamsrepel[!duplicated(alldataparamsrepel$TRT),]
    p <-  ggplot(alldata,aes(Time,TTPPRED) )
    p <- p +
      geom_hline(data=alldataparamsrepelinf,aes(yintercept=TTP0,linetype=TRT),show.legend = FALSE)+
      geom_hline(data=alldataparamsrepelinf,aes(yintercept=TTPINF,linetype=TRT),show.legend = FALSE)+
      geom_segment(data=alldataparams,aes(x=TIMETTPMAX50,  y= TTP0,linetype=TRT,
                                          xend=TIMETTPMAX50,yend=TTPMAX50),show.legend = FALSE)+
      geom_segment(data=alldataparams,aes(x=TIMEEFFMAX50,   y= TTP0,linetype=TRT,
                                          xend=TIMEEFFMAX50,yend=EEFFMAX50),show.legend = FALSE)+
      geom_segment(data=alldataparams,aes(   x=Time,      y= TTPMAX50,linetype=TRT,
                                             xend=TIMETTPMAX50,yend=TTPMAX50),show.legend = FALSE)+
      geom_segment(data=alldataparams,aes(x=Time,   y= EEFFMAX50,linetype=TRT,
                                          xend=TIMEEFFMAX50,yend=EEFFMAX50),show.legend = FALSE)+
      geom_line(aes(linetype=TRT),size=1.5,alpha=0.5)+  
      geom_point(data=alldataparamsrepel,aes(x=TIME,y= PARAM,shape=parameter,fill=TRT),size=5,alpha=0.8)+
      geom_label_repel(data=alldataparamsrepel,aes(x=0,y= PARAM,fill=TRT,
                                                   label=paste(round(PARAM,1))),
                       direction = "y",alpha=0.6,show.legend = FALSE)+
      
      geom_label_repel(data=alldataparamsrepel,aes(x=TIME,y= TTP0*0.9,fill=TRT,
                                                   label=paste(round(TIME,1))),
                       direction = "both",alpha=0.6,show.legend = FALSE)+
      
      geom_label_repel(data=alldataparamsrepelinf,aes(x=+Inf,y= TTP0*1.05,fill=TRT,
                                                      label=paste("TTP(0) =",round(TTP0,1))))+
      geom_label_repel(data=alldataparamsrepelinf,aes(x=+Inf,y= TTPINF*1.05,fill=TRT,
                                                      label=paste("TTP(\u221E) =",round(TTPINF,1))))+  
      
      
      labs(y="TTP (t), Days",x="Days Post Start of Treatment",linetype="",colour="",shape="")+
      scale_shape_manual(values=c(23,24),
                         labels=c(expression(Eff[50]),expression(TTP[50])))+
      scale_fill_manual(values=c("darkgray","white"))+
      
      scale_x_continuous(breaks=c(0,30,60,90,120))+
      theme_bw(base_size = 20) +
      guides(
        linetype = guide_legend(order = 1),
        shape = guide_legend(order = 2,reverse=TRUE),
        fill=FALSE
      )+
      coord_cartesian(xlim=c(0,120))+
      theme(legend.key.width  = unit(2, "cm"),legend.position="bottom",legend.box = "vertical")
    
    print(p)

})





  

  output$table1<-renderTable({
    plotdata<- refcurve()
    gompertzdataparams<- plotdata[1,]
    comparetourve<- comparetourve()[1,]
    df<- rbind(gompertzdataparams,comparetourve)
    df<- df[,c("TRT","BASELINETTP","Offset","Alpha","Beta","Gamma")]
    df
  },include.rownames=FALSE)

output$table2<-renderTable({
  plotdata<- refcurve()
  gompertzdataparams<- plotdata[1,]
  comparetourve<- comparetourve()[1,]
  df<- rbind(gompertzdataparams,comparetourve)
  df<- df[,c("TRT","TTP0","TTPMAX50","EEFFMAX50","TTPINF",
               "DeltaTTP","TIMETTPMAX50","TIMEEFFMAX50","TTPAUC1","TTPAUC3")]
  df
},include.rownames=FALSE)


observeEvent(
  {input$resetpop }
  , ignoreNULL=FALSE, {
    shinyjs::reset("offsetslider")
    shinyjs::reset("alphaslider")
    shinyjs::reset("betaslider")
    shinyjs::reset("gammaslider")
    shinyjs::reset("baselinettpslider")
  })


observe({  
    trtcov<- input$slidersinitials
    if(trtcov!=""){
      slidersdata<- makegompertzModelCurve(TRT=trtcov)[1,]
      updateSliderInput(session,"offsettrtslider",
                        value=slidersdata[,"OffsetTRT"])  
      updateSliderInput(session,"alphatrtslider",
                        value=slidersdata[,"AlphaTRT"])  
      updateSliderInput(session,"betatrtslider",
                        value=slidersdata[,"BetaTRT"])  
      updateSliderInput(session,"gammatrtslider",
                        value=slidersdata[,"GammaTRT"])
      updateSliderInput(session,"baselinettpslider",
                        value=slidersdata[,"BASELINETTP"])  
    }
 
  })

}


