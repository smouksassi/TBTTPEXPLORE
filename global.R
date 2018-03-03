library(ggplot2)
library(ggrepel)
library(shinyjs)
library(shinyBS)
library(htmltools)
library(dplyr)

# Given a week range, figure out the human-friendly category name
# [1,2) becomes simply 1
# [11,13) becomes 11-12
week_category_name <- function(old_name) {
  # extract the two numbers out of the range
  limits <- unlist(strsplit(old_name, c("[^0-9]")))[-1]

  # if the range includes only one integer
  if (as.integer(limits[1]) + 1 == as.integer(limits[2])) {
    name <- limits[1]
  }
  # if the range includes multiple integers
  else {
    if (substr(old_name, nchar(old_name), nchar(old_name)) == "]") {
      name <- paste0(limits[1], "-", limits[2])
    } else {
      name <- paste0(limits[1], "-", as.integer(limits[2]) - 1)
    }
  }

  name
}

# validate a TTP dataset to ensure it contains the necessary columns and values
validate_dataset <- function(data) {
  required_cols <- c("WEEK", "TTP", "TRTDOSE")
  for (col in required_cols) {
    if (!col %in% colnames(data)) {
      error <- paste("Column", col, "must be present in the data")
      return(error)
    }
  }

  suppressWarnings(weeks <- as.integer(data$WEEK))
  if (any(is.na(weeks)) || any(weeks < 0)) {
    error <-  "WEEK must be non-negative integers"
    return(error)
  }

  return(TRUE)
}

# Given a TTP dataset with the WEEKS column as raw integers, convert it to bins
clean_dataset_weeks <- function(data) {
  data$WEEK_BIN <- data$WEEK
  max_week <- max(as.integer(data$WEEK))
  week_cats <-
    cut(
      data$WEEK_BIN,

      breaks = unique(sort(
        c(0:11, 13, 15, 17, 19, 21, 23, 25, max_week)
      )),

      include.lowest = TRUE,
      right = FALSE,
      ordered_result = TRUE
    )
  levels(week_cats) <-
    as.list(setNames(levels(week_cats),
                     lapply(levels(week_cats), week_category_name)))
  data$WEEK_BIN <- week_cats
  max_day <- max(as.integer(data$TIMEDAYS))
  day_cats <-
    cut(
      data$TIMEDAYS,
      breaks = unique(sort(c(
        0:15, 17, 19, 21, 23, 25, max_day
      ))) ,
      include.lowest = TRUE,
      right = FALSE,
      ordered_result = TRUE
    )
  levels(day_cats) <-
    as.list(setNames(levels(day_cats),
                     lapply(levels(day_cats), week_category_name)))
  data$DAY_BIN <- day_cats
  data
}

######################

# Read and set up the internal dataset
ttp_data <- read.csv(
  "data/TTPDATA_Shiny_PRED.csv",
  na.strings = c("", " ", ".", "NA", "na"),
  stringsAsFactors = FALSE
)
if (!isTRUE(validate_dataset(ttp_data))) {
  stop("There is a problem with the internal data file", call. = FALSE)
}
ttp_data <- clean_dataset_weeks(ttp_data)
ref_drug_list_all <- sort(as.character(unique(ttp_data$TRTDOSE)))
study_list <- sort(as.character(unique(ttp_data$STUDY)))


makegompertzModelCurve <- function(
  tvOffset = 4.33843,
  tvAlpha = 22.4067,
  tvBeta = 2.22748,
  tvGamma = 0.049461,
  TimeStartDays = 0,
  TimeEndDays = 120,
  TimebyDays = 0.1,
  BASELINETTP = 6.295,
  REFBASELINETTP = 6.295,
  TRT = "RHZE",
  dE0dTRTSIM = 1,
  dAlphadTRTSIM = 1,
  dBetadTRTSIM = 1,
  dGamdTRTSIM = 1) {

  if (!TRT %in% c(
    "RHZE",
    "TMC207",
    "Placebo (Background Regimen)",
    "MICRONUTRIENT/RHZE",
    "PA-824/PZ/M",
    "PHZE",
    "MRZE",
    "User Sim"
  )) {
    print(TRT)
    stop(
      paste(
        "TRT=",
        TRT,
        "not supported, please choose a treatment from the following possibilities:
        'RHZE', 'TMC207' ,'Placebo (Background Regimen)', 'MICRONUTRIENT/RHZE', 'PA-824/PZ/M','MRZE',
        'PHZE', 'User Sim'  "
      )
    )
  }

  TRT <- ifelse(TRT == "Placebo (Background Regimen)", "Placebo", TRT)

  BASELINEINFO <-
    data.frame(
      TRT = c(
        "RHZE",
        "TMC207",
        "Placebo",
        "MICRONUTRIENT/RHZE",
        "PA-824/PZ/M",
        "PHZE",
        "MRZE"
      ),
      BASELINETTP = c(6.50, 6.73, 6.71, 7.00, 4.03, 6.90, 6.00),
      REFBASELINETTP = c(6.295, 6.295, 6.295, 6.295, 6.295, 6.295, 6.295)
    )
  if (TRT != "User Sim") {
    BASELINETTP <- BASELINEINFO[BASELINEINFO$TRT == TRT, "BASELINETTP"]
    REFBASELINETTP <-
      BASELINEINFO[BASELINEINFO$TRT == TRT, "REFBASELINETTP"]
  }
  if (TRT == "User Sim") {
    BASELINETTP <- BASELINETTP
    REFBASELINETTP <- REFBASELINETTP
  }
  Time    <- seq(TimeStartDays, TimeEndDays, TimebyDays)
  #final cov coefficient estimates
  dE0dBLMEANTTP    <-  1.02709
  dAlphadBLMEANTTP <- -0.130315
  dBetadBLMEANTTP  <- -0.0126719
  dGamdBLMEANTTP   <- -0.133855

  dE0dTRT1 <-  0.134838
  dE0dTRT2 <-  0.120369
  dE0dTRT5 <-  0.0324585
  dE0dTRT6 <-  0.179031
  dE0dTRT7 <-  0.0113496
  dE0dTRT17 <- -0.0195158

  dAlphadTRT1 <- -0.331069
  dAlphadTRT2 <- -0.489191
  dAlphadTRT5 <- -0.289313
  dAlphadTRT6 <-  0.0394328
  dAlphadTRT7 <- -0.0613815
  dAlphadTRT17 <-  0.0523088

  dBetadTRT1 <- 0.227497
  dBetadTRT2 <- 0.162116
  dBetadTRT5 <- 0.154976
  dBetadTRT6 <- 0.277678
  dBetadTRT7 <- 0.0588087
  dBetadTRT17 <- 0.195526

  dGamdTRT1 <-  0.643565
  dGamdTRT2 <-  0.39996
  dGamdTRT5 <-  0.843865
  dGamdTRT6 <-  0.433691
  dGamdTRT7 <-  0.340369
  dGamdTRT17 <- 0.304038

  Offset <-
    tvOffset * (BASELINETTP / REFBASELINETTP) ^ dE0dBLMEANTTP * exp(dE0dTRT1 *
                                                                      (TRT == "TMC207")) *
    exp(dE0dTRT2 * (TRT == "Placebo")) * exp(dE0dTRT5 * (TRT == "MICRONUTRIENT/RHZE")) *
    exp(dE0dTRT6 * (TRT == "PA-824/PZ/M")) * exp(dE0dTRT7 * (TRT == "PHZE")) *
    exp(dE0dTRT17 * (TRT == "MRZE")) *
    ifelse(TRT == "User Sim", dE0dTRTSIM, 1)

  Alpha <-
    tvAlpha * (BASELINETTP / REFBASELINETTP) ^ dAlphadBLMEANTTP * exp(dAlphadTRT1 *
                                                                        (TRT == "TMC207")) *
    exp(dAlphadTRT2 * (TRT == "Placebo")) * exp(dAlphadTRT5 * (TRT == "MICRONUTRIENT/RHZE")) *
    exp(dAlphadTRT6 * (TRT == "PA-824/PZ/M")) * exp(dAlphadTRT7 * (TRT ==
                                                                     "PHZE")) *
    exp(dAlphadTRT17 * (TRT == "MRZE")) *
    ifelse(TRT == "User Sim", dAlphadTRTSIM, 1)

  Beta <-
    tvBeta * (BASELINETTP / REFBASELINETTP) ^ dBetadBLMEANTTP * exp(dBetadTRT1 *
                                                                      (TRT == "TMC207")) *
    exp(dBetadTRT2 * (TRT == "Placebo")) * exp(dBetadTRT5 * (TRT == "MICRONUTRIENT/RHZE")) *
    exp(dBetadTRT6 * (TRT == "PA-824/PZ/M")) * exp(dBetadTRT7 * (TRT == "PHZE")) *
    exp(dBetadTRT17 * (TRT == "MRZE")) *
    ifelse(TRT == "User Sim", dBetadTRTSIM, 1)

  Gamma <-
    tvGamma * (BASELINETTP / REFBASELINETTP) ^ dGamdBLMEANTTP * exp(dGamdTRT1 *
                                                                      (TRT == "TMC207")) *
    exp(dGamdTRT2 * (TRT == "Placebo")) * exp(dGamdTRT5 * (TRT == "MICRONUTRIENT/RHZE")) *
    exp(dGamdTRT6 * (TRT == "PA-824/PZ/M")) * exp(dGamdTRT7 * (TRT == "PHZE")) *
    exp(dGamdTRT17 * (TRT == "MRZE")) *
    ifelse(TRT == "User Sim", dGamdTRTSIM, 1)

  TTPPRED <- Offset + Alpha * exp(-Beta * exp(-Gamma * Time))
  TIMETTPMAX50 <-
    log (log ((1 / 2 - Offset / (2 * Alpha)) ^ (-1 / Beta)) ^ (-1 / Gamma))
  TIMEEFFMAX50 <-
    log (log((0.5 * (1 + exp(
      -Beta
    ))) ^ (-1 / Beta)) ^ (-1 / Gamma))
  TTPMAX50 <- 0.5 * (Offset + Alpha)
  EEFFMAX50 <- Offset + 0.5 * Alpha * (1 + exp(-Beta))
  TTP0 <-   Offset + Alpha * exp(-Beta)
  TTPINF <-   Offset + Alpha
  DeltaTTP <- Alpha  - Alpha * exp(-Beta)

  Time90    <- seq(0, 90, 0.1)
  Time30    <- seq(0, 30, 0.1)
  TTPPRED90 <- Offset + Alpha * exp(-Beta * exp(-Gamma * Time90))
  TTPPRED30 <- Offset + Alpha * exp(-Beta * exp(-Gamma * Time30))
  TTPAUC3 = sum(diff(Time90) * na.omit(dplyr::lead(TTPPRED90) + TTPPRED90)) / 2
  TTPAUC1 = sum(diff(Time30) * na.omit(dplyr::lead(TTPPRED30) + TTPPRED30)) / 2


  OffsetBASELINEEFF <- (BASELINETTP / REFBASELINETTP) ^ dE0dBLMEANTTP
  AlphaBASELINEEFF <- (BASELINETTP / REFBASELINETTP) ^ dAlphadBLMEANTTP
  BetaBASELINEEFF <- (BASELINETTP / REFBASELINETTP) ^ dBetadBLMEANTTP
  GammaBASELINEEFF <- (BASELINETTP / REFBASELINETTP) ^ dGamdBLMEANTTP

  OffsetTRT <- Offset / (tvOffset * OffsetBASELINEEFF)
  AlphaTRT <- Alpha / (tvAlpha * AlphaBASELINEEFF)
  BetaTRT  <- Beta / (tvBeta * BetaBASELINEEFF)
  GammaTRT <- Gamma / (tvGamma * GammaBASELINEEFF)
  TRT <- ifelse(TRT == "Placebo", "Placebo (Background Regimen)", TRT)

  df <- data.frame(
    Time = Time,
    TTPPRED = TTPPRED,
    TIMETTPMAX50 = TIMETTPMAX50,
    TIMEEFFMAX50 = TIMEEFFMAX50,
    TTPMAX50 = TTPMAX50,
    EEFFMAX50 = EEFFMAX50,
    TTP0 = TTP0,
    TTPINF = TTPINF,
    DeltaTTP = DeltaTTP,
    TRT = TRT,
    Offset = Offset,
    Alpha = Alpha,
    Beta = Beta,
    Gamma = Gamma,
    BASELINETTP = BASELINETTP,
    REFBASELINETTP = REFBASELINETTP,
    TTPAUC1 = TTPAUC1,
    TTPAUC3 = TTPAUC3,
    OffsetTRT = OffsetTRT,
    AlphaTRT = AlphaTRT,
    BetaTRT = BetaTRT,
    GammaTRT = GammaTRT,
    tvOffset = tvOffset,
    tvAlpha = tvAlpha,
    tvBeta = tvBeta,
    tvGamma = tvGamma,
    OffsetBASELINEEFF = OffsetBASELINEEFF,
    AlphaBASELINEEFF = AlphaBASELINEEFF,
    BetaBASELINEEFF = BetaBASELINEEFF,
    GammaBASELINEEFF = GammaBASELINEEFF

  )
  df
}

placebosims <- makegompertzModelCurve(TRT = "Placebo (Background Regimen)")
#makegompertzModelCurve(TRT="User Sim")[1,]
makegompertzModelCurve(TRT = "Placebo (Background Regimen)")[1, ]

radioTooltip <- function(id, choice, title, placement = "bottom",
                         trigger = "hover", options = NULL) {
    options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
    options = paste0("{'", paste(
      names(options),
      options,
      sep = "': '",
      collapse = "', '"
    ), "'}")
    bsTag <- shiny::tags$script(shiny::HTML(
      paste0(
        "
        $(document).ready(function() {
          setTimeout(function() {
            $('input', $('#", id, "')).each(function() {
              if(this.getAttribute('value') == '", choice, "') {
                opts = $.extend(", options,", { html: true });
                $(this.parentElement).tooltip('destroy');
                $(this.parentElement).tooltip(opts);
              }
            })
          }, 500)
        });
        "
      )
    ))
    htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
  }

stripAttributes <- function (data) {
  attr <- names(attributes(data))
  good <- c("names", "row.names", "class")
  for (i in attr[!attr %in% good]) {
    attr(data, i) <- NULL
  }
  return(data)
}
