# Return whether or not a string contains (non-whitespace) characters
notempty <- function(x) nzchar(trimws(x))

# Create an inline version of a shiny input
inlineInput <- function(tag) {
  stopifnot(inherits(tag, "shiny.tag"))
  tagAppendAttributes(tag, style = "display: inline-block; vertical-align: top;")
}

tvOffsetDefault <- 4.33843
tvAlphaDefault <- 22.4067
tvBetaDefault <- 2.22748
tvGammaDefault <- 0.04946
BASELINETTPDefault <- 6.295
treatments_default <- c(
  "TMC207",
  "Placebo (Background Regimen)",
  "MICRONUTRIENT/RHZE",
  "PA-824/PZ/M",
  "PHZE",
  "MRZE",
  "RHZE"
)

gompertz_default_args <- list(
  tvOffset = tvOffsetDefault,
  tvAlpha = tvAlphaDefault,
  tvBeta = tvBetaDefault,
  tvGamma = tvGammaDefault,
  TimeStartDays = 0,
  TimeEndDays = 120,
  TimebyDays = 0.1,
  BASELINETTP = BASELINETTPDefault,
  REFBASELINETTP = BASELINETTPDefault,
  TRT = "RHZE",
  dE0dTRTSIM = 1,
  dAlphadTRTSIM = 1,
  dBetadTRTSIM = 1,
  dGamdTRTSIM = 1
)

makegompertzModelCurve <- function(
  tvOffset = tvOffsetDefault,
  tvAlpha = tvAlphaDefault,
  tvBeta = tvBetaDefault,
  tvGamma = tvGammaDefault,
  TimeStartDays = gompertz_default_args$TimeStartDays,
  TimeEndDays = gompertz_default_args$TimeEndDays,
  TimebyDays = gompertz_default_args$TimebyDays,
  BASELINETTP = BASELINETTPDefault,
  REFBASELINETTP = BASELINETTPDefault,
  TRT = gompertz_default_args$TRT,
  dE0dTRTSIM = gompertz_default_args$dE0dTRTSIM,
  dAlphadTRTSIM = gompertz_default_args$dAlphadTRTSIM,
  dBetadTRTSIM = gompertz_default_args$dBetadTRTSIM,
  dGamdTRTSIM = gompertz_default_args$dGamdTRTSIM) {

  if (TRT == "Placebo (Background Regimen)") {
    TRT <- "Placebo"
  }

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
      REFBASELINETTP = rep(BASELINETTPDefault, 7)
    )

  default_trt_exists <- TRT %in% BASELINEINFO$TRT

  if (default_trt_exists) {
    BASELINETTP <- BASELINEINFO[BASELINEINFO$TRT == TRT, "BASELINETTP"]
    REFBASELINETTP <-
      BASELINEINFO[BASELINEINFO$TRT == TRT, "REFBASELINETTP"]
  } else {
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
    ifelse(!default_trt_exists, dE0dTRTSIM, 1)

  Alpha <-
    tvAlpha * (BASELINETTP / REFBASELINETTP) ^ dAlphadBLMEANTTP * exp(dAlphadTRT1 *
                                                                        (TRT == "TMC207")) *
    exp(dAlphadTRT2 * (TRT == "Placebo")) * exp(dAlphadTRT5 * (TRT == "MICRONUTRIENT/RHZE")) *
    exp(dAlphadTRT6 * (TRT == "PA-824/PZ/M")) * exp(dAlphadTRT7 * (TRT ==
                                                                     "PHZE")) *
    exp(dAlphadTRT17 * (TRT == "MRZE")) *
    ifelse(!default_trt_exists, dAlphadTRTSIM, 1)

  Beta <-
    tvBeta * (BASELINETTP / REFBASELINETTP) ^ dBetadBLMEANTTP * exp(dBetadTRT1 *
                                                                      (TRT == "TMC207")) *
    exp(dBetadTRT2 * (TRT == "Placebo")) * exp(dBetadTRT5 * (TRT == "MICRONUTRIENT/RHZE")) *
    exp(dBetadTRT6 * (TRT == "PA-824/PZ/M")) * exp(dBetadTRT7 * (TRT == "PHZE")) *
    exp(dBetadTRT17 * (TRT == "MRZE")) *
    ifelse(!default_trt_exists, dBetadTRTSIM, 1)

  Gamma <-
    tvGamma * (BASELINETTP / REFBASELINETTP) ^ dGamdBLMEANTTP * exp(dGamdTRT1 *
                                                                      (TRT == "TMC207")) *
    exp(dGamdTRT2 * (TRT == "Placebo")) * exp(dGamdTRT5 * (TRT == "MICRONUTRIENT/RHZE")) *
    exp(dGamdTRT6 * (TRT == "PA-824/PZ/M")) * exp(dGamdTRT7 * (TRT == "PHZE")) *
    exp(dGamdTRT17 * (TRT == "MRZE")) *
    ifelse(!default_trt_exists, dGamdTRTSIM, 1)

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

  df <- dplyr::data_frame(
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
