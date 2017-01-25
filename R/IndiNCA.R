#' Noncompartmental Analysis for an Individual
#'
#' It performs a noncompartmental analysis with one subject data
#'
#' This performs a noncompartmental analysis for a subject. It returns
#' practically the same result with the most popular commercial software.
#'
#' @param x vector values of independent variable, usually time
#' @param y vector values of dependent variable, usually concentration
#' @param Dose administered dose for the subject
#' @param Method either of \code{"Linear"} or \code{"Log"} to indicate the way
#' to calculate AUC and AUMC
#' @param AdmMode one of \code{"Bolus"} or \code{"Infusion"} or
#' \code{"Extravascular"} to indicate drug administration mode
#' @param TimeInfusion infusion duration for constant infusion, otherwise 0
#' @param RetNames character vector for the pharmacokinetic parameter names to
#' be returned
#' @param Report either of \code{"Table"} or \code{"Text"} to specify the type
#' of return value
#' @param iAUC data.frame with three columns, "Name", "Start", "End" to specify
#' the invervals for partial (interval) AUC
#' @return \item{CMAX}{maximum concentration, Cmax} \item{CMAXD}{dose
#' normalized Cmax, CMAX / Dose, Cmax / Dose} \item{TMAX}{time of maximum
#' concentration, Tmax} \item{TLAG}{time to observe the first non-zero
#' concentration, for extravascular administration only} \item{CLST}{last
#' positive concentration observed, Clast} \item{CLSTP}{last positive
#' concentration predicted, Clast_pred} \item{TLST}{time of last positive
#' concentration, Tlast} \item{LAMZHL}{half-life by lambda z, ln(2)/LAMZ}
#' \item{LAMZ}{lambda_z negative of best fit terminal slope}
#' \item{LAMZLL}{earliest time for LAMZ} \item{LAMZUL}{last time for LAMZ}
#' \item{LAMZNPT}{number of points for LAMZ} \item{CORRXY}{correlation of
#' log(concentration) and time} \item{R2}{R-squared} \item{R2ADJ}{R-squared
#' adjusted} \item{C0}{back extrapolated concentration at time 0, for bolus
#' intravascular administration only} \item{AUCLST}{AUC from 0 to TLST}
#' \item{AUCALL}{AUC using all the given points, including trailing zero
#' concentrations} \item{AUCIFO}{AUC infinity observed} \item{AUCIFOD}{AUCIFO /
#' Dose} \item{AUCIFP}{AUC infinity predicted using CLSTP instead of CLST}
#' \item{AUCIFPD}{AUCIFP / Dose} \item{AUCPEO}{AUC \% extrapolation observed}
#' \item{AUCPEP}{AUC \% extrapolated for AUCIFP} \item{AUCPBEO}{AUC \% back
#' extrapolation observed, for bolus IV administration only} \item{AUCPBEP}{AUC
#' \% back extrapolation predicted with AUCIFP, for bolus IV administration
#' only} \item{AUMCLST}{AUMC to the TLST} \item{AUMCIFO}{AUMC infinity observed
#' using CLST} \item{AUMCIFP}{AUMC infinity determined by CLSTP}
#' \item{AUMCPEO}{AUMC \% extrapolated observed} \item{AUMCPEP}{AUMC \%
#' extrapolated predicted} \item{MRTIVLST}{mean residence time (MRT) to TLST,
#' for intravascular administration} \item{MRTIVIFO}{mean residence time (MRT)
#' infinity using CLST, for intravascular administration} \item{MRTIVIFP}{mean
#' residence time (MRT) infinity using CLSTP, for intravascular administration}
#' \item{MRTEVLST}{mean residence time (MRT) to TLST, for extravascular
#' administration} \item{MRTEVIFO}{mean residence time (MRT) infinity using
#' CLST, for extravascular administration} \item{MRTEVIFP}{mean residence time
#' (MRT) infinity using CLSTP, for extravascular administration}
#' \item{VZO}{volume of distribution determined by LAMZ and AUCIFO, for
#' intravascular administration} \item{VZP}{volume of distribution determined
#' by LAMZ and AUCIFP, for intravascular administration} \item{VZFO}{VZO for
#' extravascular administration, VZO/F, F is bioavailability} \item{VZFP}{VZP
#' for extravascular administration, VZP/F, F is bioavailability}
#' \item{CLO}{clearance using AUCIFO, for intravascular administration}
#' \item{CLP}{clearance using AUCIFP, for intravascular administration}
#' \item{CLFO}{CLO for extravascular administration, CLO/F, F is
#' bioavailability} \item{CLFP}{CLP for extravascular administration, CLP/F, F
#' is bioavailability} \item{VSSO}{volume of distribution at steady state using
#' CLST, for intravascular administration only} \item{VSSP}{volume of
#' distribution at stead state using CLSTP, for intravascular administration
#' only}
#' @seealso \code{\link{AUC}}, \code{\link{BestSlope}}
#' @references \enumerate{ \item Gabrielsson J, Weiner D. Pharmacokinetic and
#' Pharmacodynamic Data Analysis - Concepts and Applications. 5th ed. 2016.
#' \item Shargel L, Yu A. Applied Biopharmaceutics and Pharmacokinetics. 7th
#' ed. 2015.  \item Rowland M, Tozer TN. Clinical Pharmacokinetics and
#' Pharmacodynamics - Concepts and Applications. 4th ed. 2011. \item Gibaldi M,
#' Perrier D. Pharmacokinetics. 2nd ed. revised and expanded. 1982. }
#' @keywords NCA analysis noncompartmenal
#' @examples
#' IndiNCA(Theoph[Theoph$Subject==1,"Time"], Theoph[Theoph$Subject==1, "conc"], Dose=320)
#' IndiNCA(Indometh[Indometh$Subject==1,"time"], Indometh[Indometh$Subject==1, "conc"], Dose=25,
#'         AdmMode="Bolus")
#' IndiNCA(Indometh[Indometh$Subject==1,"time"], Indometh[Indometh$Subject==1, "conc"], Dose=25,
#'         AdmMode="Infusion", TimeInfusion=0.25)
#'
#' IndiNCA(Theoph[Theoph$Subject==1,"Time"], Theoph[Theoph$Subject==1, "conc"], Dose=320,
#'         Report="Text")
#' IndiNCA(Indometh[Indometh$Subject==1,"time"], Indometh[Indometh$Subject==1, "conc"], Dose=25,
#'         AdmMode="Bolus", Report="Text")
#' IndiNCA(Indometh[Indometh$Subject==1,"time"], Indometh[Indometh$Subject==1, "conc"], Dose=25,
#'         AdmMode="Infusion", TimeInfusion=0.25, Report="Text")
#'
#' iAUC = data.frame(Name=c("AUC[0-12h]","AUC[0-24h]"), Start=c(0,0), End=c(12,24)) ; iAUC
#' IndiNCA(Theoph[Theoph$Subject==1,"Time"], Theoph[Theoph$Subject==1, "conc"], Dose=320,
#'         iAUC=iAUC)
#' IndiNCA(Indometh[Indometh$Subject==1,"time"], Indometh[Indometh$Subject==1, "conc"], Dose=25,
#'         AdmMode="Bolus", iAUC=iAUC)
#' IndiNCA(Indometh[Indometh$Subject==1,"time"], Indometh[Indometh$Subject==1, "conc"], Dose=25,
#'         AdmMode="Infusion", TimeInfusion=0.25, iAUC=iAUC)
#' @export IndiNCA
IndiNCA <-
function(x, y, Dose=0, Method="Linear", AdmMode="Extravascular", TimeInfusion=0, RetNames, Report="Table", iAUC)
{
  n = length(x)
  if (n != length(y) | !is.numeric(x) | !is.numeric(y) | !is.numeric(Dose) | !is.numeric(TimeInfusion) | !is.character(AdmMode) | !is.character(Method)) stop("Bad Input!")
  if (AdmMode == "Infusion" & !(TimeInfusion > 0)) stop("Infusion mode should have TimeInfusion larger than 0!")

  x0 = x[1:max(which(y>0))] # Till Non-zero concentration. i.e. removing trailing zeros
  y0 = y[1:max(which(y>0))] # Till Non-zero concentration. i.e. removing trailing zeros
  x0s = x0[y0 != 0]
  y0s = y0[y0 != 0]

  colOrd = paste0(AdmMode,"Default")
  RetNames0 = RptCfg[RptCfg[,colOrd] > 0,c("PPTESTCD",colOrd)]
  RetNames = RetNames0[order(RetNames0[,colOrd]),"PPTESTCD"] ;

  if (!(Dose > 0)) RetNames = setdiff(RetNames, c("CMAXD", "AUCIFOD", "AUCIFPD"))

  if (!missing(iAUC)) {
    if (nrow(iAUC) > 0) {
      RetNames = union(RetNames, as.character(iAUC[,"Name"]))
    }
  }

  Res = vector()
  Res[c("R2", "R2ADJ", "LAMZNPT", "LAMZ", "b0", "CORRXY", "LAMZLL", "LAMZUL", "CLSTP")] = BestSlope(x0s, y0s, AdmMode)

  C0Imputed = FALSE
  if (AdmMode == "Bolus") {
    if (y[1] > y[2] & y[2] > 0) {
      C0 = exp(-x[1]*(log(y[2]) - log(y[1]))/(x[2] - x[1]) + log(y[1]))
    } else {
      C0 =  min(x[y > 0])
    }
    xa = c(0, x)
    ya = c(C0, y)
    xa0 = c(0, x0)
    ya0 = c(C0, y0)
    C0Imputed = TRUE
  } else {
    if (is.na(x[x==0][1])) {
      xa = c(0, x)
      ya = c(0, y)
      xa0 = c(0, x0)
      ya0 = c(0, y0)
      C0Imputed = TRUE
    } else {
      xa = x
      ya = y
      xa0 = x0
      ya0 = y0
    }
  }
  nxa = length(xa)
  nxa0 = length(xa0)

  tabAUC = AUC(xa0, ya0, Method=Method)
  Res[c("AUCLST","AUMCLST")] = tabAUC[nxa0,]
  Res["AUCALL"] = AUC(xa, ya, Method=Method)[nxa,1]
  Res["LAMZHL"] = log(2)/Res["LAMZ"]
  Res["TMAX"] = x[which.max(y)]
  Res["CMAX"] = max(y)
  locLast = max(which(y>0))           # Till non-zero concentration
  Res["TLST"] = x[locLast]
  Res["CLST"] = y[locLast]
  Res["AUCIFO"] = Res["AUCLST"] + Res["CLST"]/Res["LAMZ"]
  Res["AUCIFP"] = Res["AUCLST"] + Res["CLSTP"]/Res["LAMZ"]
  Res["AUCPEO"] = (1 - Res["AUCLST"]/Res["AUCIFO"])*100
  Res["AUCPEP"] = (1 - Res["AUCLST"]/Res["AUCIFP"])*100
  Res["AUMCIFO"] = Res["AUMCLST"] + Res["CLST"]*Res["TLST"]/Res["LAMZ"] + Res["CLST"]/Res["LAMZ"]/Res["LAMZ"]
  Res["AUMCIFP"] = Res["AUMCLST"] + Res["CLSTP"]*Res["TLST"]/Res["LAMZ"] + Res["CLSTP"]/Res["LAMZ"]/Res["LAMZ"]
  Res["AUMCPEO"] = (1 - Res["AUMCLST"]/Res["AUMCIFO"])*100
  Res["AUMCPEP"] = (1 - Res["AUMCLST"]/Res["AUMCIFP"])*100

  if (Dose > 0) {
    Res["CMAXD"] = Res["CMAX"] / Dose
    Res["AUCIFOD"] = Res["AUCIFO"] / Dose
    Res["AUCIFPD"] = Res["AUCIFP"] / Dose
  }

  if (AdmMode == "Bolus") {
    Res["C0"] = C0                      # Phoneix WinNonlin 6.4 User's Guide p27
    Res["AUCPBEO"] = tabAUC[2,1] / Res["AUCIFO"] * 100
    Res["AUCPBEP"] = tabAUC[2,1] / Res["AUCIFP"] * 100
  } else {
    if (sum(y0==0) > 0) Res["TLAG"] = x0[max(which(y0==0))] # Trailing zero should not exist
    else Res["TLAG"] = 0
    if (!is.na(x0[x0==0][1])) {
      if (y0[x0==0] > 0) Res["TLAG"] = 0    # This is WinNonlin logic
    }
  }

  if (AdmMode == "Extravascular") {
    Res["VZFO"] = Dose/Res["AUCIFO"]/Res["LAMZ"]
    Res["VZFP"] = Dose/Res["AUCIFP"]/Res["LAMZ"]
    Res["CLFO"] = Dose/Res["AUCIFO"]
    Res["CLFP"] = Dose/Res["AUCIFP"]
    Res["MRTEVLST"] = Res["AUMCLST"]/Res["AUCLST"]
    Res["MRTEVIFO"] = Res["AUMCIFO"]/Res["AUCIFO"]
    Res["MRTEVIFP"] = Res["AUMCIFP"]/Res["AUCIFP"]
  } else {
    Res["VZO"] = Dose/Res["AUCIFO"]/Res["LAMZ"]
    Res["VZP"] = Dose/Res["AUCIFP"]/Res["LAMZ"]
    Res["CLO"] = Dose/Res["AUCIFO"]
    Res["CLP"] = Dose/Res["AUCIFP"]
    Res["MRTIVLST"] = Res["AUMCLST"]/Res["AUCLST"] - TimeInfusion/2
    Res["MRTIVIFO"] = Res["AUMCIFO"]/Res["AUCIFO"] - TimeInfusion/2
    Res["MRTIVIFP"] = Res["AUMCIFP"]/Res["AUCIFP"] - TimeInfusion/2
    Res["VSSO"] = Res["MRTIVIFO"] * Res["CLO"]
    Res["VSSP"] = Res["MRTIVIFP"] * Res["CLP"]
  }

  if (!missing(iAUC)) {
    for (i in 1:nrow(iAUC)) {
      if (AdmMode == "Bolus") Res[as.character(iAUC[i,"Name"])] = IntAUC(xa, ya, iAUC[i,"Start"], iAUC[i,"End"], Res, Method=Method)
      else Res[as.character(iAUC[i,"Name"])] = IntAUC(x, y, iAUC[i,"Start"], iAUC[i,"End"], Res, Method=Method)
    }
  }

  if (Report == "Table") {
    Result = Res[RetNames]
  } else if (Report == "Text") {

 # Begin Making Summary Table
    iL = which(xa0==Res["LAMZLL"])
    iU = which(xa0==Res["LAMZUL"])
    xr0 = xa0[iL:iU]
    yr0 = ya0[iL:iU]
    ypr = exp(Res["b0"] - Res["LAMZ"]*xr0)
    yre = yr0 - ypr
 # End Making Summary Table
    DateTime = strsplit(as.character(Sys.time())," ")[[1]]

    Result = vector()
    cLineNo = 1
    Result[cLineNo] = paste("                        NONCOMPARTMENTAL ANALYSIS REPORT") ; cLineNo = cLineNo + 1
    Result[cLineNo] = paste0("                       Package version ", packageVersion("ncar"), " (", packageDescription("ncar")$Date, ")") ; cLineNo = cLineNo + 1
    Result[cLineNo] = paste("                         ", version$version.string) ; cLineNo = cLineNo + 1
    Result[cLineNo] = "" ; cLineNo = cLineNo + 1
    Result[cLineNo] = paste("Date and Time:", Sys.time(), Sys.timezone(location=FALSE)) ; cLineNo = cLineNo + 1
    Result[cLineNo] = "" ; cLineNo = cLineNo + 1
    Result[cLineNo] = "Calculation Setting" ; cLineNo = cLineNo + 1
    Result[cLineNo] = "-------------------" ; cLineNo = cLineNo + 1
    if (AdmMode == "Bolus") { Adm = "Bolus IV" }
    else if (AdmMode == "Infusion") { Adm = "Constant Infusion" }
    else { Adm = "Extravascular" }
    Result[cLineNo] = paste("Drug Administration:", Adm) ; cLineNo = cLineNo + 1
    Result[cLineNo] = paste("Observation count excluding trailing zero:", length(x0)) ; cLineNo = cLineNo + 1
    Result[cLineNo] = paste("Dose at time 0:", Dose) ; cLineNo = cLineNo + 1
    if (AdmMode == "Infusion") {
      Result[cLineNo] = paste("Length of Infusion:", TimeInfusion) ; cLineNo = cLineNo + 1
    }
    if (Method == "Linear") {
      Result[cLineNo] = "AUC Calculation method: Linear-up Linear-down method" ; cLineNo = cLineNo + 1
    } else if (Method == "Log") {
      Result[cLineNo] = "AUC Calculation method: Linear-up Log-down method" ; cLineNo = cLineNo + 1
    } else {
      Result[cLineNo] = paste("AUC Calculation method: Unknown") ; cLineNo = cLineNo + 1
    }
    Result[cLineNo] = "Weighting for lambda z: Uniform (Ordinary Least Square, OLS)" ; cLineNo = cLineNo + 1
    Result[cLineNo] = "Lambda z selection criterion: Heighest adjusted R-squared value with precision=1e-4" ; cLineNo = cLineNo + 1
    Result[cLineNo] = "" ; cLineNo = cLineNo + 1
    Result[cLineNo] = "" ; cLineNo = cLineNo + 1
    Result[cLineNo] = "Fitting, AUC, AUMC Result" ; cLineNo = cLineNo + 1
    Result[cLineNo] = "-------------------------" ; cLineNo = cLineNo + 1
    Result[cLineNo] = "      Time         Conc.      Pred.   Residual       AUC       AUMC      Weight" ; cLineNo = cLineNo + 1
    Result[cLineNo] = "-------------------------------------------------------------------------------" ; cLineNo = cLineNo + 1
    for (i in 1:length(xa0)) {
      Str = sprintf("%11.4f", Round(xa0[i],4))
      if (C0Imputed & i == 1) { Str = paste(Str, "+") }
      else if (i >= iL & i <= iU) { Str = paste(Str, "*") }
      else { Str = paste(Str, " ") }
      Str = paste(Str, sprintf("%10.4f", Round(ya0[i], 4)))
      if (i >= iL & i <= iU) { Str = paste(Str, sprintf("%10.4f", Round(ypr[i - iL + 1], 4))) }
      else { Str = paste(Str, "          ") }
      if (i >= iL & i <= iU) { Str = paste(Str, sprintf("%+10.3e", yre[i - iL + 1])) }
      else { Str = paste(Str, "          ") }
      Str = paste(Str, sprintf("%10.4f", Round(tabAUC[i,1], 4)))
      Str = paste(Str, sprintf("%10.4f", Round(tabAUC[i,2], 4)))
      if (i >= iL & i <= iU) { Str = paste(Str, sprintf("%10.4f",Round(1, 4))) }
      else { Str = paste(Str, "           ") }
      Result[cLineNo] = Str ; cLineNo = cLineNo + 1
    }
    Result[cLineNo] = "" ; cLineNo = cLineNo + 1
    if (C0Imputed) {
      Result[cLineNo] = "+: Back extrapolated concentration" ; cLineNo = cLineNo + 1
    }
    Result[cLineNo] = "*: Used for the calculation of Lambda z." ; cLineNo = cLineNo + 1
    Result[cLineNo] = "" ; cLineNo = cLineNo + 1
    Result[cLineNo] = "" ; cLineNo = cLineNo + 1
    Result[cLineNo] = "Calculated Values" ; cLineNo = cLineNo + 1
    Result[cLineNo] = "-----------------" ; cLineNo = cLineNo + 1
    for (i in 1:length(RetNames)) {
      SYNO = RptCfg[RptCfg$PPTESTCD==RetNames[i],"SYNONYM"]
      if (RetNames[i] == "LAMZNPT") {
        Result[cLineNo] = paste(sprintf("%-10s", RetNames[i]), sprintf("%-40s", SYNO), sprintf("%8d", Round(Res[RetNames[i]], 4))) ; cLineNo = cLineNo + 1
      } else {
        Result[cLineNo] = paste(sprintf("%-10s", RetNames[i]), sprintf("%-40s", SYNO), sprintf("%13.4f", Round(Res[RetNames[i]], 4))) ; cLineNo = cLineNo + 1
      }
    }
  } else {
    stop("Unknown Report Type!")
  }

  return(Result)
}
