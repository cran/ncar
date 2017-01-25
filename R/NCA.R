#' Noncompartmental analysis for a dataset with multiple subjects
#'
#' conduct noncompartmental analysis for many subjects in a data table
#'
#' This function calls \code{IndiNCA} repeatedly to do NCA for each subject. If
#' you specify \code{Report="Text"}, this function returns in free text format
#' to be used in a report file.
#'
#' @param Data name of data table containing time-concentration data of
#' multiple subjects
#' @param colSubj column name for subject ID
#' @param colTime column name for the time
#' @param colConc column name for the concentration
#' @param colTrt column name for the treatment code. This is useful for
#' crossover study like bioequivalence trial.
#' @param Method one of \code{"Linear"} or \code{"Log"} to indicate the way to
#' calculate AUC
#' @param Dose administered dose. One should be careful for the unit. This can
#' be a vector containing dose for each subject in order.
#' @param AdmMode one of \code{"Bolus"} or \code{"Infusion"} or
#' \code{"Extravascular"} to indicate drug administration mode
#' @param TimeInfusion infusion duration for constant infusion, otherwise 0.
#' This can be a vector containing values for each subject in order.
#' @param Report either of \code{"Table"} or \code{"Text"} to specify the type
#' of return value
#' @param iAUC data.frame with three columns, "Name", "Start", "End" to specify
#' partial interval AUC
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
#' @seealso \code{\link{IndiNCA}}
#' @references \enumerate{ \item Gabrielsson J, Weiner D. Pharmacokinetic and
#' Pharmacodynamic Data Analysis - Concepts and Applications. 5th ed. 2016.
#' \item Shargel L, Yu A. Applied Biopharmaceutics and Pharmacokinetics. 7th
#' ed. 2015.  \item Rowland M, Tozer TN. Clinical Pharmacokinetics and
#' Pharmacodynamics - Concepts and Applications. 4th ed. 2011. \item Gibaldi M,
#' Perrier D. Pharmacokinetics. 2nd ed. revised and expanded. 1982. }
#' @keywords NCA
#' @examples
#' # Theoph and Indometh data: dose in mg, conc in mg/L, time in h
#' NCA(Theoph, "Subject", "Time", "conc", Dose=320)
#' NCA(Indometh, "Subject", "time", "conc", Dose=25, AdmMode="Bolus")
#'
#' iAUC = data.frame(Name=c("AUC[0-12h]","AUC[0-24h]"), Start=c(0,0), End=c(12,24)) ; iAUC
#' NCA(Theoph, "Subject", "Time", "conc", Dose=320, iAUC=iAUC)
#' NCA(Indometh, "Subject", "time", "conc", Dose=25, AdmMode="Bolus", iAUC=iAUC)
#'
#' writeLines(NCA(Theoph, "Subject", "Time", "conc", Dose=320, Report="Text"),
#'            "Theoph_Linear_CoreOutput.txt")
#' writeLines(NCA(Theoph, "Subject", "Time", "conc", Dose=320, Method="Log", Report="Text"),
#'            "Theoph_Log_CoreOutput.txt")
#' writeLines(NCA(Indometh, "Subject", "time", "conc", Dose=25, AdmMode="Bolus", Report="Text"),
#'            "Indometh_Bolus_Linear_CoreOutput.txt")
#' writeLines(NCA(Indometh, "Subject", "time", "conc", Dose=25, AdmMode="Bolus", Method="Log",
#'            Report="Text"), "Indometh_Bolus_Log_CoreOutput.txt")
#' writeLines(NCA(Indometh, "Subject", "time", "conc", Dose=25, AdmMode="Infusion", TimeInfusion=0.25,
#'            Report="Text"), "Indometh_Infusion_Linear_CoreOutput.txt")
#' writeLines(NCA(Indometh, "Subject", "time", "conc", Dose=25, AdmMode="Infusion", TimeInfusion=0.25,
#'            Method="Log", Report="Text"), "Indometh_Infusion_Log_CoreOutput.txt")
#' @export NCA
NCA <-
function(Data, colSubj, colTime, colConc, colTrt, Method="Linear", Dose=0, AdmMode="Extravascular", TimeInfusion=0, Report="Table", iAUC)
{
  if (!is.numeric(Dose) | !is.numeric(TimeInfusion) | !is.character(AdmMode) | !is.character(Method)) stop("Bad Input!")

  colOrd = paste0(AdmMode, "Default")
  ColName00 = RptCfg[RptCfg[,colOrd] > 0, c("PPTESTCD", colOrd)]
  ColName0 = ColName00[order(ColName00[, colOrd]), "PPTESTCD"] ;

  if (!(max(Dose) > 0)) ColName0= setdiff(ColName0, c("CMAXD", "AUCIFOD", "AUCIFPD"))

  if (!missing(iAUC)) {
    ColName0 = union(ColName0, as.character(iAUC[,"Name"]))
  }

  SUBJIDs = unique(as.character(Data[,colSubj]))
  nSUBJID = length(SUBJIDs)

  if (length(Dose) > 1 & length(Dose) != nSUBJID) stop("Dose should be fixed or given for each subject!")
  if (length(TimeInfusion) > 1 & length(TimeInfusion) != nSUBJID) stop("TimeInfusion should be fixed or given for each subject!")

  if (missing(colTrt)) {
    Res0 = data.frame(SUBJID=character(), stringsAsFactors=FALSE)
    if (Report == "Table") {
      Result = data.frame()
    } else {
      Result = vector()
    }
    for (i in 1:nSUBJID) {
      cSUBJID = SUBJIDs[i]
      Dat = Data[Data[,colSubj]==cSUBJID,]
      if (nrow(Dat) > 0) {
        x = as.numeric(Dat[,colTime])
        y = as.numeric(Dat[,colConc])
        if (length(Dose) > 1) {
          cDose = Dose[i]
        } else {
          cDose = Dose
        }
        if (length(TimeInfusion) > 1) {
          cTimeInfusion = TimeInfusion[i]
        } else {
          cTimeInfusion = TimeInfusion
        }
        if (AdmMode == "Infusion" & !(cTimeInfusion > 0)) stop("Infusion mode should have TimeInfusion larger than 0!")

        Res0 = rbind(Res0, data.frame(cSUBJID, stringsAsFactors=FALSE))
        if (!missing(iAUC)) {
          cResult = IndiNCA(x, y, Method=Method, Dose=cDose, AdmMode=AdmMode, TimeInfusion=cTimeInfusion, RetNames=ColName0, Report=Report, iAUC=iAUC)
        } else {
          cResult = IndiNCA(x, y, Method=Method, Dose=cDose, AdmMode=AdmMode, TimeInfusion=cTimeInfusion, RetNames=ColName0, Report=Report)
        }
        if (Report == "Table") {
          Result = rbind(Result, cResult)
        } else {
          Result = c(Result, "NCA REPORT", paste0("Subject=", cSUBJID), "", cResult, "", "")
        }
      }
    }
    if (Report == "Table") {
      Result = cbind(Res0, Result)
      colnames(Result) = c(colSubj, ColName0)
    }
  } else {
    TRTs = sort(unique(as.character(Data[,colTrt])))
    nTRT = length(TRTs)
    Res0 = data.frame(SUBJID=character(), TRT=character(), stringsAsFactors=FALSE)
    if (Report == "Table") {
      Result = data.frame()
    } else {
      Result = vector()
    }
    for (i in 1:nSUBJID) {
      for (j in 1:nTRT) {
        cSUBJID = SUBJIDs[i]
        cTRT = TRTs[j]
        Dat = Data[Data[,colSubj]==cSUBJID & Data[,colTrt]==cTRT,]
        if (nrow(Dat) > 0) {
          x = as.numeric(Dat[,colTime])
          y = as.numeric(Dat[,colConc])
          Res0 = rbind(Res0, data.frame(cSUBJID, cTRT, stringsAsFactors=FALSE))
          if (!missing(iAUC)) {
            cResult = IndiNCA(x, y, Method=Method, Dose=Dose, AdmMode=AdmMode, TimeInfusion=TimeInfusion, RetNames=ColName0, Report=Report, iAUC=iAUC)
          } else {
            cResult = IndiNCA(x, y, Method=Method, Dose=Dose, AdmMode=AdmMode, TimeInfusion=TimeInfusion, RetNames=ColName0, Report=Report)
          }
          if (Report == "Table") {
            Result = rbind(Result, cResult)
          } else {
            Result = c(Result, "NCA REPORT", paste0("Subject=", cSUBJID), paste0("Treatment=", cTRT), cResult, "", "")
          }
        }
      }
    }
    if (Report == "Table") {
      Result = cbind(Res0, Result)
      colnames(Result) = c(colSubj, colTrt, ColName0)
    }
  }

  return(Result)
}
