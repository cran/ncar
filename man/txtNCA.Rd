\name{txtNCA}
\alias{txtNCA}
\title{Text output of NCA for one subject}
\description{This is the text form output.}
\usage{
txtNCA(x, y, dose = 0, adm = "Extravascular", dur = 0, doseUnit = "mg", 
       timeUnit = "h", concUnit = "ug/L", iAUC = "", down="Linear", R2ADJ=0, 
       MW = 0, SS = FALSE, excludeDelta = 1)
}
\arguments{
  \item{x}{usually time}
  \item{y}{usually concentration}
  \item{dose}{given amount}
  \item{adm}{one of \code{"Bolus"} or \code{"Infusion"} or \code{"Extravascular"} to indicate drug administration mode}
  \item{dur}{duration of infusion}
  \item{doseUnit}{unit of dose}
  \item{timeUnit}{unit of time}
  \item{concUnit}{unit of concentration}
  \item{iAUC}{interval AUCs to calculate}
  \item{down}{either of \code{"Linear"} or \code{"Log"} to indicate the way to calculate AUC and AUMC}
  \item{R2ADJ}{Minimum adjusted R-square value to determine terminal slope automatically}
  \item{MW}{molecular weight of the drug}
  \item{SS}{if steady-state, this should be TRUE. AUCLST (AUClast) is used instead of AUCIFO (AUCinf) for the calculation of Vz (VZFO, VZO), CL (CLFO, CLO), and Vdss (VSSO).}
  \item{excludeDelta}{Improvement of R2ADJ larger than this value could exclude the last point. Default value 1 is for the compatibility with other software. Author recommends to use \code{excludeDelta} option with about 0.3.}
}
\value{
  \item{CMAX}{maximum concentration, Cmax}
  \item{CMAXD}{dose normalized Cmax, CMAX / Dose, Cmax / Dose}
  \item{TMAX}{time of maximum concentration, Tmax}
  \item{TLAG}{time to observe the first non-zero concentration, for extravascular administration only}
  \item{CLST}{last positive concentration observed, Clast}
  \item{CLSTP}{last positive concentration predicted, Clast_pred}
  \item{TLST}{time of last positive concentration, Tlast}
  \item{LAMZHL}{half-life by lambda z, ln(2)/LAMZ}
  \item{LAMZ}{lambda_z negative of best fit terminal slope}
  \item{LAMZLL}{earliest time for LAMZ}
  \item{LAMZUL}{last time for LAMZ}
  \item{LAMZNPT}{number of points for LAMZ}
  \item{CORRXY}{correlation of log(concentration) and time}
  \item{R2}{R-squared}
  \item{R2ADJ}{R-squared adjusted}
  \item{C0}{back extrapolated concentration at time 0, for bolus intravascular administration only}
  \item{AUCLST}{AUC from 0 to TLST}
  \item{AUCALL}{AUC using all the given points, including trailing zero concentrations}
  \item{AUCIFO}{AUC infinity observed}
  \item{AUCIFOD}{AUCIFO / Dose}
  \item{AUCIFP}{AUC infinity predicted using CLSTP instead of CLST}
  \item{AUCIFPD}{AUCIFP / Dose}
  \item{AUCPEO}{AUC \% extrapolation observed}
  \item{AUCPEP}{AUC \% extrapolated for AUCIFP}
  \item{AUCPBEO}{AUC \% back extrapolation observed, for bolus IV administration only}
  \item{AUCPBEP}{AUC \% back extrapolation predicted with AUCIFP, for bolus IV administration only}
  \item{AUMCLST}{AUMC to the TLST}
  \item{AUMCIFO}{AUMC infinity observed using CLST}
  \item{AUMCIFP}{AUMC infinity determined by CLSTP}
  \item{AUMCPEO}{AUMC \% extrapolated observed}
  \item{AUMCPEP}{AUMC \% extrapolated predicted}
  \item{MRTIVLST}{mean residence time (MRT) to TLST, for intravascular administration}
  \item{MRTIVIFO}{mean residence time (MRT) infinity using CLST, for intravascular administration}
  \item{MRTIVIFP}{mean residence time (MRT) infinity using CLSTP, for intravascular administration}
  \item{MRTEVLST}{mean residence time (MRT) to TLST, for extravascular administration}
  \item{MRTEVIFO}{mean residence time (MRT) infinity using CLST, for extravascular administration}
  \item{MRTEVIFP}{mean residence time (MRT) infinity using CLSTP, for extravascular administration}
  \item{VZO}{volume of distribution determined by LAMZ and AUCIFO, for intravascular administration}
  \item{VZP}{volume of distribution determined by LAMZ and AUCIFP, for intravascular administration}
  \item{VZFO}{VZO for extravascular administration, VZO/F, F is bioavailability}
  \item{VZFP}{VZP for extravascular administration, VZP/F, F is bioavailability}
  \item{CLO}{clearance using AUCIFO, for intravascular administration}
  \item{CLP}{clearance using AUCIFP, for intravascular administration}
  \item{CLFO}{CLO for extravascular administration, CLO/F, F is bioavailability}
  \item{CLFP}{CLP for extravascular administration, CLP/F, F is bioavailability}
  \item{VSSO}{volume of distribution at steady state using CLST, for intravascular administration only}
  \item{VSSP}{volume of distribution at stead state using CLSTP, for intravascular administration only}
}
\author{Kyun-Seop Bae <k@acr.kr>}
\seealso{
\code{\link{help}}, \code{\link{pdfNCA}}, \code{\link{rtfNCA}} 
}
\examples{
# For one subject
txtNCA(Theoph[Theoph$Subject=="1","Time"], Theoph[Theoph$Subject=="1","conc"], 
       dose=320, doseUnit="mg", concUnit="mg/L", timeUnit="h")

# or equivalently
x = Theoph[Theoph$Subject=="1","Time"]
y = Theoph[Theoph$Subject=="1","conc"]
txtNCA(x, y, dose=320, doseUnit="mg", concUnit="mg/L", timeUnit="h")

# For all subjects
IDs = sort(as.numeric(unique(Theoph[,"Subject"])))
nID = length(IDs)
Res = vector()
for (i in 1:nID) {
  tRes = txtNCA(Theoph[Theoph[,"Subject"]==IDs[i],"Time"], 
                Theoph[Theoph[,"Subject"]==IDs[i],"conc"], 
                dose=320, concUnit="mg/L")
  tRes = c(paste("ID =", IDs[i]), tRes, "")
  Res = c(Res, tRes)
}
Res
}
\keyword{Output Form}
