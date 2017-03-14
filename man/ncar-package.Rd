\name{ncar-package}
\alias{ncar-package}
\alias{ncar}
\docType{package}
\title{Noncompartmental Analysis for Pharmacokinetic Data for Reviewer}
\description{
It conducts a noncompartmental analysis(NCA) as closely as possible to the most widely used commercial pharmacokinetic analysis software.
}
\details{
The main functions are
\preformatted{
NCA	     to perform NCA for many subjects.

IndiNCA  to perform NCA for one subject.
}
}
\author{Kyun-Seop Bae <k@acr.kr>}

\references{
\enumerate{
\item Gabrielsson J, Weiner D. Pharmacokinetic and Pharmacodynamic Data Analysis - Concepts and Applications. 5th ed. 2016.
\item Shargel L, Yu A. Applied Biopharmaceutics and Pharmacokinetics. 7th ed. 2015. 
\item Rowland M, Tozer TN. Clinical Pharmacokinetics and Pharmacodynamics - Concepts and Applications. 4th ed. 2011.
\item Gibaldi M, Perrier D. Pharmacokinetics. 2nd ed. revised and expanded. 1982.
}
}
\keyword{ package }
\keyword{ NCA }
\examples{
# Theoph and Indometh data: dose in mg, conc in mg/L, time in h
tabNCA(Theoph, dose=320, concUnit="mg/L")
tabNCA(Indometh, colSubj="Subject", colTime="time", colConc="conc", dose=25,
       adm="Infusion", dur=0.5, concUnit="mg/L")

tabNCA(Theoph, dose=320, concUnit="mg/L")
tabNCA(Indometh, colSubj="Subject", colTime="time", colConc="conc", dose=25, 
       adm="Infusion", dur=0.5, concUnit="mg/L")

# Output to PDF file
#pdfNCA(fileName="NCA-Theoph.pdf", Theoph, colSubj="Subject", colTime="Time", 
#       colConc="conc", dose=320, doseUnit="mg", timeUnit="h", concUnit="mg/L")
#pdfNCA(fileName="NCA-Indometh.pdf", Indometh, colSubj="Subject", colTime="time", 
#       colConc="conc", adm="Infusion", dur=0.5, dose=25, doseUnit="mg", 
#       timeUnit="h", concUnit="mg/L")

# Output to RTF file
#rtfNCA(fileName="NCA-Theoph.rtf", Theoph, colSubj="Subject", colTime="Time", 
#       colConc="conc", dose=320, doseUnit="mg", timeUnit="h", concUnit="mg/L")
#rtfNCA(fileName="NCA-Indometh.rtf", Indometh, colSubj="Subject", colTime="time", 
#       colConc="conc", adm="Infusion", dur=0.5, dose=25, doseUnit="mg", 
#       timeUnit="h", concUnit="mg/L")

# For one subject
iAUC = data.frame(Name=c("AUC[0-12h]","AUC[0-24h]"), Start=c(0,0), End=c(12,24)) ; iAUC

x = Theoph[Theoph$Subject=="1","Time"]
y = Theoph[Theoph$Subject=="1","conc"]

sNCA(x, y, dose=320, doseUnit="mg", concUnit="mg/L", timeUnit="h", iAUC=iAUC)
sNCA(x, y, dose=320, concUnit="mg/L", returnNA=FALSE, iAUC=iAUC)

txtNCA(x, y, dose=320, doseUnit="mg", concUnit="mg/L", timeUnit="h", iAUC=iAUC)
}
