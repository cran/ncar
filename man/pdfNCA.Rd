\name{pdfNCA}
\alias{pdfNCA}
\title{NCA output to pdf file}
\description{This output NCA result in a pdf file.}
\usage{
pdfNCA(fileName = "Temp-NCA.pdf", concData, colSubj = "Subject", colTime = "Time", 
       colConc = "conc", dose = 0, adm = "Extravascular", dur = 0, doseUnit = "mg", 
       timeUnit = "h", concUnit = "ug/L", down="Linear", MW = 0)
}
\arguments{
  \item{fileName}{file name to save}
  \item{concData}{concentration data table}
  \item{colSubj}{column name for subject ID}
  \item{colTime}{column name for time}
  \item{colConc}{column name for concentration}
  \item{dose}{administered dose}
  \item{adm}{one of \code{"Bolus"} or \code{"Infusion"} or \code{"Extravascular"} to indicate drug administration mode}
  \item{dur}{duration of infusion}
  \item{doseUnit}{unit of dose}
  \item{timeUnit}{unit of time}
  \item{concUnit}{unit of concentration}
  \item{down}{either of \code{"Linear"} or \code{"Log"} to indicate the way to calculate AUC and AUMC}
  \item{MW}{molecular weight of drug}
}
\value{Basically same with \code{\link{sNCA}}}
\author{Kyun-Seop Bae <k@acr.kr>}
\seealso{
\code{\link{help}}, \code{\link{sNCA}}, \code{\link{txtNCA}}, \code{\link{tabNCA}}, \code{\link{rtfNCA}} 
}
\examples{
#pdfNCA(fileName="NCA-Theoph.pdf", Theoph, colSubj="Subject", colTime="Time", 
#       colConc="conc", dose=320, doseUnit="mg", timeUnit="h", concUnit="mg/L")
#pdfNCA(fileName="NCA-Indometh.pdf", Indometh, colSubj="Subject", colTime="time", 
#       colConc="conc", adm="Infusion", dur=0.5, dose=25, doseUnit="mg", 
#       timeUnit="h", concUnit="mg/L")
}
\keyword{Output Form}
