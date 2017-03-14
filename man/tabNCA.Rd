\name{tabNCA}
\alias{tabNCA}
\title{Table output NCA}
\description{This output NCA result to table form.}
\usage{
tabNCA(concData, colSubj = "Subject", colTime = "Time", colConc = "conc", dose = 0, 
       adm = "Extravascular", dur = 0, doseUnit = "mg", timeUnit = "h", 
       concUnit = "ug/L", down = "Linear", MW = 0, returnNA = FALSE)
}
\arguments{
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
  \item{down}{method to calculate AUC, \code{"Linear"} or \code{"Log"}}
  \item{MW}{molecular weight of drug}
  \item{returnNA}{if \code{returnNA} is \code{TRUE}, it returns NA values also.}
}
\value{Basically same with \code{\link{sNCA}}}
\author{Kyun-Seop Bae <k@acr.kr>}
\seealso{
\code{\link{help}}, \code{\link{sNCA}}, \code{\link{txtNCA}}, \code{\link{pdfNCA}}, \code{\link{rtfNCA}} 
}
\examples{
tabNCA(Theoph, dose=320, concUnit="mg/L")
tabNCA(Indometh, colSubj="Subject", colTime="time", colConc="conc", dose=25, 
       adm="Infusion", dur=0.5, concUnit="mg/L")
}
\keyword{Output Form}
