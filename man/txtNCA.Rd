\name{txtNCA}
\alias{txtNCA}
\title{Text output of NCA}
\description{This output NCA result in text form.}
\usage{
txtNCA(x, y, dose = 0, adm = "Extravascular", dur = 0, doseUnit = "mg", timeUnit = "h", 
       concUnit = "ug/L", iAUC = "", down="Linear", MW = 0, returnNA = FALSE)
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
  \item{MW}{molecular weight of the drug}
  \item{returnNA}{if \code{returnNA} is \code{TRUE}, it returns NA values also.}
}
\value{Basically same with \code{\link{sNCA}}}
\author{Kyun-Seop Bae <k@acr.kr>}
\seealso{
\code{\link{help}}, \code{\link{sNCA}}, \code{\link{tabNCA}}, \code{\link{pdfNCA}}, \code{\link{rtfNCA}} 
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
                dose=320, concUnit="mg/L", returnNA=FALSE)
  tRes = c(paste("ID =", IDs[i]), tRes, "")
  Res = c(Res, tRes)
}
Res
}
\keyword{Output Form}
