\name{RptCfg}
\alias{RptCfg}
\docType{data}
\title{
NCA Report Configuation Table
}
\description{
Contains the names and order of colum of return table/text by \code{IndiNCA} and \code{NCA} functions 
}
\usage{RptCfg}
\format{
  A data frame with 48 observations on the following 10 variables.
  \describe{
    \item{\code{PPTESTCD}}{a character vector of CDISC SDTM PPTESTCD}
    \item{\code{SYNONYM}}{a character vector of CDISC SDTM PPTESTCD Synonym}
    \item{\code{NCI}}{a character vector of NCI peferred terms}
    \item{\code{WNL}}{a character vector of WinNonlin(R) software variables}
    \item{\code{ExtravascularDefault}}{a numeric vector of ordering in report for extravascular administration, Zero means exclusion in the report.}
    \item{\code{ExtravascularWNL}}{a numeric vector of WinNonlin(R) style ordering in report for extravascular administration, Zero means exclusion in the report.}
    \item{\code{BolusDefault}}{a numeric vector of ordering in report for extravascular administration, Zero means exclusion in the report.}
    \item{\code{BolusWNL}}{a numeric vector of WinNonlin(R) style ordering in report for extravascular administration, Zero means exclusion in the report.}
    \item{\code{InfusionDefault}}{a numeric vector of ordering in report for extravascular administration, Zero means exclusion in the report.}
    \item{\code{InfusionWNL}}{a numeric vector of WinNonlin(R) style ordering in report for extravascular administration, Zero means exclusion in the report.}
  }
}
\details{
This table should exist in NonCompart package.
User can edit this table for shaping the report in one's own style. 
}

\keyword{datasets}
