#' NCA Report Configuation Table
#'
#' Contains the names and order of colum of return table/text by \code{IndiNCA}
#' and \code{NCA} functions
#'
#' This table should exist in ncar package. User can edit this table for
#' shaping the report in one's own style.
#'
#' @name RptCfg
#' @docType data
#' @format A data frame with 48 observations on the following 10 variables.
#' \describe{ \item{PPTESTCD}{a character vector of CDISC SDTM
#' PPTESTCD} \item{SYNONYM}{a character vector of CDISC SDTM PPTESTCD
#' Synonym} \item{NCI}{a character vector of NCI peferred terms}
#' \item{WNL}{a character vector of WinNonlin(R) software variables}
#' \item{ExtravascularDefault}{a numeric vector of ordering in report
#' for extravascular administration, Zero means exclusion in the report.}
#' \item{ExtravascularWNL}{a numeric vector of WinNonlin(R) style
#' ordering in report for extravascular administration, Zero means exclusion in
#' the report.} \item{BolusDefault}{a numeric vector of ordering in
#' report for extravascular administration, Zero means exclusion in the
#' report.} \item{BolusWNL}{a numeric vector of WinNonlin(R) style
#' ordering in report for extravascular administration, Zero means exclusion in
#' the report.} \item{InfusionDefault}{a numeric vector of ordering in
#' report for extravascular administration, Zero means exclusion in the
#' report.} \item{InfusionWNL}{a numeric vector of WinNonlin(R) style
#' ordering in report for extravascular administration, Zero means exclusion in
#' the report.} }
#' @keywords dataset
#' @usage data(RptCfg)
NULL
