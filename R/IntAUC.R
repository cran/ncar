#' Calculate interval AUC
#'
#' It calculates interval AUC
#'
#' This calculates an interval (partial) AUC (from t1 to t2) with the given
#' series of x and y. If t1 and/or t2 cannot be found within x vector, it
#' interpolates according to the \code{Method}.
#'
#' @param x vector values of independent variable, usually time
#' @param y vector values of dependent variable, usually concentration
#' @param t1 start time for AUC
#' @param t2 end time for AUC
#' @param Res result from \code{IndiNCA} function
#' @param Method either of \code{"Linear"} or \code{"Log"} to indicate the way
#' to calculate AUC
#' @return return interval AUC value (scalar)
#' @seealso \code{\link{AUC}}, \code{\link{Interpol}}
#' @references \enumerate{ \item Gabrielsson J, Weiner D. Pharmacokinetic and
#' Pharmacodynamic Data Analysis - Concepts and Applications. 5th ed. 2016.
#' \item Shargel L, Yu A. Applied Biopharmaceutics and Pharmacokinetics. 7th
#' ed. 2015.  \item Rowland M, Tozer TN. Clinical Pharmacokinetics and
#' Pharmacodynamics - Concepts and Applications. 4th ed. 2011. \item Gibaldi M,
#' Perrier D. Pharmacokinetics. 2nd ed. revised and expanded. 1982. }
#' @keywords AUC interval partial
#' @examples
#' Res = IndiNCA(Theoph[Theoph$Subject==1,"Time"], Theoph[Theoph$Subject==1, "conc"], Dose=320)
#' IntAUC(Theoph[Theoph$Subject==1, "Time"], Theoph[Theoph$Subject==1, "conc"], t1=0.5, t2=11, Res)
#' @export IntAUC
IntAUC <-
function(x, y, t1, t2, Res, Method="Linear")
{
  n = length(x)
  if (n != length(y) | !is.numeric(x) | !is.numeric(y)) stop("Bad Input!")
  if (t1 > Res["TLST"]) stop("Start time of interval AUC is after Tlast.")

  tL = Res["TLST"]
  if (t2 > tL & is.na(Res["LAMZ"])) return(NA)

  newSeries = Interpol(x, y, t1, Res["LAMZ"], Res["b0"], Method=Method)
  newSeries = Interpol(newSeries[[1]], newSeries[[2]], t2, Res["LAMZ"], Res["b0"], Method=Method)
  x = newSeries[[1]]
  y = newSeries[[2]]

  if (Method=="Linear") {
    if (t2 <= tL) {
      ResIntAUC = LinAUC(x[x>=t1 & x<=t2], y[x>=t1 & x<=t2])[[1]]
    } else {
      ResIntAUC = LinAUC(x[x>=t1 & x<=tL], y[x>=t1 & x<=tL])[[1]] + LogAUC(x[x>=tL & x<=t2], y[x>=tL & x<=t2])[[1]]
    }
  } else if (Method=="Log") {
    ResIntAUC = LogAUC(x[x>=t1 & x<=t2], y[x>=t1 & x<=t2])[[1]]
  } else stop("Unknown Method!")

  return(ResIntAUC)
}
