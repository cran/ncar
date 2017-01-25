#' Area Under the Curve(AUC) and Area Under the first Moment Curve(AUMC) by
#' linear trapezoidal method
#'
#' It calculates AUC and AUMC using linear trapezoidal method
#'
#' This function returns AUC and AUMC by linear trapezoidal method.
#'
#' @param x vector values of independent variable, usually time
#' @param y vector values of dependent variable, usually concentration
#' @return \item{AUC}{area under the curve} \item{AUMC}{area under the first
#' moment curve}
#' @seealso \code{\link{LogAUC}},\code{\link{AUC}}
#' @references \enumerate{ \item Gabrielsson J, Weiner D. Pharmacokinetic and
#' Pharmacodynamic Data Analysis - Concepts and Applications. 5th ed. 2016.
#' \item Shargel L, Yu A. Applied Biopharmaceutics and Pharmacokinetics. 7th
#' ed. 2015.  \item Rowland M, Tozer TN. Clinical Pharmacokinetics and
#' Pharmacodynamics - Concepts and Applications. 4th ed. 2011. \item Gibaldi M,
#' Perrier D. Pharmacokinetics. 2nd ed. revised and expanded. 1982. }
#' @keywords AUC
#' @examples
#' LinAUC(Theoph[Theoph$Subject==1, "Time"],Theoph[Theoph$Subject==1, "conc"])
#' AUC(Theoph[Theoph$Subject==1, "Time"],Theoph[Theoph$Subject==1, "conc"]) # compare the last line
#' @export LinAUC
LinAUC <-
function(x, y) # Method="Linear"
{
  n = length(x)
  if (n != length(y) | !is.numeric(x) | !is.numeric(y)) stop("Bad Input!")

  auc = sum((x[-1] - x[-n])*(y[-1] + y[-n]))/2
  aumc = sum((x[-1] - x[-n])*(x[-1]*y[-1] + x[-n]*y[-n]))/2
  Result = c(auc, aumc)
  names(Result) = c("AUC", "AUMC")
  return(Result)
}
