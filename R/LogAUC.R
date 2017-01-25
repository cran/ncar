#' Area Under the Curve(AUC) and Area Under the first Moment Curve(AUMC) by
#' linear-up log-down method
#'
#' It calculates AUC and AUMC using linear-up log-down method
#'
#' This function returns AUC and AUMC by linear-up log-down method.
#'
#' @param x vector values of independent variable, usually time
#' @param y vector values of dependent variable, usually concentration
#' @return \item{AUC}{area under the curve} \item{AUMC}{area under the first
#' moment curve}
#' @seealso \code{\link{LinAUC}},\code{\link{AUC}}
#' @references \enumerate{ \item Gabrielsson J, Weiner D. Pharmacokinetic and
#' Pharmacodynamic Data Analysis - Concepts and Applications. 5th ed. 2016.
#' \item Shargel L, Yu A. Applied Biopharmaceutics and Pharmacokinetics. 7th
#' ed. 2015.  \item Rowland M, Tozer TN. Clinical Pharmacokinetics and
#' Pharmacodynamics - Concepts and Applications. 4th ed. 2011. \item Gibaldi M,
#' Perrier D. Pharmacokinetics. 2nd ed. revised and expanded. 1982. }
#' @keywords AUC
#' @examples
#' LogAUC(Theoph[Theoph$Subject==1, "Time"],Theoph[Theoph$Subject==1, "conc"])
#' # Compare the last line with the above
#' AUC(Theoph[Theoph$Subject==1, "Time"],Theoph[Theoph$Subject==1, "conc"], Method="Log")
#' @export LogAUC
LogAUC <-
function(x, y) # Method="Log" means Linear-Up Log-Down
{
  n = length(x)
  if (n != length(y) | !is.numeric(x) | !is.numeric(y)) stop("Bad Input!")

  auc = 0
  aumc = 0
  for (i in 2:n) {
    if (y[i] < y[i-1] & y[i] > 0) {
      k = (log(y[i-1]) - log(y[i]))/(x[i] - x[i-1]) # -k slope in y-log scale
      auc = auc + (y[i-1] - y[i])/k
      aumc = aumc + (x[i-1]*y[i-1] - x[i]*y[i])/k + (y[i-1] - y[i])/k/k
    } else {
      auc = auc + (x[i] - x[i-1])*(y[i] + y[i-1])/2
      aumc = aumc + (x[i] - x[i-1])*(y[i]*x[i] + y[i-1]*x[i-1])/2
    }
  }
  Result = c(auc, aumc)
  names(Result) = c("AUC", "AUMC")
  return(Result)
}
