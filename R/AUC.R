#' Calculate Area Under the Curve (AUC) and Area Under the first Moment Curve
#' (AUMC) in a table format
#'
#' Calculate Area Under the Curve(AUC) and the first Moment Curve(AUMC) in two
#' ways; 'linear trapezoidal method' or 'linear-up and log-down' method. Return
#' a table of cumulative values.
#'
#' \code{Method="Linear"} means linear trapezoidal rule with linear
#' interpolation.  \code{Method="Log"} means linear-up and log-down method.
#'
#' @param x vector values of independent variable, usually time
#' @param y vector values of dependent variable, usually concentration
#' @param Method either of \code{"Linear"} or \code{"Log"} to indicate the way
#' to calculate AUC and AUMC
#' @return Table with two columns, \code{AUC} and \code{AUMC}; the first column
#' values are cumulative AUCs and the second column values cumulative AUMCs.
#' @seealso \code{\link{LinAUC}},\code{\link{LogAUC}}
#' @references Rowland M, Tozer TN. Clinical Pharmacokinetics and
#' Pharmacodynamics - Concepts and Applications. 4th ed. pp687-689. 2011.
#' @keywords AUC AUMC
#' @examples
#' AUC(Theoph[Theoph$Subject==1, "Time"],Theoph[Theoph$Subject==1, "conc"]) # Default is "Linear"
#' AUC(Theoph[Theoph$Subject==1, "Time"],Theoph[Theoph$Subject==1, "conc"], Method="Log")
#' @export AUC
AUC <-
function(x, y, Method="Linear")
{
  n = length(x)
  if (n != length(y) | !is.numeric(x) | !is.numeric(y)) stop("Bad Input!")

  Res = matrix(nrow=n, ncol=2)
  Res[1,] = c(0, 0)
  for (i in 2:n) {
    if (y[i] >= y[i-1]) {
      Res[i,1] = (x[i] - x[i-1])*(y[i] + y[i-1])/2
      Res[i,2] = (x[i] - x[i-1])*(x[i]*y[i] + x[i-1]*y[i-1])/2
    } else if (Method=="Linear") {
      Res[i,1] = (x[i] - x[i-1])*(y[i] + y[i-1])/2
      Res[i,2] = (x[i] - x[i-1])*(x[i]*y[i] + x[i-1]*y[i-1])/2
    } else if (Method=="Log") {
      k = (log(y[i-1]) - log(y[i]))/(x[i] - x[i-1]) # -k slope in y-log scale
      Res[i,1] = (y[i-1] - y[i])/k
      Res[i,2] = (x[i-1]*y[i-1] - x[i]*y[i])/k + (y[i-1] - y[i])/k/k
    } else {
      stop("Unknown Method!")
    }
  }
  Result = cbind(cumsum(Res[,1]), cumsum(Res[,2]))
  colnames(Result) = c("AUC","AUMC")
  return(Result)
}
