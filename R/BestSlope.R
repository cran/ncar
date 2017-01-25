#' Choose best fit slope for the log(y) and x regression by the criteria of
#' adjusted R-square
#'
#' It sequentially fits (log(y) ~ x) from the last point of x to the previous
#' points with at least 3 points. It chooses a slope the highest adjusted
#' R-square. If the difference is less then 1e-4, it chooses longer slope.
#'
#' Choosing the best terminal slope (y in log scale) in pharmacokinetic
#' analysis is somewhat challenging, and it could vary by analysis performer.
#' Currently this function uses ordinary least square method(OLS) only.
#'
#' @param x vector values of x-axis, usually time
#' @param y vector values of y-axis, usually concentration
#' @param AdmMode one of \code{"Bolus"} or \code{"Infusion"} or
#' \code{"Extravascular"} to indicate drug administration mode
#' @return \item{R2}{R-squared} \item{R2ADJ}{adjusted R-squared}
#' \item{LAMZNPT}{number of points used for slope} \item{LAMZ}{negative of
#' slope, lambda_z} \item{b0}{intercept of regression line}
#' \item{CORRXY}{correlation of log(y) and x} \item{LAMZLL}{earliest x for
#' lambda_z} \item{LAMZUL}{last x for lambda_z} \item{CLSTP}{predicted y value
#' at last point, predicted concentration for the last time point}
#' @seealso \code{\link{Slope}}
#' @keywords Slope best fit slope
#' @examples
#' BestSlope(Theoph[Theoph$Subject==1, "Time"],Theoph[Theoph$Subject==1, "conc"])
#' BestSlope(Indometh[Indometh$Subject==1, "time"],Indometh[Indometh$Subject==1, "conc"],
#'           AdmMode="Bolus")
#' @export BestSlope
BestSlope <-
function(x, y, AdmMode="Extravascular")
{
  n = length(x)
  if (n != length(y) | !is.numeric(x) | !is.numeric(y)) stop("Bad Input!")

  if (AdmMode == "Bolus") {
    locStart = which.max(y)  # From Tmax (for Bolus)
  } else {
    locStart = which.max(y) + 1  # From next to Tmax (for the others)
  }
  locLast = max(which(y>0))    # Till non-zero concentration
  if (locLast - locStart < 2) return(c(NA, NA, 0, NA, NA, NA, NA, NA, NA)) # too few to fit

  tmpMat = matrix(nrow=(locLast - locStart - 1), ncol=9) # Slope function returns 9 columns
  colnames(tmpMat) = c("R2", "R2ADJ", "LAMZNPT", "LAMZ", "b0", "CORRXY", "LAMZLL", "LAMZUL", "CLSTP")
  for (i in locStart:(locLast-2)) {
    tmpMat[i - locStart + 1,] = Slope(x[i:locLast], log(y[i:locLast]))
  }
  maxAdjRsq = max(tmpMat[,"R2ADJ"]) # The second column is "Rsq_adjusted" which is the criterion
  OKs = ifelse(abs(maxAdjRsq - tmpMat[,"R2ADJ"]) < 1e-4, TRUE, FALSE) # Tolerance is 1e-4, Phoneix WinNonlin 6.4 User's Guide p33
  nMax = max(tmpMat[OKs,"LAMZNPT"])   # Third column is "No_points_lambda_z" or "n"
  return(tmpMat[OKs & tmpMat[,"LAMZNPT"]==nMax,])
}
