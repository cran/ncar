#' Get the Slope of regression log(y) ~ x
#'
#' It calculates the slope with linear regression of log(y) ~ x
#'
#' With time-concentration curve, you frequently need to estimate slope in
#' log(concentration) ~ time. This function is usually called by
#' \code{BestSlope} function and you seldom need to call this function
#' directly.
#'
#' @param x vector values of independent variable, usually time
#' @param y vector values of dependent variable, usually concentration
#' @return \item{R2}{R-squared} \item{R2ADJ}{adjusted R-squared}
#' \item{LAMZNPT}{number of points used for slope} \item{LAMZ}{negative of
#' slope, lambda_z} \item{b0}{intercept of regression line}
#' \item{CORRXY}{correlation of log(y) and x} \item{LAMZLL}{earliest x for
#' lambda_z} \item{LAMZUL}{last x for lambda_z} \item{CLSTP}{predicted y value
#' at last point, predicted concentration for the last time point}
#' @seealso \code{\link{BestSlope}}
#' @keywords slope
#' @examples
#' Slope(Indometh[Indometh$Subject==1, "time"],Indometh[Indometh$Subject==1, "conc"])
#' @export Slope
Slope <-
function(x, y)
{
  n = length(x)
  if (n != length(y) | !is.numeric(x) | !is.numeric(y)) stop("Bad Input!")

  mx = mean(x)
  my = mean(y)
  Sxx = sum((x - mx)*(x - mx))
  Sxy = sum((x - mx)*(y - my))
  Syy = sum((y - my)*(y - my))
  b1 = Sxy/Sxx
  b0 = my - b1*mx
  Rsq = b1 * Sxy / Syy
  aRsq = 1 - (1 - Rsq)*(n - 1)/(n - 2)           # Rsq_adjusted, See wikipedia
  Corr = sign(b1)*sqrt(Rsq)
  LambdaLower = x[1]
  LambdaUpper = x[n]
  ClastPred = exp(b0 + b1 * x[n])

  if (b1 < 0) Result = c(Rsq, aRsq, n, -b1, b0, Corr, LambdaLower, LambdaUpper, ClastPred)  # negative slope to positive slope
  else        Result = c(NA, NA, 0, NA, NA, NA, NA, NA, NA)      # positive slope

  names(Result) = c("R2", "R2ADJ", "LAMZNPT", "LAMZ", "b0", "CORRXY", "LAMZLL", "LAMZUL", "CLSTP")
  return(Result)
}
