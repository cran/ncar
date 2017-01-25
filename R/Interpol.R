#' Interpolate y value
#' 
#' It interpolates y value when a corresponding x value (xnew) does not exist
#' within x vector
#' 
#' This function interpolate y value, if xnew is not in x vector. If xnew is in
#' x vector, it just returns the given x and y vector. This function usually is
#' called by \code{IntAUC} function Returned vector is sorted in the order of
#' increasing x values.
#' 
#' @param x vector values of x-axis, usually time
#' @param y vector values of y-axis, usually concentration
#' @param xnew new x point to be interpolated, usually new time point
#' @param Slope slope of regression log(y) ~ x
#' @param b0 y value of just left point of xnew
#' @param Method either of \code{"Linear"} or \code{"Log"} to indicate the way
#' to interpolate
#' @return new x and y vector containing xnew and ynew point
#' @seealso \code{\link{IntAUC}}
#' @keywords AUC interpolation interval partial
#' @examples
#' 
#' 
#' 
#' x = 10:1 + 0.1
#' y = -2*x + 40.2
#' Interpol(x, y, 1.5)
#' Interpol(x, y, 1.5, Method="Log")
#' 
#' 
#' 
#' @export Interpol
Interpol <-
function(x, y, xnew, Slope, b0, Method="Linear")
{
  n = length(x)
  if (n != length(y) | !is.numeric(x) | !is.numeric(y)) stop("Bad Input!")

  if (xnew %in% x) return(list(x, y))

  if (sum(x < xnew) > 0) {
    LEFT = TRUE
    x1 = x[max(which(x < xnew))]
    y1 = y[max(which(x < xnew))]
  } else LEFT = FALSE

  if (sum(x > xnew) > 0) {
    RIGHT = TRUE
    x2 = x[min(which(x > xnew))]
    y2 = y[min(which(x > xnew))]
  } else RIGHT = FALSE

  if (LEFT==TRUE & RIGHT==TRUE) {
    if (Method=="Log" & y2 < y1 & y2 > 0) ynew = exp(log(y1) + (log(y2) - log(y1))/(x2 - x1)*(xnew - x1))
    else                                  ynew = y1 + (y2 - y1)/(x2 - x1)*(xnew - x1)
  }

  if (LEFT==TRUE & RIGHT==FALSE) ynew = exp(b0 - Slope*xnew)  # NOT ynew = exp(log(y1) - Slope*(xnew - x1))
  if (LEFT==FALSE & RIGHT==TRUE) ynew = y2/x2 * xnew
  if (LEFT==FALSE & RIGHT==FALSE) stop("Bad Input!")

  return(list(sort(c(x,xnew)),c(y,ynew)[order(c(x,xnew))]))
}
