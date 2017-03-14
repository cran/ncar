Interpol = function(x, y, xnew, Slope, b0, down="Linear")
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
    if (down=="Log" & y2 < y1 & y2 > 0) ynew = exp(log(y1) + (log(y2) - log(y1))/(x2 - x1)*(xnew - x1))
    else                                     ynew = y1 + (y2 - y1)/(x2 - x1)*(xnew - x1)
  }

  if (LEFT==TRUE & RIGHT==FALSE) ynew = exp(b0 - Slope*xnew)  # NOT ynew = exp(log(y1) - Slope*(xnew - x1))
  if (LEFT==FALSE & RIGHT==TRUE) ynew = y2/x2 * xnew
  if (LEFT==FALSE & RIGHT==FALSE) stop("Bad Input!")

  return(list(sort(c(x,xnew)), c(y,ynew)[order(c(x,xnew))]))
}
