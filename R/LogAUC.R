LogAUC = function(x, y) # down="Log" means Linear-Up Log-Down
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
