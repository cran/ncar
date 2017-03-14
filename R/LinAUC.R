LinAUC = function(x, y) # down="Linear"
{
  n = length(x)
  if (n != length(y) | !is.numeric(x) | !is.numeric(y)) stop("Bad Input!")

  auc = sum((x[-1] - x[-n])*(y[-1] + y[-n]))/2
  aumc = sum((x[-1] - x[-n])*(x[-1]*y[-1] + x[-n]*y[-n]))/2
  Result = c(auc, aumc)
  names(Result) = c("AUC", "AUMC")
  return(Result)
}
