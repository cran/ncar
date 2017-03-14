Slope = function(x, y)
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
