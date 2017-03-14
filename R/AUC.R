AUC = function(x, y, down="Linear")
{
  n = length(x)
  if (n != length(y) | !is.numeric(x) | !is.numeric(y)) stop("Bad Input!")  
  
  Res = matrix(nrow=n, ncol=2)
  Res[1,] = c(0, 0)
  for (i in 2:n) {
    if (y[i] >= y[i-1]) {
      Res[i,1] = (x[i] - x[i-1])*(y[i] + y[i-1])/2 
      Res[i,2] = (x[i] - x[i-1])*(x[i]*y[i] + x[i-1]*y[i-1])/2
    } else if (down == "Linear") {
      Res[i,1] = (x[i] - x[i-1])*(y[i] + y[i-1])/2 
      Res[i,2] = (x[i] - x[i-1])*(x[i]*y[i] + x[i-1]*y[i-1])/2
    } else if (down == "Log") {
      k = (log(y[i-1]) - log(y[i]))/(x[i] - x[i-1]) # -k slope in y-log scale
      Res[i,1] = (y[i-1] - y[i])/k
      Res[i,2] = (x[i-1]*y[i-1] - x[i]*y[i])/k + (y[i-1] - y[i])/k/k
    } else {
      stop("Unknown method!") 
    }
  }
  Result = cbind(cumsum(Res[,1]), cumsum(Res[,2]))
  colnames(Result) = c("AUC","AUMC")
  return(Result)
}
