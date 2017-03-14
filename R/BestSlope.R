BestSlope = function(x, y, adm="Extravascular")
{
  n = length(x)
  if (n != length(y) | !is.numeric(x) | !is.numeric(y)) stop("Bad Input!")

  if (toupper(adm) == "BOLUS") {
    locStart = which.max(y)  # From Tmax (for Bolus)
  } else {
    locStart = which.max(y) + 1  # From next to Tmax (for the others)
  }
  locLast = max(which(y > 0))    # Till non-zero concentration
  if (locLast - locStart < 2) return(c(NA, NA, 0, NA, NA, NA, NA, NA, NA)) # too few to fit

  tmpMat = matrix(nrow=(locLast - locStart - 1), ncol=9) # Slope function returns 9 columns
  colnames(tmpMat) = c("R2", "R2ADJ", "LAMZNPT", "LAMZ", "b0", "CORRXY", "LAMZLL", "LAMZUL", "CLSTP")
  for (i in locStart:(locLast - 2)) {
    tmpMat[i - locStart + 1,] = Slope(x[i:locLast], log(y[i:locLast]))
  }
  maxAdjRsq = max(tmpMat[,"R2ADJ"]) # The second column is "Rsq_adjusted" which is the criterion
  OKs = ifelse(abs(maxAdjRsq - tmpMat[,"R2ADJ"]) < 1e-4, TRUE, FALSE) # Tolerance is 1e-4, Phoneix WinNonlin 6.4 User's Guide p33
  nMax = max(tmpMat[OKs,"LAMZNPT"])   # Third column is "No_points_lambda_z" or "n"
  Res = tmpMat[OKs & tmpMat[,"LAMZNPT"]==nMax,]
  if (length(Res) == 0) Res = c(NA, NA, 0, NA, NA, NA, NA, NA, NA)
  return(Res)
}
