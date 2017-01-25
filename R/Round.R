#' Round Half Away from Zero
#'
#' This is an ordinary rounding function, so called round half away from zero
#'
#' The function \code{round} in R base rounds to the even number, i.e.
#' \code{round(0.5)} is 0 not 1. If you want rounding 0.5 be 1, you can use
#' this \code{Round} function. This function is for the consistency with other
#' software like MS-Excel, SAS.
#'
#' @param x numeric to be rounded
#' @param n indicating decimal digits
#' @return ordinarily rounded value
#' @references See wikipedia subject "Rounding"
#' @keywords round rounding
#' @examples
#' (x = 1:10 - 0.5)
#' Round(x)
#' round(x) # compare with the above
#' @export Round
Round <-
function(x, n=0)
{
  return(sign(x)*trunc(abs(x)*10^n + 0.5)/10^n)
}
