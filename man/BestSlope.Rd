\name{BestSlope}
\alias{BestSlope}
\title{Choose best fit slope for the log(y) and x regression by the criteria of adjusted R-square}
\description{
It sequentially fits (log(y) ~ x) from the last point of x to the previous points with at least 3 points.
It chooses a slope the highest adjusted R-square.
If the difference is less then 1e-4, it chooses longer slope.
}
\usage{BestSlope(x, y, adm = "Extravascular")}
\arguments{
  \item{x}{vector values of x-axis, usually time}
  \item{y}{vector values of y-axis, usually concentration}
  \item{adm}{one of \code{"Bolus"} or \code{"Infusion"} or \code{"Extravascular"} to indicate drug administration mode}
}
\details{
Choosing the best terminal slope (y in log scale) in pharmacokinetic analysis is somewhat challenging, and it could vary by analysis performer.
Currently this function uses ordinary least square method(OLS) only.
}
\value{
  \item{R2}{R-squared}
  \item{R2ADJ}{adjusted R-squared}
  \item{LAMZNPT}{number of points used for slope}
  \item{LAMZ}{negative of slope, lambda_z}
  \item{b0}{intercept of regression line}
  \item{CORRXY}{correlation of log(y) and x}
  \item{LAMZLL}{earliest x for lambda_z}
  \item{LAMZUL}{last x for lambda_z}
  \item{CLSTP}{predicted y value at last point, predicted concentration for the last time point}
}
\author{Kyun-Seop Bae <k@acr.kr>}
\seealso{\code{\link{Slope}}}
\examples{
BestSlope(Theoph[Theoph$Subject==1, "Time"],Theoph[Theoph$Subject==1, "conc"])
BestSlope(Indometh[Indometh$Subject==1, "time"],Indometh[Indometh$Subject==1, "conc"],
          adm="Bolus")
}
\keyword{Slope}
