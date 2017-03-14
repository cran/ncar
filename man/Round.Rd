\name{Round}
\alias{Round}
\title{Round Half Away from Zero}
\description{
This is an ordinary rounding function, so called round half away from zero
}
\usage{Round(x, n = 0)}
\arguments{
  \item{x}{numeric to be rounded}
  \item{n}{indicating decimal digits}
}
\details{
The function \code{round} in R base rounds to the even number, i.e. \code{round(0.5)} is 0 not 1.
If you want rounding 0.5 be 1, you can use this \code{Round} function.
This function is for the consistency with other software like MS-Excel, SAS.
}
\value{
ordinarily rounded value
}
\references{See wikipedia subject "Rounding"}
\author{Kyun-Seop Bae <k@acr.kr>}
\examples{
(x = 1:10 - 0.5)
Round(x)
round(x) # compare with the above
}
\keyword{rounding}
