\name{pdfOQ}
\alias{pdfOQ}
\title{Operational Qualification (OQ) report to a pdf file}
\description{
Generate a self-contained PDF report that documents whether the
\pkg{NonCompart} noncompartmental analysis engine (as used through \pkg{ncar})
reproduces pre-specified reference results on the user's own machine, within a
pre-specified relative tolerance. It is intended as Operational Qualification
(OQ) evidence, in the spirit of the WinNonlin computational-engine verification.
The report uses only base R, \pkg{NonCompart}, and the package's own pdf helpers,
so it requires no LaTeX, pandoc, or other external tools.
}
\usage{
pdfOQ(fileName = "ncar-OQ-Report.pdf", cases = NULL, tol = 0.001,
      refDir = system.file("OQ", package = "ncar"), performedBy = "",
      paper = "auto", sigField = FALSE)
}
\arguments{
  \item{fileName}{file name to save the PDF report}
  \item{cases}{a list of OQ case definitions. If \code{NULL}, the built-in set of
    eight WinNonlin-referenced scenarios is used (Theophylline and Indometacin,
    extravascular / IV bolus / IV infusion, each with linear-up-linear-down and
    linear-up-log-down AUC). Each case is a list with elements \code{id},
    \code{desc}, \code{dataset}, \code{key}, \code{colTime}, \code{colConc},
    \code{args} (a list of further \code{\link[NonCompart]{tblNCA}} arguments), and \code{ref}
    (the reference csv file name in \code{refDir}).}
  \item{tol}{acceptance tolerance: a parameter passes when the symmetric relative
    difference \code{abs(R - W) / ((abs(R) + abs(W))/2)} between the computed value
    \code{R} and the reference value \code{W} is at most \code{tol}. An absolute
    fallback is used when both values are essentially zero.}
  \item{refDir}{directory holding the reference csv files. Defaults to the
    \code{OQ} folder installed with \pkg{ncar}.}
  \item{performedBy}{name of the person performing the qualification, printed in
    the report header. Defaults to the login name.}
  \item{paper}{paper size: \code{"letter"}, \code{"a4"}, or \code{"auto"} (the
    default), which selects \code{"letter"} in a United States locale and
    \code{"a4"} elsewhere. The report uses 1 inch margins on every side and
    places the signature (approval) page first.}
  \item{sigField}{if \code{TRUE}, add Adobe Acrobat Reader signature fields to the
    finished report (via \code{\link{addSigField}}) so it can be signed with one
    click in Acrobat Reader.}
}
\details{
For each case the function runs \code{\link[NonCompart]{tblNCA}} on the (frozen) input data,
maps the CDISC PPTESTCD result columns to WinNonlin parameter names using the
internal \code{RptCfg} table, matches comparable parameters to the reference table
(robust to \code{\%} vs \code{.} naming), and compares every parameter for every
subject. A case passes only when all of its comparisons pass.

The bundled reference values are WinNonlin 6.4 NCA outputs. These establish
\emph{concordance} with the de-facto reference implementation (Tier B); they are
not, by themselves, an independent verification. Independent (Tier A) anchors
(hand-calculated edge cases, published textbook values) are a planned addition.
}
\value{
Invisibly returns a list with elements \code{fileName}, \code{qualified}
(logical), \code{tol}, \code{nCases}, \code{nFailCases}, and \code{results}
(a per-case list with status, comparison counts, maximum relative difference,
and any failed comparisons). A PDF file is written to \code{fileName}.
}
\author{Kyun-Seop Bae <k@acr.kr>}
\seealso{
\code{\link{pdfIQ}}, \code{\link{pdfNCA}}, \code{\link[NonCompart]{tblNCA}}
}
\examples{
#pdfOQ()
#res <- pdfOQ(fileName = "MyEngine-OQ.pdf", performedBy = "Jane Doe")
#res$qualified
#res$results[[1]]$maxRD
}
\keyword{Output Form}
