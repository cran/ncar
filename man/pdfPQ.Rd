\name{pdfPQ}
\alias{pdfPQ}
\title{Performance Qualification (PQ) report to a pdf file}
\description{
Generate a self-contained PDF report documenting a Performance Qualification (PQ)
of noncompartmental analysis for the user's own data and intended workflow. Whereas
\code{\link{pdfOQ}} compares the engine to fixed reference values, PQ runs the
analysis the user actually intends to run, on the user's (or representative) data,
and checks that the result is reproducible, scientifically plausible, and
(optionally) matches the user's pre-specified expected values, with a documented,
signed record including a fingerprint (md5) of the input data. The report uses the
package's own pdf helpers (no LaTeX) and the signature/approval page is placed first.
}
\usage{
pdfPQ(fileName = "ncar-PQ-Report.pdf", concData, key = "Subject",
      colTime = "Time", colConc = "conc", ..., expected = NULL,
      acceptance = NULL, performedBy = "", paper = "auto", sigField = FALSE)
}
\arguments{
  \item{fileName}{file name to save the PDF report.}
  \item{concData}{the user's concentration data set (data frame) to qualify.}
  \item{key}{column name(s) identifying each profile.}
  \item{colTime}{column name for time.}
  \item{colConc}{column name for concentration.}
  \item{\dots}{further arguments passed to \code{\link[NonCompart]{tblNCA}} (e.g.
    \code{dose}, \code{adm}, \code{dur}, \code{down}, \code{R2ADJ}, \code{concUnit}),
    i.e. the analysis options the user intends to use in routine work.}
  \item{expected}{optional data frame of the user's pre-specified expected parameter
    values (a \code{key} column plus PPTESTCD columns such as \code{CMAX},
    \code{AUCLST}); each is compared to the computed value within
    \code{acceptance$tol}. This is the strongest, user-owned PQ acceptance.}
  \item{acceptance}{a list of PQ acceptance criteria; if \code{NULL}, sensible
    defaults are used (\code{maxPctExtrap=20}, \code{minR2adj=0.80}, \code{tol=1e-3},
    \code{reproTol=0}, and flags \code{checkTmaxRange}, \code{checkCmaxGEClast},
    \code{checkAUCINFgeAUClast}, \code{failOnNonEvaluable}). Adjust to your intended
    use / SOP.}
  \item{performedBy}{name of the person performing the qualification, printed on the
    signature page. Defaults to the login name.}
  \item{paper}{paper size: \code{"letter"}, \code{"a4"}, or \code{"auto"} (the
    default; Letter in a United States locale, A4 elsewhere).}
  \item{sigField}{if \code{TRUE}, add Adobe Acrobat Reader signature fields under the
    Performed by / Reviewed by lines (via \code{\link{addSigField}}).}
}
\details{
The report contains: the intended-use inputs and the input-data md5 fingerprint; the
PQ acceptance criteria; a reproducibility check (the analysis is re-run on the same
data and must reproduce identical results); per-profile performance/plausibility
checks (Cmax > 0, Cmax >= Clast, Tmax within the observed time range, AUClast > 0,
and for profiles with a terminal slope: AUCINF_obs >= AUClast, AUC \%Extrap within
limit, adjusted R-squared above limit, half-life positive and finite); an optional
comparison to user-supplied expected values; an overall QUALIFIED / NOT QUALIFIED
verdict; and a \code{\link[utils]{sessionInfo}} appendix.

PQ acceptance criteria and the choice of representative data are the user's
responsibility; this function provides the tool and configurable defaults.
}
\value{
Invisibly, a list with \code{fileName}, \code{qualified}, \code{paper},
\code{results} (the \code{tblNCA} table), \code{reproducible}, \code{profiles}
(per-profile checks), \code{fingerprint}, \code{nProfFail}, \code{nNonEval}, and
\code{expected}. A PDF is written to \code{fileName}.
}
\author{Kyun-Seop Bae <k@acr.kr>}
\seealso{
\code{\link{pdfIQ}}, \code{\link{pdfOQ}}, \code{\link[NonCompart]{tblNCA}}
}
\examples{
#Theoph2 <- as.data.frame(Theoph); Theoph2$Subject <- as.numeric(as.character(Theoph2$Subject))
#pdfPQ("MyStudy-PQ.pdf", Theoph2, "Subject", "Time", "conc", dose = 320,
#      concUnit = "mg/L", performedBy = "Kyun-Seop Bae", sigField = TRUE)
}
\keyword{Output Form}
