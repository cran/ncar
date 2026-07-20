\name{pdfIQ}
\alias{pdfIQ}
\title{Installation Qualification (IQ) report to a pdf file}
\description{
Generate a self-contained PDF report that documents whether the
\code{ncar} / \code{NonCompart} packages are correctly installed, intact,
loadable, and operational in the user's own R environment. It is intended
as Installation Qualification (IQ) evidence, in the spirit of the
WinNonlin validation suite. The report uses only base R and the package's
own pdf helpers, so it requires no LaTeX, pandoc, or other external tools.
}
\usage{
pdfIQ(fileName = "ncar-IQ-Report.pdf", pkgs = c("ncar", "NonCompart"),
      functional = TRUE, performedBy = "", paper = "auto", sigField = FALSE)
}
\arguments{
  \item{fileName}{file name to save the PDF report}
  \item{pkgs}{character vector of package names to qualify. Their declared
    dependencies are checked automatically.}
  \item{functional}{if \code{TRUE}, run a small operational check: a known
    NCA computation (Theophylline subject 1) is compared with an external
    WinNonlin reference within a relative tolerance of 1e-3, plus two
    control tests that verify the comparator itself (a known-match must
    pass, a known-differ must fail).}
  \item{performedBy}{name of the person performing the qualification,
    printed in the report header. Defaults to the login name.}
  \item{paper}{paper size: \code{"letter"}, \code{"a4"}, or \code{"auto"}
    (the default), which selects \code{"letter"} in a United States locale and
    \code{"a4"} elsewhere. The report uses 1 inch margins on every side and
    places the signature (approval) page first.}
  \item{sigField}{if \code{TRUE}, add Adobe Acrobat Reader signature fields to the
    finished report (via \code{\link{addSigField}}) so it can be signed with one
    click in Acrobat Reader.}
}
\details{
The report contains: (1) the test environment (R version, platform, OS,
locale, library paths); (2) installed package versions, locations and
declared-dependency version satisfaction; (3) file integrity via
\code{\link[tools]{checkMD5sums}} (PASS when files match the MD5 manifest,
FAIL on mismatch, WARN when no manifest is present, e.g. for a local source
install rather than a CRAN install); (4) namespace load and presence of
core exported functions; (5) the optional functional verification; an
overall QUALIFIED / NOT QUALIFIED verdict; a signature block; and a
\code{\link[utils]{sessionInfo}} appendix.
}
\value{
Invisibly returns a list with elements \code{fileName}, \code{qualified}
(logical), \code{checks} (a data frame of every check with its Section,
Item, Result and Status), and the counts \code{nPass}, \code{nFail},
\code{nWarn}. A PDF file is written to \code{fileName} as a side effect.
}
\author{Kyun-Seop Bae <k@acr.kr>}
\seealso{
\code{\link{pdfNCA}}, \code{\link{txtNCA}}, \code{\link[tools]{checkMD5sums}},
\code{\link[utils]{sessionInfo}}
}
\examples{
#pdfIQ()
#pdfIQ(fileName = "MyInstall-IQ.pdf", performedBy = "Jane Doe")
#res <- pdfIQ()
#res$qualified
#res$checks
}
\keyword{Output Form}
