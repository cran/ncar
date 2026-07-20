\name{writeMD5}
\alias{writeMD5}
\title{Write the MD5 integrity manifest for an installed package}
\description{
Write the standard R \file{MD5} manifest into an installed package directory so
that \code{\link[tools]{checkMD5sums}} (and \code{\link{pdfIQ}}) can verify file
integrity. The manifest records the md5 checksum of every installed file. It is the
same \file{MD5} file that CRAN ships and that R's own installer writes; packages
installed from CRAN already contain it, but a local source install usually does not
(which is why the IQ integrity check then reports WARN). Run \code{writeMD5()} once,
immediately after installing the package, to record the trusted baseline; thereafter
\code{checkMD5sums()} detects any later modification of the installed files.
}
\usage{
writeMD5(pkg = "ncar", lib.loc = NULL)
}
\arguments{
  \item{pkg}{name of the installed package whose \file{MD5} manifest should be
    written.}
  \item{lib.loc}{character vector of library paths to search for \code{pkg}, passed
    to \code{\link{find.package}}. \code{NULL} uses the default libraries.}
}
\details{
The manifest is written in the format used by \code{checkMD5sums}: one line per file,
\code{<md5sum> *<relative-path>}. The \file{MD5} file itself is excluded. The
function uses only the exported \code{\link[tools]{md5sum}}. The installed package
directory must be writable.
}
\value{
Invisibly, the path to the \file{MD5} file that was written. Called for its side
effect.
}
\author{Kyun-Seop Bae <k@acr.kr>}
\seealso{
\code{\link{pdfIQ}}, \code{\link[tools]{checkMD5sums}}, \code{\link[tools]{md5sum}}
}
\examples{
#writeMD5("ncar")
#writeMD5("NonCompart")
#ncar::pdfIQ()          # the file-integrity check now reports PASS
}
\keyword{utilities}
