\name{signPDF}
\alias{signPDF}
\alias{verifyPDF}
\title{Digitally sign and verify a PDF report}
\description{
Apply a cryptographic digital signature to a (report) PDF, as an alternative to the
print-sign-scan workflow, and verify it. \code{signPDF} signs the PDF bytes with the
signer's private key (RSA or EC) using SHA-256 and writes a detached signature
sidecar \file{<pdf>.sig} (and, by default, the signer's public key
\file{<pdf>.pubkey.pem}). \code{verifyPDF} confirms, with the signer's public
key or certificate, that the PDF is byte-for-byte intact and was signed by that key
(tamper-evidence and non-repudiation). These functions require the \pkg{openssl}
package.

This is a \emph{detached} signature (a separate \file{.sig} file), not a PAdES
signature embedded inside the PDF; embedding a visible in-viewer signature requires a
dedicated PDF tool (e.g. Adobe Acrobat or \samp{pyhanko}). The detached signature is
cryptographically equivalent for integrity and non-repudiation.
}
\usage{
signPDF(pdf, key, password = NULL, signer = "", role = "",
        sigFile = paste0(pdf, ".sig"), writePubkey = TRUE)

verifyPDF(pdf, sigFile = paste0(pdf, ".sig"), pubkey = paste0(pdf, ".pubkey.pem"))
}
\arguments{
  \item{pdf}{path to the PDF file to sign or verify.}
  \item{key}{the signer's private key: a path to a PEM key file or an \pkg{openssl}
    key object. Create one once with, e.g., \code{openssl::rsa_keygen()} and
    \code{openssl::write_pem()}.}
  \item{password}{password for an encrypted private key, or \code{NULL}.}
  \item{signer}{name of the signer, recorded in the signature sidecar. Defaults to
    the login name.}
  \item{role}{optional role recorded in the sidecar, e.g. \code{"Performed by"} or
    \code{"Reviewed by"}.}
  \item{sigFile}{path of the signature sidecar to write (\code{signPDF}) or read
    (\code{verifyPDF}).}
  \item{writePubkey}{if \code{TRUE}, also write the signer's public key next to the
    PDF for convenience. For non-repudiation, verify against the signer's
    independently-trusted public key or certificate, not one shipped with the file.}
  \item{pubkey}{the signer's public key or certificate used to verify: a PEM path
    (public key or X.509 certificate) or an \pkg{openssl} pubkey/cert object.}
}
\value{
\code{signPDF} invisibly returns a list with the sidecar path, the PDF \code{sha256},
the \code{signer}, and the public-key fingerprint; it writes the \file{.sig} sidecar
as a side effect. \code{verifyPDF} invisibly returns \code{TRUE} only if both the
signature is valid and the recorded hash matches, and prints a human-readable result.
}
\author{Kyun-Seop Bae <k@acr.kr>}
\seealso{
\code{\link{pdfIQ}}, \code{\link{pdfOQ}}, \code{\link{writeMD5}}
}
\examples{
#key <- openssl::rsa_keygen()
#openssl::write_pem(key, "signer_key.pem")
#pdfIQ("ncar-IQ-Report.pdf", performedBy = "Kyun-Seop Bae")
#signPDF("ncar-IQ-Report.pdf", "signer_key.pem", signer = "Kyun-Seop Bae")
#verifyPDF("ncar-IQ-Report.pdf")
}
\keyword{utilities}
