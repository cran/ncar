\name{addSigField}
\alias{addSigField}
\title{Add Acrobat-Reader signature fields to a report PDF}
\description{
Insert empty digital-signature form fields (AcroForm \code{/Sig} fields) into a PDF
so it can be signed in Adobe Acrobat Reader (the free reader) with one click: open
the PDF, click a field, and sign with a Digital ID. The field is added with a
pure base-R PDF incremental update, with no external tools or packages, so it works
wherever R does. It is designed for the simple PDFs produced by \code{\link{pdfIQ}}
and \code{\link{pdfOQ}}.

Acrobat Reader can also sign a PDF that has no field at all (\dQuote{All tools} >
\dQuote{Use a certificate} > \dQuote{Digitally sign}, then drag a rectangle). This
function only makes the workflow click-to-sign by pre-placing labelled fields.
}
\usage{
addSigField(pdf, out = pdf, page = 1L,
            fieldNames = c("Performed_by", "Reviewed_by"), rects = NULL)
}
\arguments{
  \item{pdf}{path to the input PDF.}
  \item{out}{path to write the result; defaults to overwriting \code{pdf}.}
  \item{page}{1-based page number to place the field(s) on (the signature page is
    page 1 of \code{pdfIQ}/\code{pdfOQ} reports).}
  \item{fieldNames}{character vector of field names; one signature field is added per
    name. The names appear in Acrobat's Signature panel.}
  \item{rects}{optional list of numeric length-4 rectangles \code{c(x0,y0,x1,y1)} in
    PDF points (origin at the lower-left of the page), one per field. If \code{NULL},
    sensible stacked rectangles are used within the page margins.}
}
\details{
The function appends an incremental-update section that adds an \code{/AcroForm} to
the document catalog (with \code{/SigFlags 3}), one \code{/Widget} signature
annotation per field, and the field references to the page's \code{/Annots}. The
original content is left untouched. The resulting fields are unsigned; the actual
cryptographic signature is applied by Acrobat Reader using the signer's Digital ID.
Only classic cross-reference-table PDFs are supported (those written by R's
\code{\link[grDevices]{pdf}} device); the function stops on cross-reference-stream
PDFs.
}
\value{
Invisibly, the output path. A PDF with signature fields is written as a side effect.
}
\author{Kyun-Seop Bae <k@acr.kr>}
\seealso{
\code{\link{pdfIQ}}, \code{\link{pdfOQ}}, \code{\link{signPDF}}
}
\examples{
#pdfIQ("ncar-IQ-Report.pdf", performedBy = "Kyun-Seop Bae")
#addSigField("ncar-IQ-Report.pdf")          # adds Performed_by / Reviewed_by fields
## or in one step:
#pdfIQ("ncar-IQ-Report.pdf", performedBy = "Kyun-Seop Bae", sigField = TRUE)
## then open in Acrobat Reader and click each field to sign with your Digital ID.
}
\keyword{utilities}
