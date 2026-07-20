\name{NEWS}
\title{News for Package \pkg{ncar}}
\section{Changes in version 0.7.0 (2026-06-20)}{
  \itemize{
    \item{\code{pdfIQ} added: generates a self-contained Installation Qualification (IQ) report in pdf that documents whether \pkg{ncar} and \pkg{NonCompart} are correctly installed, intact, loadable, and operational in the user's R environment (environment, package versions, dependency satisfaction, MD5 file integrity, namespace/exports, a functional NCA check against an external reference, and an overall QUALIFIED/NOT QUALIFIED verdict with a sessionInfo appendix).}
    \item{\code{pdfOQ} added: generates a self-contained Operational Qualification (OQ) report in pdf. It runs a set of NCA test cases (eight WinNonlin-referenced scenarios shipped in \file{inst/OQ}) through \code{tblNCA} on the user's machine and compares every parameter for every subject to the pre-specified reference within a relative tolerance. The report lists every computed value next to its reference value, the absolute and relative difference, and a pass flag, with an overall QUALIFIED/NOT QUALIFIED verdict.}
    \item{\code{writeMD5} added: writes the standard R \file{MD5} integrity manifest into an installed package directory (using \code{tools::md5sum}), so that the \code{pdfIQ} file-integrity check reports PASS for local installs. CRAN installs already include this manifest.}
    \item{\code{pdfPQ} added: a Performance Qualification (PQ) report generator. It runs the analysis the user intends on the user's own data and checks reproducibility, per-profile scientific plausibility, and (optionally) agreement with the user's expected values, with an input-data md5 fingerprint and a signed record.}
    \item{\code{pdfNCA} now prints \dQuote{Performed by} / \dQuote{Reviewed by} approval lines (and an optional \code{performedBy} name) on the first page.}
    \item{\code{addSigField} added (and a \code{sigField} argument to \code{pdfIQ}/\code{pdfOQ}/\code{pdfPQ}): insert Adobe Acrobat Reader signature fields into the report PDF (pure base-R PDF incremental update, no external tools) so it can be signed with one click in the free Acrobat Reader, instead of printing and scanning.}
    \item{\code{signPDF} and \code{verifyPDF} added: digitally sign a report PDF (a detached SHA-256 signature written to \file{<pdf>.sig}, using the \pkg{openssl} package) and verify it, as a scriptable alternative.}
    \item{The IQ and OQ reports use 1 inch margins on every side, select Letter paper in a United States locale and A4 elsewhere (\code{paper} argument), place the signature/approval page first, and include a per-file md5 checksum appendix (IQ).}
  }
}
\section{Changes in version 0.6.1 (2026-06-19)}{
  \itemize{
    \item{The dependency on the archived \pkg{rtf} package was removed. The subset of \pkg{rtf} functions required by \code{rtfNCA} is now vendored internally in \file{R/rtf_utils.R} (originally authored by Michael E. Schaffer, GPL (>= 2)).}
  }
}
\section{Changes in version 0.6.0 (2026-04-26)}{
  \itemize{
    \item{txtNCA, pdfNCA, and rtfNCA support indices of points (UsePoints) to calculate terminal slope.}
  }
}
\section{Changes in version 0.5.1 (2026-03-27)}{
  \itemize{
    \item{pdfNCA and rtfNCA support vector input for dur argument.}
    \item{Four errata fixed.}
  }
}
\section{Changes in version 0.5.0 (2023-11-11)}{
  \itemize{
    \item{SS option for steady-state added in txtNCA, pdfNCA and rtfNCA.}
  }
}
\section{Changes in version 0.4.5 (2022-02-23)}{
  \itemize{
    \item{Problem with tbl_df in pdfNCA and rtfNCA function is fixed.}
  }
}
\section{Changes in version 0.4.4 (2021-07-09)}{
  \itemize{
    \item{\code{mayExcludeLast} option added to pdfNCA, rtfNCA, txtNCA. This option works with NonCompart 0.4.9 or later.}
  }
}
\section{Changes in version 0.4.3 (2021-06-11)}{
  \itemize{
    \item{Handles NA better with less error in the plot.}
  }
}
\section{Changes in version 0.4.2 (2018-09-27)}{
  \itemize{
    \item{Some sentences are revised.}
  }
}
\section{Changes in version 0.4.1 (2018-07-20)}{
  \itemize{
    \item{Argument \code{key} replaced \code{key} in \code{rtfNCA} and \code{pdfNCA} function.}
  }
}
\section{Changes in version 0.4.0 (2018-06-20)}{
  \itemize{
    \item{Deprecated option in \code{Sys.timezone()}. is removed.}
  }
}
\section{Changes in version 0.3.9 (2018-05-10)}{
  \itemize{
    \item{Calling twice for terminal slope is corrected.}
  }
}
\section{Changes in version 0.3.8 (2018-04-10)}{
  \itemize{
    \item{Errata in manual corrected.}
  }
}
\section{Changes in version 0.3.7 (2017-08-16)}{
  \itemize{
    \item{\pkg{ncar} to be compatible with NonCompart_0.3.3.}
  }
}
\section{Changes in version 0.3.6 (2017-07-18)}{
  \itemize{
    \item{\pkg{ncar} A bug in C0 calcuation of IV bolus case was fixed.}
  }
}
\section{Changes in version 0.3.5 (2017-07-10)}{
  \itemize{
    \item{\pkg{ncar} depends on \pkg{NonCompart}}
  }
}
\section{Changes in version 0.3.4 (2017-03-12)}{
  \itemize{
    \item{\pkg{ncar} does not sort subject ID any more.}
  }
}
\section{Changes in version 0.3.3 (2017-03-12)}{
  \itemize{
    \item{Various report functions like pdfNCA, rtfNCA, and txtNCA are added.}
    \item{IndiNCA and NCA are deprecated.}
  }
}

