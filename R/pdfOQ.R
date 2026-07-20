# pdfOQ.R
# Operational Qualification (OQ) report generator for the 'NonCompart' NCA engine
# (as used through 'ncar'). Runs a set of NCA test cases on the user's machine and
# compares each computed parameter to a pre-specified reference value within a
# pre-specified relative tolerance, then writes a self-contained PDF report with
# per-case relative-difference summaries and an overall PASS/FAIL verdict.
# Uses only base R + NonCompart + ncar's own pdf helpers (BasicUtil.R): no LaTeX.
#
# Reference values are the WinNonlin 6.4 NCA outputs shipped in inst/OQ
# (Final_Parameters_Pivoted_*.csv). These establish *concordance* with the
# de-facto reference implementation (Tier B). They are not, by themselves, an
# independent verification; independent (Tier A) anchors are a planned addition.

#-- internal: canonicalize a column name for robust matching (handles % vs .) ---
.oqCanon = function(x) tolower(gsub("[^A-Za-z0-9]", "", x))

#-- internal: status of one observed-vs-reference value ------------------------
# Symmetric relative difference  abs(R-W) / ((|R|+|W|)/2)  (the metric used in the
# WinNonlin Computational Engines Verification Report), with an absolute fallback
# when both values are ~0 (so a true zero is not silently skipped).
.oqDiff = function(R, W, tol, absTol=1e-6)
{
  R = suppressWarnings(as.numeric(R)); W = suppressWarnings(as.numeric(W))
  if (is.na(R) && is.na(W)) return(list(rd=0, pass=TRUE))
  if (is.na(R) ||  is.na(W)) return(list(rd=NA_real_, pass=FALSE))
  denom = (abs(R) + abs(W)) / 2
  if (denom == 0) return(list(rd=0, pass=(abs(R - W) <= absTol)))
  rd = abs(R - W) / denom
  list(rd=rd, pass=(rd <= tol))
}

#-- internal: the default OQ case set (8 WinNonlin-referenced scenarios) --------
.oqDefaultCases = function()
{
  list(
    list(id="Theoph_Linear",  desc="Plasma, Extravascular, Linear-up Linear-down",
         dataset="Theoph", key="Subject", colTime="Time", colConc="conc",
         args=list(dose=320, concUnit="mg/L"),
         ref="Final_Parameters_Pivoted_Theoph_Linear.csv"),
    list(id="Theoph_Log",     desc="Plasma, Extravascular, Linear-up Log-down",
         dataset="Theoph", key="Subject", colTime="Time", colConc="conc",
         args=list(dose=320, down="Log", concUnit="mg/L"),
         ref="Final_Parameters_Pivoted_Theoph_Log.csv"),
    list(id="Indometh_Bolus_Linear", desc="Plasma, IV Bolus, Linear-up Linear-down",
         dataset="Indometh", key="Subject", colTime="time", colConc="conc",
         args=list(dose=25, adm="Bolus", concUnit="mg/L", R2ADJ=0.8),
         ref="Final_Parameters_Pivoted_Indometh_Linear.csv"),
    list(id="Indometh_Bolus_Log", desc="Plasma, IV Bolus, Linear-up Log-down",
         dataset="Indometh", key="Subject", colTime="time", colConc="conc",
         args=list(dose=25, adm="Bolus", down="Log", concUnit="mg/L", R2ADJ=0.8),
         ref="Final_Parameters_Pivoted_Indometh_Log.csv"),
    list(id="Indometh_Infusion_Linear", desc="Plasma, IV Infusion (dur=0.25), Linear-up Linear-down",
         dataset="Indometh", key="Subject", colTime="time", colConc="conc",
         args=list(dose=25, adm="Infusion", dur=0.25, concUnit="mg/L", R2ADJ=0.8),
         ref="Final_Parameters_Pivoted_Indometh_Linear_Infusion.csv"),
    list(id="Indometh_Infusion_Log", desc="Plasma, IV Infusion (dur=0.25), Linear-up Log-down",
         dataset="Indometh", key="Subject", colTime="time", colConc="conc",
         args=list(dose=25, adm="Infusion", dur=0.25, down="Log", concUnit="mg/L", R2ADJ=0.8),
         ref="Final_Parameters_Pivoted_Indometh_Log_Infusion.csv"),
    list(id="Indometh_Extravasc_Linear", desc="Plasma, Extravascular (default adm), Linear-up Linear-down",
         dataset="Indometh", key="Subject", colTime="time", colConc="conc",
         args=list(dose=25, concUnit="mg/L", R2ADJ=0.8),
         ref="Final_Parameters_Pivoted_Indometh_Linear_Wrong_Extravascular.csv"),
    list(id="Indometh_Extravasc_Log", desc="Plasma, Extravascular (default adm), Linear-up Log-down",
         dataset="Indometh", key="Subject", colTime="time", colConc="conc",
         args=list(dose=25, down="Log", concUnit="mg/L", R2ADJ=0.8),
         ref="Final_Parameters_Pivoted_Indometh_Log_Wrong_Extravascular.csv")
  )
}

#-- internal: run a single case, return a structured result --------------------
.oqRunCase = function(case, refDir, tol)
{
  out = list(id=case$id, desc=case$desc, status="FAIL", nComp=0, nFail=0,
             maxRD=NA_real_, nParam=0, nSubj=0, comp=NULL,
             params=character(0), subjects=character(0), settings="", message="")
  out$settings = tryCatch(
    sprintf("dataset=%s, time=%s, conc=%s, %s", case$dataset, case$colTime, case$colConc,
            paste(names(case$args), vapply(case$args, as.character, character(1)),
                  sep="=", collapse=", ")),
    error=function(e) "")

  # load + coerce the input dataset (as.data.frame strips groupedData class,
  # which otherwise can crash tblNCA)
  d = tryCatch({
        e = new.env(); utils::data(list=case$dataset, package="datasets", envir=e)
        as.data.frame(e[[case$dataset]])
      }, error=function(err) NULL)
  if (is.null(d)) { out$message = paste("dataset not available:", case$dataset); return(out) }
  d[[case$key]] = suppressWarnings(as.numeric(as.character(d[[case$key]])))

  # run the engine
  R = tryCatch(
        do.call(NonCompart::tblNCA,
                c(list(concData=d, key=case$key, colTime=case$colTime, colConc=case$colConc),
                  case$args)),
        error=function(err) err)
  if (inherits(R, "error")) { out$message = paste("tblNCA error:", conditionMessage(R)); return(out) }
  R = as.data.frame(R)

  # map PPTESTCD -> WinNonlin names via ncar's internal RptCfg
  cn = colnames(R)
  rn = rownames(RptCfg); rownames(RptCfg) = RptCfg$PPTESTCD
  wnl = RptCfg[cn[-1], "WNL"]
  rownames(RptCfg) = rn
  colnames(R) = c(cn[1], wnl)

  # reference table (WinNonlin names; keep % etc. intact)
  W = tryCatch(read.csv(file.path(refDir, case$ref), as.is=TRUE, check.names=FALSE),
               error=function(err) NULL)
  if (is.null(W)) { out$message = paste("reference not found:", case$ref); return(out) }

  # comparable parameter columns by canonical name (robust to % vs .),
  # presented in the order they appear in the reference table
  rCanon = .oqCanon(colnames(R)); wCanon = .oqCanon(colnames(W))
  skip = .oqCanon(c("Subject", "b0", case$key))
  wcols = character(0); rcols = character(0)
  for (j in seq_along(colnames(W))) {
    cc = wCanon[j]
    if (cc %in% skip) next
    ri = match(cc, rCanon)
    if (is.na(ri)) next
    wcols = c(wcols, colnames(W)[j]); rcols = c(rcols, colnames(R)[ri])
  }
  if (length(wcols) == 0) { out$message = "no comparable parameters"; return(out) }

  subj = intersect(W[[1]], R[[1]])
  np = length(wcols); ns = length(subj); N = np * ns
  P = character(N); SB = character(N); RF = numeric(N); OB = numeric(N)
  RD = numeric(N); PS = logical(N)
  k = 0; maxRD = 0; nFail = 0
  for (s in subj) {
    wr = W[W[[1]] == s, , drop=FALSE][1, ]
    rr = R[R[[1]] == s, , drop=FALSE][1, ]
    for (m in seq_len(np)) {
      st = .oqDiff(rr[[rcols[m]]], wr[[wcols[m]]], tol)
      k = k + 1
      P[k]  = wcols[m]; SB[k] = as.character(s)
      RF[k] = suppressWarnings(as.numeric(wr[[wcols[m]]]))
      OB[k] = suppressWarnings(as.numeric(rr[[rcols[m]]]))
      RD[k] = st$rd; PS[k] = isTRUE(st$pass)
      if (!is.na(st$rd) && st$rd > maxRD) maxRD = st$rd
      if (!PS[k]) nFail = nFail + 1
    }
  }
  out$comp = data.frame(param=P, subj=SB, ref=RF, obs=OB, reldiff=RD, pass=PS,
                        stringsAsFactors=FALSE)
  out$params = wcols; out$subjects = as.character(subj)
  out$nParam = np; out$nSubj = ns; out$nComp = N; out$nFail = nFail; out$maxRD = maxRD
  out$status = if (N > 0 && nFail == 0) "PASS" else "FAIL"
  out
}

#-- main ------------------------------------------------------------------------
pdfOQ = function(fileName="ncar-OQ-Report.pdf", cases=NULL, tol=1e-3,
                 refDir=system.file("OQ", package="ncar"), performedBy="",
                 paper="auto", sigField=FALSE)
{
  now = Sys.time(); si = Sys.info()
  if (is.null(cases)) cases = .oqDefaultCases()
  if (!nzchar(refDir) || !dir.exists(refDir))
    stop("OQ reference directory not found. Install 'ncar' or pass refDir=.")

  results = lapply(cases, .oqRunCase, refDir=refDir, tol=tol)

  pkgVer = function(p) tryCatch(as.character(packageVersion(p)), error=function(e) "not installed")
  hr  = paste(rep("=", 72), collapse="")
  hr2 = paste(rep("-", 72), collapse="")

  nFailCases = sum(vapply(results, function(r) r$status != "PASS", logical(1)))
  qualified  = (nFailCases == 0) && length(results) > 0
  status = if (qualified) "QUALIFIED" else "NOT QUALIFIED"
  who    = if (nzchar(performedBy)) performedBy else as.character(si["login"])
  stamp  = format(now, "%Y-%m-%d %H:%M:%S")

  ## ----- page 1: signature / approval page -----
  sig = character(0); sg = function(...) sig <<- c(sig, ...)
  sg(hr)
  sg("        OPERATIONAL QUALIFICATION (OQ) REPORT")
  sg(hr)
  sg("")
  sg("Engine under test:")
  sg(sprintf("    %-14s %s", "NonCompart", pkgVer("NonCompart")))
  sg(sprintf("    %-14s %s   (driver of this report)", "ncar", pkgVer("ncar")))
  sg("")
  sg(sprintf("Report generated : %s %s", stamp, Sys.timezone()))
  sg(sprintf("Performed by     : %s", who))
  sg(sprintf("Acceptance       : symmetric relative difference <= %g", tol))
  sg("Reference         : WinNonlin 6.4 NCA outputs (Tier B, concordance)")
  sg("")
  sg(sprintf("Overall result   : %s", status))
  sg(sprintf("                   %d of %d cases passed",
             length(results) - nFailCases, length(results)))
  sg("")
  if (qualified) {
    sg(strwrap(paste("All operational test cases reproduced the pre-specified",
                     "reference values within the acceptance tolerance on this",
                     "system. Detailed per-case results follow on the next pages."),
               width=72))
  } else {
    sg(strwrap(paste("One or more cases did not meet the acceptance criterion.",
                     "See the per-case details on the following pages."), width=72))
  }
  sg(""); sg("")
  sg("Approval"); sg(hr2)
  sg("Sign this report in Adobe Acrobat Reader (no print/scan): open it,")
  sg("choose \"Use a certificate\" > \"Digitally sign\", sign with your Digital")
  sg("ID, then click the signature box under each name below. (Generate with")
  sg("sigField=TRUE, or run addSigField(), to add the boxes.) Scriptable")
  sg("alternative: signPDF()/verifyPDF().")
  sg("")
  perfRow = length(sig) + 1L
  sg("    Performed by : ____________________________   Date: __________")
  sg(""); sg(""); sg("")
  revRow = length(sig) + 1L
  sg("    Reviewed by  : ____________________________   Date: __________")
  sg(""); sg("")

  ## ----- body: case summary, per-case detail, sessionInfo -----
  body = character(0); ad = function(...) body <<- c(body, ...)
  ad("Case Summary"); ad(hr2)
  ad(sprintf(" %-4s %-28s %5s %5s %11s", "Stat", "Case", "Par", "Subj", "max rel.df"))
  ad(hr2)
  for (r in results) {
    mrd = if (is.na(r$maxRD)) "  NA" else sprintf("%.2e", r$maxRD)
    ad(sprintf(" [%-2s] %-28s %5d %5d %11s",
               if (r$status == "PASS") "OK" else "X",
               substr(r$id, 1, 28), r$nParam, r$nSubj, mrd))
  }
  ad("")
  ad("Case Details - every computed value vs its reference"); ad(hr2)
  fmtNum = function(x) if (is.na(x)) "NA" else formatC(x, format="g", digits=8)
  fmtE   = function(x) if (is.na(x)) "NA" else sprintf("%.2e", x)
  hdr = sprintf("   %-15s %12s %12s %9s %9s %2s",
                "PARAMETER", "REFERENCE", "OBSERVED", "ABS.DIFF", "REL.DIFF", "ST")
  for (r in results) {
    ad(hr)
    ad(sprintf("Case: %s   [%s]", r$id, r$status))
    ad(sprintf("   %s", substr(r$desc, 1, 66)))
    if (nzchar(r$settings))
      for (ln in strwrap(paste("Settings:", r$settings), width=72, exdent=3)) ad(ln)
    if (nzchar(r$message)) { ad(sprintf("   note: %s", r$message)); ad(""); next }
    ad(sprintf("   %d parameters x %d subjects = %d comparisons; %d failed; max rel.diff %s",
               r$nParam, r$nSubj, r$nComp, r$nFail, fmtE(r$maxRD)))
    cp = r$comp
    for (s in r$subjects) {
      ad("")
      ad(sprintf(" Subject %s", s))
      ad(hdr); ad(paste0("   ", paste(rep("-", 66), collapse="")))
      for (i in which(cp$subj == s)) {
        ab = abs(cp$ref[i] - cp$obs[i])
        ad(sprintf("   %-15s %12s %12s %9s %9s %2s",
                   substr(cp$param[i], 1, 15), fmtNum(cp$ref[i]), fmtNum(cp$obs[i]),
                   fmtE(ab), fmtE(cp$reldiff[i]), if (cp$pass[i]) "OK" else "X"))
      }
    }
    ad("")
  }
  ad("\f"); ad(hr); ad("        OVERALL RESULT"); ad(hr)
  ad(sprintf("    STATUS: %s   (%d of %d cases passed)",
             status, length(results) - nFailCases, length(results)))
  ad("\f")
  ad("Appendix A. sessionInfo()"); ad(hr2)
  ow = options(width=78)
  siLines = tryCatch(capture.output(print(sessionInfo())), error=function(e) "unavailable")
  options(ow)
  for (ln in siLines) ad(ln)

  ## ---- render: signature page first, then detail; 1-inch margins ----------
  rinfo = .qRenderPDF(fileName, sigLines=sig, bodyLines=body,
                      title="Operational Qualification Report", verdict=status,
                      footer=sprintf("ncar::pdfOQ  %s", stamp), paper=paper)
  if (isTRUE(sigField))
    try(addSigField(fileName, page=1L, fieldNames=c("Performed_by", "Reviewed_by"),
                    rects=list(.qSigRect(rinfo, perfRow), .qSigRect(rinfo, revRow))),
        silent=TRUE)

  apath = normalizePath(fileName, winslash="/", mustWork=FALSE)
  cat(sprintf("OQ report (STATUS: %s, paper: %s, %d/%d cases passed) saved at:\n  %s\n",
              status, rinfo$paper, length(results) - nFailCases, length(results), apath))
  if (isTRUE(sigField))
    cat("  Open this PDF in Adobe Acrobat Reader and click a signature field to sign.\n")
  invisible(list(fileName=apath, qualified=qualified, tol=tol, results=results,
                 paper=rinfo$paper, nCases=length(results), nFailCases=nFailCases))
}
