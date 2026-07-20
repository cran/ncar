# pdfPQ.R
# Performance Qualification (PQ) report generator for NCA.
#
# PQ demonstrates that the software performs correctly FOR THE USER'S INTENDED USE,
# with the USER'S OWN (or representative) data, under the user's procedures and
# environment. Unlike OQ (which compares the engine to fixed reference values), PQ
# runs the analysis the user actually intends to run on their data and checks that
# the result is reproducible, scientifically plausible, and (optionally) matches the
# user's pre-specified expected values, with a documented, signed record.
#
# PQ acceptance criteria and the choice of representative data are the USER's
# responsibility; this function provides the tool and sensible, configurable
# defaults. Adjust `acceptance` to match your intended use / SOP.

#-- default PQ acceptance criteria (the user should review/adjust these) --------
.pqDefaults = function()
  list(maxPctExtrap = 20,        # AUC %extrapolated (obs) must be <= this
       minR2adj     = 0.80,      # lambda_z adjusted R-squared must be >= this
       tol          = 1e-3,      # tolerance for the optional expected-value check
       reproTol     = 0,         # tolerance for the reproducibility (re-run) check
       checkTmaxRange       = TRUE,
       checkCmaxGEClast     = TRUE,
       checkAUCINFgeAUClast = TRUE,
       failOnNonEvaluable   = FALSE)  # profiles with no lambda_z: FALSE = report, do not fail

pdfPQ = function(fileName="ncar-PQ-Report.pdf", concData, key="Subject",
                 colTime="Time", colConc="conc", ..., expected=NULL,
                 acceptance=NULL, performedBy="", paper="auto", sigField=FALSE)
{
  if (is.null(acceptance)) acceptance = .pqDefaults()
  now = Sys.time(); si = Sys.info()
  concData = as.data.frame(concData)
  dots = list(...)

  ## run the intended NCA (twice, for the reproducibility check) ---------------
  runNCA = function() as.data.frame(do.call(NonCompart::tblNCA,
             c(list(concData=concData, key=key, colTime=colTime, colConc=colConc), dots)))
  R1 = tryCatch(runNCA(), error=function(e) e)
  if (inherits(R1, "error")) stop("NCA failed on the supplied data: ", conditionMessage(R1))
  R2 = runNCA()
  reproMax = 0; reproOK = TRUE
  for (cc in intersect(names(R1), names(R2))) {
    a = suppressWarnings(as.numeric(R1[[cc]])); b = suppressWarnings(as.numeric(R2[[cc]]))
    dd = abs(a - b); dd[is.na(a) & is.na(b)] = 0; dd[is.na(dd)] = Inf
    if (any(dd > acceptance$reproTol)) reproOK = FALSE
    fin = dd[is.finite(dd)]; if (length(fin)) reproMax = max(reproMax, max(fin))
  }

  ## input-data fingerprint (documents exactly which data was qualified) -------
  fp = tryCatch({
         tf = tempfile(fileext=".csv")
         utils::write.csv(concData[, unique(c(key, colTime, colConc))], tf, row.names=FALSE)
         h = unname(as.character(tools::md5sum(tf))); unlink(tf); h
       }, error=function(e) NA_character_)

  ## per-profile plausibility checks ------------------------------------------
  has = function(col) col %in% names(R1)
  g = function(col, i) if (has(col)) suppressWarnings(as.numeric(R1[[col]][i])) else NA_real_
  fmtg = function(x) if (length(x) == 0 || is.na(x)) "NA" else formatC(x, format="g", digits=5)
  profs = vector("list", nrow(R1))
  for (i in seq_len(nrow(R1))) {
    sel = rep(TRUE, nrow(concData))
    for (kc in key) sel = sel & (as.character(concData[[kc]]) == as.character(R1[[kc]][i]))
    tt = suppressWarnings(as.numeric(concData[sel & !is.na(concData[[colConc]]), colTime]))
    tmin = if (length(tt)) min(tt, na.rm=TRUE) else NA_real_
    tmax = if (length(tt)) max(tt, na.rm=TRUE) else NA_real_
    cmax=g("CMAX",i); tmaxv=g("TMAX",i); clst=g("CLST",i); auclst=g("AUCLST",i)
    aucifo=g("AUCIFO",i); pext=g("AUCPEO",i); r2=g("R2ADJ",i); hl=g("LAMZHL",i); lamz=g("LAMZ",i)
    evaluable = !is.na(lamz)
    chk = list()
    addc = function(nm, ok, info) chk[[length(chk)+1]] <<- list(nm=nm, ok=isTRUE(ok), info=info)
    addc("Cmax > 0", !is.na(cmax) && cmax > 0, paste0("Cmax=", fmtg(cmax)))
    if (isTRUE(acceptance$checkCmaxGEClast))
      addc("Cmax >= Clast", !is.na(cmax) && !is.na(clst) && cmax >= clst,
           paste0("Cmax=", fmtg(cmax), " Clast=", fmtg(clst)))
    if (isTRUE(acceptance$checkTmaxRange))
      addc("Tmax within observed time range",
           !is.na(tmaxv) && !is.na(tmin) && tmaxv >= tmin && tmaxv <= tmax,
           paste0("Tmax=", fmtg(tmaxv), " range=[", fmtg(tmin), ",", fmtg(tmax), "]"))
    addc("AUClast > 0", !is.na(auclst) && auclst > 0, paste0("AUClast=", fmtg(auclst)))
    if (evaluable) {
      if (isTRUE(acceptance$checkAUCINFgeAUClast))
        addc("AUCINF_obs >= AUClast", !is.na(aucifo) && !is.na(auclst) && aucifo >= auclst,
             paste0("AUCINF=", fmtg(aucifo), " AUClast=", fmtg(auclst)))
      addc(sprintf("AUC%%Extrap_obs <= %g", acceptance$maxPctExtrap),
           !is.na(pext) && pext <= acceptance$maxPctExtrap, paste0("%Extrap=", fmtg(pext)))
      addc(sprintf("R2adj >= %g", acceptance$minR2adj),
           !is.na(r2) && r2 >= acceptance$minR2adj, paste0("R2adj=", fmtg(r2)))
      addc("HL_Lambda_z > 0 and finite", !is.na(hl) && is.finite(hl) && hl > 0,
           paste0("HL=", fmtg(hl)))
    }
    nfail = sum(!vapply(chk, function(z) z$ok, logical(1)))
    status = if (nfail > 0) "FAIL"
             else if (!evaluable) "PASS*"      # plausible but no terminal phase
             else "PASS"
    profs[[i]] = list(id=paste(vapply(key, function(kc) as.character(R1[[kc]][i]), ""), collapse="/"),
                      evaluable=evaluable, cmax=cmax, tmaxv=tmaxv, auclst=auclst, aucifo=aucifo,
                      pext=pext, r2=r2, hl=hl, chk=chk, nfail=nfail, status=status)
  }
  nProfFail = sum(vapply(profs, function(p) p$status == "FAIL", logical(1)))
  nNonEval  = sum(vapply(profs, function(p) !p$evaluable, logical(1)))

  ## optional expected-value comparison (the user's pre-specified reference) ---
  expRows = list(); expOK = TRUE; expDone = FALSE
  if (!is.null(expected)) {
    expDone = TRUE
    expected = as.data.frame(expected)
    paramCols = setdiff(intersect(names(expected), names(R1)), key)
    for (j in seq_len(nrow(expected))) {
      sel = rep(TRUE, nrow(R1))
      for (kc in key) if (kc %in% names(expected))
        sel = sel & (as.character(R1[[kc]]) == as.character(expected[[kc]][j]))
      ri = which(sel)[1]
      if (is.na(ri)) { expRows[[length(expRows)+1]] = list(id="(unmatched)", param="-",
                          exp=NA, obs=NA, rd=NA, ok=FALSE); expOK = FALSE; next }
      for (pc in paramCols) {
        ev = suppressWarnings(as.numeric(expected[[pc]][j]))
        rv = suppressWarnings(as.numeric(R1[[pc]][ri]))
        denom = (abs(ev) + abs(rv))/2
        rd = if (is.na(ev) || is.na(rv)) NA else if (denom == 0) abs(ev-rv) else abs(ev-rv)/denom
        ok = !is.na(rd) && rd <= acceptance$tol
        if (!ok) expOK = FALSE
        expRows[[length(expRows)+1]] = list(id=paste(vapply(key, function(kc) as.character(R1[[kc]][ri]), ""), collapse="/"),
                                            param=pc, exp=ev, obs=rv, rd=rd, ok=ok)
      }
    }
  }

  qualified = reproOK && nProfFail == 0 && (!expDone || expOK) &&
              (!isTRUE(acceptance$failOnNonEvaluable) || nNonEval == 0)
  status = if (qualified) "QUALIFIED" else "NOT QUALIFIED"
  who = if (nzchar(performedBy)) performedBy else as.character(si["login"])
  stamp = format(now, "%Y-%m-%d %H:%M:%S")
  hr  = paste(rep("=", 72), collapse=""); hr2 = paste(rep("-", 72), collapse="")
  pkgVer = function(p) tryCatch(as.character(packageVersion(p)), error=function(e) "NA")
  optStr = if (length(dots)) paste(names(dots), vapply(dots, function(z) paste(deparse(z), collapse=""), ""),
                                   sep="=", collapse=", ") else "(defaults)"

  ## ---- signature / approval page (page 1) ----------------------------------
  sig = character(0); sg = function(...) sig <<- c(sig, ...)
  sg(hr); sg("        PERFORMANCE QUALIFICATION (PQ) REPORT"); sg(hr); sg("")
  sg(sprintf("Engine          : NonCompart %s (via ncar %s)", pkgVer("NonCompart"), pkgVer("ncar")))
  sg(sprintf("Report generated: %s %s", stamp, Sys.timezone()))
  sg(sprintf("Performed by    : %s", who))
  sg(sprintf("Input data      : %d rows, %d profile(s), columns %s/%s/%s",
             nrow(concData), nrow(R1), paste(key, collapse="+"), colTime, colConc))
  sg(sprintf("Data fingerprint: md5 %s", fp))
  sg("")
  sg(sprintf("Overall result  : %s", status))
  sg(sprintf("                  %d profile(s): %d pass, %d fail, %d non-evaluable",
             nrow(R1), nrow(R1) - nProfFail - 0, nProfFail, nNonEval))
  sg("")
  if (qualified)
    sg(strwrap(paste("The analysis ran reproducibly and every evaluable profile met",
                     "the pre-specified performance/plausibility criteria on this",
                     "data in this environment. PQ acceptance criteria are the user's",
                     "responsibility; confirm they match the intended use."), width=72))
  else
    sg(strwrap(paste("One or more PQ criteria were not met (see the following pages).",
                     "Review the failures against your intended use before routine use."),
               width=72))
  sg(""); sg(""); sg("Approval"); sg(hr2)
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
  sg(""); sg(""); sg("")
  sg("Detailed performance-qualification evidence follows on the next pages.")

  ## ---- body -----------------------------------------------------------------
  body = character(0); ad = function(...) body <<- c(body, ...)
  ad("1. Intended Use and Inputs"); ad(hr2)
  ad(sprintf(" Engine            : NonCompart %s (driver: ncar %s)", pkgVer("NonCompart"), pkgVer("ncar")))
  ad(sprintf(" Data fingerprint  : md5 %s", fp))
  ad(sprintf(" Rows / profiles   : %d / %d", nrow(concData), nrow(R1)))
  ad(sprintf(" Key / Time / Conc : %s / %s / %s", paste(key, collapse="+"), colTime, colConc))
  for (ln in strwrap(paste0(" tblNCA options    : ", optStr), width=72, exdent=21)) ad(ln)
  ad("")
  ad("2. PQ Acceptance Criteria (user-configurable)"); ad(hr2)
  ad(sprintf("   AUC %%Extrap_obs            <= %g %%", acceptance$maxPctExtrap))
  ad(sprintf("   lambda_z adjusted R-square >= %g", acceptance$minR2adj))
  ad(sprintf("   Tmax within observed range : %s", acceptance$checkTmaxRange))
  ad(sprintf("   Cmax >= Clast              : %s", acceptance$checkCmaxGEClast))
  ad(sprintf("   AUCINF_obs >= AUClast      : %s", acceptance$checkAUCINFgeAUClast))
  ad(sprintf("   reproducibility tolerance  : %g (re-run must match)", acceptance$reproTol))
  ad(sprintf("   expected-value tolerance   : %g (if 'expected' supplied)", acceptance$tol))
  ad("")
  ad("3. Reproducibility (analysis re-run on the same data)"); ad(hr2)
  ad(sprintf("   [%s] re-running tblNCA reproduced identical results (max diff %.2e)",
             if (reproOK) "PASS" else "FAIL", reproMax))
  ad("")
  ad("4. Per-profile Performance / Plausibility"); ad(hr2)
  ad(sprintf(" %-14s %8s %6s %9s %9s %6s %6s %6s %4s",
             "PROFILE", "Cmax", "Tmax", "AUClast", "AUCINF", "%Extr", "R2adj", "HL", "ST"))
  ad(paste0(" ", paste(rep("-", 70), collapse="")))
  for (p in profs)
    ad(sprintf(" %-14s %8s %6s %9s %9s %6s %6s %6s %4s",
               substr(p$id,1,14), fmtg(p$cmax), fmtg(p$tmaxv), fmtg(p$auclst), fmtg(p$aucifo),
               fmtg(p$pext), fmtg(p$r2), fmtg(p$hl), p$status))
  ad(""); ad(" (* = plausible but no terminal phase / lambda_z not estimated)")
  fails = Filter(function(p) p$nfail > 0, profs)
  if (length(fails) > 0) {
    ad(""); ad(" Failed checks:")
    for (p in fails) for (c in Filter(function(z) !z$ok, p$chk))
      ad(sprintf("   profile %-12s %-32s [%s]", substr(p$id,1,12), substr(c$nm,1,32), c$info))
  }
  if (expDone) {
    ad(""); ad("5. Expected-value Comparison (user reference)"); ad(hr2)
    ad(sprintf(" %-12s %-12s %12s %12s %9s %3s", "PROFILE","PARAM","EXPECTED","OBSERVED","REL.DIFF","ST"))
    ad(paste0(" ", paste(rep("-", 70), collapse="")))
    for (e in expRows)
      ad(sprintf(" %-12s %-12s %12s %12s %9s %3s", substr(e$id,1,12), substr(e$param,1,12),
                 fmtg(e$exp), fmtg(e$obs),
                 if (is.na(e$rd)) "NA" else sprintf("%.2e", e$rd), if (e$ok) "OK" else "X"))
  }
  ad("\f"); ad(hr); ad("        OVERALL RESULT"); ad(hr)
  ad(sprintf("    STATUS: %s", status))
  ad(sprintf("    reproducible=%s, profiles pass/fail/non-eval = %d/%d/%d%s",
             reproOK, nrow(R1)-nProfFail, nProfFail, nNonEval,
             if (expDone) sprintf(", expected-values %s", if (expOK) "match" else "DIFFER") else ""))
  ad("\f"); ad("Appendix A. sessionInfo()"); ad(hr2)
  ow = options(width=78)
  siLines = tryCatch(capture.output(print(sessionInfo())), error=function(e) "unavailable")
  options(ow)
  for (ln in siLines) ad(ln)

  rinfo = .qRenderPDF(fileName, sigLines=sig, bodyLines=body,
                      title="Performance Qualification Report", verdict=status,
                      footer=sprintf("ncar::pdfPQ  %s", stamp), paper=paper)
  if (isTRUE(sigField))
    try(addSigField(fileName, page=1L, fieldNames=c("Performed_by", "Reviewed_by"),
                    rects=list(.qSigRect(rinfo, perfRow), .qSigRect(rinfo, revRow))),
        silent=TRUE)

  apath = normalizePath(fileName, winslash="/", mustWork=FALSE)
  cat(sprintf("PQ report (STATUS: %s, paper: %s) saved at:\n  %s\n", status, rinfo$paper, apath))
  if (isTRUE(sigField))
    cat("  Open this PDF in Adobe Acrobat Reader and click a signature field to sign.\n")
  invisible(list(fileName=apath, qualified=qualified, paper=rinfo$paper, results=R1,
                 reproducible=reproOK, profiles=profs, fingerprint=fp,
                 nProfFail=nProfFail, nNonEval=nNonEval,
                 expected=if (expDone) expRows else NULL))
}
