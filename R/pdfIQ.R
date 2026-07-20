# pdfIQ.R
# Installation Qualification (IQ) report generator for the 'ncar' / 'NonCompart' stack.
# Produces a self-contained PDF certifying that the package(s) are correctly
# installed, intact, loadable, and operational in the user's own R environment.
# Uses only base R + the package's own PDF helpers (PrepPDF/AddPage/TextM/ClosePDF
# in BasicUtil.R), so it needs NO LaTeX, pandoc, or other external tools.

#-- internal: relative-difference comparator (mirrors tests/Test.R Equal()) -----
.iqClose = function(observed, reference, tol=1e-3)
{
  observed = as.numeric(observed)
  reference = as.numeric(reference)
  if (is.na(observed) || is.na(reference)) return(FALSE)
  if (reference == 0) return(abs(observed) <= tol)
  return(abs((observed - reference)/reference) <= tol)
}

#-- internal: parse a DESCRIPTION Depends/Imports string into name/op/version ---
.iqParseDeps = function(str)
{
  if (is.null(str) || is.na(str) || !nzchar(str)) return(list())
  parts = trimws(strsplit(str, ",")[[1]])
  out = list()
  for (p in parts) {
    m = regmatches(p, regexec("^([A-Za-z][A-Za-z0-9.]*)[[:space:]]*(\\([[:space:]]*([<>=!]+)[[:space:]]*([0-9.-]+)[[:space:]]*\\))?", p))[[1]]
    if (length(m) >= 2 && nzchar(m[2])) {
      out[[length(out) + 1]] = list(name=m[2],
                                    op=if (length(m) >= 4) m[4] else "",
                                    ver=if (length(m) >= 5) m[5] else "")
    }
  }
  return(out)
}

#-- main ------------------------------------------------------------------------
pdfIQ = function(fileName="ncar-IQ-Report.pdf", pkgs=c("ncar", "NonCompart"),
                 functional=TRUE, performedBy="", paper="auto", sigField=FALSE)
{
  TOL = 1e-3                                  # OQ-style relative tolerance
  now = Sys.time()
  si  = Sys.info()

  # checks accumulator: Section, Item, Result, Status (PASS/FAIL/WARN/INFO)
  chk = data.frame(Section=character(0), Item=character(0),
                   Result=character(0), Status=character(0),
                   stringsAsFactors=FALSE)
  add = function(section, item, result, status="INFO") {
    chk[nrow(chk) + 1, ] <<- list(section, item, as.character(result), status)
  }

  expectedExports = list(
    ncar       = c("txtNCA", "pdfNCA", "rtfNCA", "Res2Txt", "Round"),
    NonCompart = c("tblNCA", "sNCA", "AUC", "IntAUC", "Unit"))

  ## 1. Test environment ------------------------------------------------------
  S = "1. Test Environment"
  add(S, "R version", R.version.string)
  add(S, "Platform", R.version$platform)
  add(S, "Operating system", paste(si["sysname"], si["release"]))
  add(S, "Machine / node", paste(si["machine"], "/", si["nodename"]))
  add(S, "User (login)", si["login"])
  add(S, "Locale (LC_CTYPE)", Sys.getlocale("LC_CTYPE"))
  libs = .libPaths()
  for (i in seq_along(libs)) add(S, paste0("Library path [", i, "]"), libs[i])

  ## 2. Package installation & versions --------------------------------------
  S = "2. Package Installation and Versions"
  allOk = TRUE
  for (p in pkgs) {
    inst = requireNamespace(p, quietly=TRUE)
    if (!inst) {
      add(S, paste0(p, ": installed"), "NOT FOUND", "FAIL"); allOk = FALSE; next
    }
    v    = tryCatch(as.character(packageVersion(p)), error=function(e) "unknown")
    path = tryCatch(find.package(p), error=function(e) "unknown")
    add(S, paste0(p, ": version"), v, "PASS")
    add(S, paste0(p, ": location"), path, "INFO")

    # declared dependency presence + version satisfaction
    basePkgs = c("base","utils","graphics","grDevices","stats","methods",
                 "datasets","tools","grid","splines","stats4","tcltk",
                 "compiler","parallel")
    deps = c(.iqParseDeps(packageDescription(p)$Depends),
             .iqParseDeps(packageDescription(p)$Imports))
    for (d in deps) {
      if (identical(d$name, "R")) {            # R version constraint
        if (!nzchar(d$ver)) next
        ok = do.call(d$op, list(getRversion(), d$ver))
        add(S, paste0(p, " requires R ", d$op, " ", d$ver),
            paste("found", as.character(getRversion())),
            if (isTRUE(ok)) "PASS" else "FAIL")
        if (!isTRUE(ok)) allOk = FALSE
        next
      }
      if (d$name %in% basePkgs) next           # base pkgs guaranteed by R itself
      if (!requireNamespace(d$name, quietly=TRUE)) {
        lbl = if (nzchar(d$ver)) paste0(" ", d$op, " ", d$ver) else ""
        add(S, paste0(p, " requires ", d$name, lbl),
            "dependency NOT installed", "FAIL"); allOk = FALSE; next
      }
      dv = packageVersion(d$name)
      if (nzchar(d$ver)) {                      # has a version constraint
        ok = do.call(d$op, list(dv, d$ver))
        add(S, paste0(p, " requires ", d$name, " ", d$op, " ", d$ver),
            paste("found", as.character(dv)),
            if (isTRUE(ok)) "PASS" else "FAIL")
        if (!isTRUE(ok)) allOk = FALSE
      } else {                                  # presence only
        add(S, paste0(p, " depends on ", d$name),
            paste("found", as.character(dv)), "PASS")
      }
    }
  }

  ## 3. File integrity (MD5 checksums) ---------------------------------------
  S = "3. File Integrity (MD5 checksums)"
  for (p in pkgs) {
    if (!requireNamespace(p, quietly=TRUE)) next
    dirp = find.package(p)
    nfile = length(setdiff(list.files(dirp, recursive=TRUE, all.files=TRUE,
                                      no.. = TRUE), "MD5"))
    md5 = tryCatch(checkMD5sums(p, dirp), error=function(e) NA)
    if (isTRUE(md5)) {
      add(S, paste0(p, ": checkMD5sums"),
          sprintf("all %d installed files match the MD5 manifest", nfile), "PASS")
    } else if (isTRUE(!md5)) {
      add(S, paste0(p, ": checkMD5sums"),
          "MISMATCH - one or more files differ from the MD5 manifest", "FAIL")
      allOk = FALSE
    } else {
      add(S, paste0(p, ": checkMD5sums"),
          paste0("no MD5 manifest in this install - run ncar::writeMD5(\"", p,
                 "\") once after installation (CRAN installs include it)"), "WARN")
    }
  }

  ## 4. Load test and exported functions -------------------------------------
  S = "4. Namespace Load and Exported Functions"
  for (p in pkgs) {
    loaded = tryCatch({ requireNamespace(p, quietly=TRUE) }, error=function(e) FALSE)
    add(S, paste0(p, ": namespace loads"),
        if (loaded) "OK" else "FAILED", if (loaded) "PASS" else "FAIL")
    if (!loaded) { allOk = FALSE; next }
    exps = getNamespaceExports(p)
    want = expectedExports[[p]]
    if (is.null(want)) {
      add(S, paste0(p, ": exported objects"), paste(length(exps), "objects exported"),
          if (length(exps) > 0) "PASS" else "FAIL")
    } else {
      missing = setdiff(want, exps)
      if (length(missing) == 0) {
        add(S, paste0(p, ": core exports present"),
            paste(want, collapse=", "), "PASS")
      } else {
        add(S, paste0(p, ": core exports present"),
            paste("MISSING:", paste(missing, collapse=", ")), "FAIL")
        allOk = FALSE
      }
    }
  }

  ## 5. Functional verification (operational smoke + control tests) ----------
  if (isTRUE(functional) && requireNamespace("NonCompart", quietly=TRUE)) {
    S = "5. Functional Verification"

    # Reference values: WinNonlin 6.4 NCA, Theophylline Subject 1
    # (Linear-up Linear-down, extravascular, dose=320). External, verified.
    x = c(0, 0.25, 0.57, 1.12, 2.02, 3.82, 5.10, 7.03, 9.05, 12.12, 24.37)
    y = c(0.74, 2.84, 6.57, 10.50, 9.66, 8.58, 8.36, 7.47, 6.89, 5.94, 3.28)
    ref = c(CMAX=10.5000, TMAX=1.1200, AUCLST=148.9231,
            AUCIFO=216.6119, LAMZHL=14.3044)
    res = tryCatch(NonCompart::sNCA(x, y, dose=320, concUnit="mg/L"),
                   error=function(e) NULL)
    if (is.null(res)) {
      add(S, "NCA smoke test (Theoph Subj 1)", "sNCA() raised an error", "FAIL")
      allOk = FALSE
    } else {
      for (k in names(ref)) {
        obs = suppressWarnings(as.numeric(res[k]))
        ok  = .iqClose(obs, ref[k], TOL)
        add(S, paste0("Theoph Subj 1: ", k),
            sprintf("obs=%.4f  ref=%.4f", obs, ref[k]),
            if (ok) "PASS" else "FAIL")
        if (!ok) allOk = FALSE
      }
    }

    # Control tests: prove the comparator itself works (WinNonlin Validation
    # Suite has the same 2 controls: a known-match must PASS, a known-differ
    # must FAIL).
    c1 = .iqClose(1.234567, 1.234567, TOL)            # identical -> TRUE
    add(S, "Control 1: identical values compare equal",
        if (c1) "comparator returned EQUAL" else "comparator failed",
        if (isTRUE(c1)) "PASS" else "FAIL")
    if (!isTRUE(c1)) allOk = FALSE

    c2 = .iqClose(1.00, 1.10, TOL)                     # 10% diff -> FALSE
    add(S, "Control 2: 10% different values flagged",
        if (!c2) "comparator returned DIFFERENT" else "comparator failed",
        if (isFALSE(c2)) "PASS" else "FAIL")
    if (!isFALSE(c2)) allOk = FALSE
  }

  ## ---- assemble report text (page 1 = signatures, then detail pages) --------
  hr  = paste(rep("=", 72), collapse="")
  hr2 = paste(rep("-", 72), collapse="")
  pkgVer = function(p) tryCatch(as.character(packageVersion(p)),
                                error=function(e) "not installed")

  nFail = sum(chk$Status == "FAIL")
  nWarn = sum(chk$Status == "WARN")
  nPass = sum(chk$Status == "PASS")
  qualified = isTRUE(allOk) && nFail == 0
  status = if (qualified) "QUALIFIED" else "NOT QUALIFIED"
  who    = if (nzchar(performedBy)) performedBy else as.character(si["login"])
  stamp  = format(now, "%Y-%m-%d %H:%M:%S")

  ## ----- page 1: signature / approval page -----
  sig = character(0); sg = function(...) sig <<- c(sig, ...)
  sg(hr)
  sg("        INSTALLATION QUALIFICATION (IQ) REPORT")
  sg(hr)
  sg("")
  sg("Qualified package(s):")
  for (p in pkgs) sg(sprintf("    %-14s %s", p, pkgVer(p)))
  sg("")
  sg(sprintf("Report generated : %s %s", stamp, Sys.timezone()))
  sg(sprintf("Generated by     : ncar::pdfIQ(), ncar %s", pkgVer("ncar")))
  sg(sprintf("Performed by     : %s", who))
  sg("")
  sg(sprintf("Overall result   : %s", status))
  sg(sprintf("                   %d PASS, %d FAIL, %d WARN", nPass, nFail, nWarn))
  sg("")
  if (qualified) {
    sg(strwrap(paste("Based on the checks documented on the following pages, the",
                     "package(s) listed are correctly installed, intact, loadable,",
                     "and operational in this R environment as of the date above."),
               width=72))
  } else {
    sg(strwrap(paste("One or more checks failed (see the following pages). The",
                     "installation is NOT qualified until the failures are resolved",
                     "and this report is re-generated."), width=72))
  }
  if (nWarn > 0) {
    sg("")
    sg(strwrap(paste("Note: [WARN] items did not fail qualification but should be",
                     "reviewed (e.g. a missing MD5 manifest means file integrity",
                     "could not be cryptographically verified for that package)."),
               width=72))
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
  sg(""); sg(""); sg("")
  sg("Detailed qualification evidence follows on the subsequent pages.")

  ## ----- body: detailed evidence + sessionInfo -----
  body = character(0); ad = function(...) body <<- c(body, ...)
  wrapVal = function(prefix, value, contIndent) {     # wrap long values to 72 cols
    width = 72
    avail = width - nchar(prefix); if (avail < 8) avail = 8
    if (nchar(value) <= avail) return(paste0(prefix, value))
    chunks = character(0)
    while (nchar(value) > 0) {
      take = min(avail, nchar(value))
      chunks = c(chunks, substr(value, 1, take))
      value  = substr(value, take + 1, nchar(value))
      avail  = width - contIndent
    }
    pad = paste(rep(" ", contIndent), collapse="")
    c(paste0(prefix, chunks[1]),
      if (length(chunks) > 1) paste0(pad, chunks[-1]))
  }
  for (S in unique(chk$Section)) {
    ad(""); ad(S); ad(hr2)
    sub = chk[chk$Section == S, ]
    for (i in seq_len(nrow(sub))) {
      prefix = sprintf(" [%-4s] %-36s : ", sub$Status[i], substr(sub$Item[i], 1, 36))
      for (ln in wrapVal(prefix, sub$Result[i], nchar(prefix))) ad(ln)
    }
  }
  ad("\f"); ad(hr); ad("        OVERALL RESULT"); ad(hr)
  ad(sprintf("    STATUS: %s   (%d PASS, %d FAIL, %d WARN)", status, nPass, nFail, nWarn))
  ad("\f")
  ad("Appendix A. sessionInfo()"); ad(hr2)
  ow = options(width=78)
  siLines = tryCatch(capture.output(print(sessionInfo())),
                     error=function(e) "sessionInfo() unavailable")
  options(ow)
  for (ln in siLines) ad(ln)

  ## ----- Appendix B: per-file md5 checksums (full integrity evidence) -----
  ad("\f")
  ad("Appendix B. Installed File Checksums (md5)"); ad(hr2)
  ad("md5sum of every installed file, computed in this environment.")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly=TRUE)) next
    dirp = find.package(p)
    files = setdiff(list.files(dirp, recursive=TRUE, all.files=TRUE, no.. = TRUE), "MD5")
    sums = tryCatch(md5sum(file.path(dirp, files)), error=function(e) rep(NA, length(files)))
    ad(""); ad(sprintf("%s %s  (%d files)", p, pkgVer(p), length(files)))
    pth = paste0("at ", dirp)
    while (nchar(pth) > 0) { ad(substr(pth, 1, 72)); pth = substr(pth, 73, nchar(pth)) }
    ad(paste(rep("-", 72), collapse=""))
    for (i in order(files)) {
      s = if (is.na(sums[i])) paste(rep("?", 32), collapse="") else sums[i]
      f = files[i]; if (nchar(f) > 38) f = paste0("...", substr(f, nchar(f) - 34, nchar(f)))
      ad(sprintf("%s  %s", s, f))
    }
  }

  ## ---- render: signature page first, then detail; 1-inch margins ----------
  rinfo = .qRenderPDF(fileName, sigLines=sig, bodyLines=body,
                      title="Installation Qualification Report", verdict=status,
                      footer=sprintf("ncar::pdfIQ  %s", stamp), paper=paper)
  if (isTRUE(sigField))
    try(addSigField(fileName, page=1L, fieldNames=c("Performed_by", "Reviewed_by"),
                    rects=list(.qSigRect(rinfo, perfRow), .qSigRect(rinfo, revRow))),
        silent=TRUE)

  apath = normalizePath(fileName, winslash="/", mustWork=FALSE)
  cat(sprintf("IQ report (STATUS: %s, paper: %s) saved at:\n  %s\n",
              status, rinfo$paper, apath))
  if (isTRUE(sigField))
    cat("  Open this PDF in Adobe Acrobat Reader and click a signature field to sign.\n")

  invisible(list(fileName=apath, qualified=qualified, paper=rinfo$paper,
                 checks=chk, nPass=nPass, nFail=nFail, nWarn=nWarn))
}
