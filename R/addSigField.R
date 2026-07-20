# addSigField.R
# Add empty AcroForm digital-signature field(s) to a PDF so it can be signed in
# Adobe Acrobat Reader (the free reader) with one click: open the PDF, click a
# field, and sign with a Digital ID. The field is inserted with a pure base-R
# PDF incremental update (no external tools or packages), so it works wherever R
# does. Designed for the simple PDFs produced by pdfIQ()/pdfOQ() (base R pdf()).
#
# Acrobat Reader can also sign a PDF that has NO field ("All tools" ->
# "Use a certificate" -> "Digitally sign" -> draw a box). This function just makes
# the workflow click-to-sign by pre-placing labelled signature fields.

addSigField = function(pdf, out=pdf, page=1L,
                       fieldNames=c("Performed_by", "Reviewed_by"), rects=NULL)
{
  if (!file.exists(pdf)) stop("PDF not found: ", pdf)
  raw = readBin(pdf, "raw", file.info(pdf)$size)
  n = length(raw)
  asc = function(a, b) rawToChar(raw[a:b])               # ASCII slice (1-based)

  objRange = function(num) {                              # [start,end] of "\n<num> 0 obj"..."endobj"
    hits = grepRaw(charToRaw(paste0("\n", num, " 0 obj")), raw, all=TRUE)
    if (length(hits) == 0) stop("object not found: ", num)
    s = hits[length(hits)] + 1L
    e = grepRaw(charToRaw("endobj"), raw, offset=s, all=FALSE)
    if (length(e) == 0) stop("endobj not found for object ", num)
    c(s, e + 5L)
  }

  toff = grepRaw(charToRaw("trailer"), raw, all=TRUE)
  if (length(toff) == 0) stop("no trailer: unsupported PDF (cross-reference streams not handled).")
  ttxt = asc(toff[length(toff)], n)
  Size = as.integer(sub(".*?/Size\\s+(\\d+).*", "\\1", ttxt))
  Root = as.integer(sub(".*?/Root\\s+(\\d+)\\s+0\\s+R.*", "\\1", ttxt))
  Info = if (grepl("/Info", ttxt)) as.integer(sub(".*?/Info\\s+(\\d+)\\s+0\\s+R.*", "\\1", ttxt)) else NA
  sx = grepRaw(charToRaw("startxref"), raw, all=TRUE)
  oldStartxref = as.integer(sub("^startxref\\s+(\\d+).*", "\\1", asc(sx[length(sx)], n)))

  cr = objRange(Root); catTxt = asc(cr[1], cr[2])
  if (grepl("/AcroForm", catTxt)) stop("PDF already has an AcroForm; not modifying.")
  pagesNum = as.integer(sub(".*?/Pages\\s+(\\d+)\\s+0\\s+R.*", "\\1", catTxt))
  pr = objRange(pagesNum); pagesTxt = asc(pr[1], pr[2])
  kids = regmatches(pagesTxt, regexpr("/Kids\\s*\\[[^]]*\\]", pagesTxt))
  kidNums = as.integer(regmatches(kids, gregexpr("\\d+(?=\\s+0\\s+R)", kids, perl=TRUE))[[1]])
  if (page < 1 || page > length(kidNums)) stop("page out of range (1..", length(kidNums), ")")
  pageNum = kidNums[page]
  prng = objRange(pageNum); pageTxt = asc(prng[1], prng[2])

  mbsrc = if (grepl("/MediaBox", pageTxt)) pageTxt else pagesTxt
  mb = as.numeric(strsplit(trimws(gsub("[^0-9. ]", " ",
         regmatches(mbsrc, regexpr("/MediaBox\\s*\\[[^]]*\\]", mbsrc)))), "\\s+")[[1]])
  mb = mb[!is.na(mb)]; W = mb[3]; H = mb[4]

  nf = length(fieldNames)
  if (is.null(rects)) {                                   # default: stacked boxes, lower area
    x0 = 72; x1 = min(W - 72, 72 + 260)
    base = list(c(x0, 250, x1, 292), c(x0, 188, x1, 230))
    if (nf > length(base))
      for (k in (length(base)+1):nf) base[[k]] = c(x0, 188 - (k-2)*62, x1, 230 - (k-2)*62)
    rects = base[seq_len(nf)]
  }
  newNums = Size + seq_len(nf) - 1L
  refs = paste(paste0(newNums, " 0 R"), collapse=" ")

  pos = n; pieces = character(0)
  emit = function(txt) { o = pos; pieces[[length(pieces)+1]] <<- txt
                         pos <<- pos + nchar(txt, type="bytes"); o }
  widgetOffs = integer(nf)
  for (i in seq_len(nf)) {
    r = rects[[i]]
    widgetOffs[i] = emit(sprintf(paste0("%d 0 obj\n<< /Type /Annot /Subtype /Widget /FT /Sig ",
                                        "/T (%s) /Rect [%g %g %g %g] /P %d 0 R /F 4 ",
                                        "/BS << /W 1 /S /S >> /MK << /BC [0 0 0] >> >>\nendobj\n"),
                                 newNums[i], fieldNames[i], r[1], r[2], r[3], r[4], pageNum))
  }
  catNew = sub(">>\\s*endobj\\s*$",
               sprintf("/AcroForm << /Fields [%s] /SigFlags 3 >> >>\nendobj\n", refs), catTxt)
  catOff = emit(catNew)
  if (grepl("/Annots", pageTxt))
    pageNew = sub("/Annots\\s*\\[", paste0("/Annots [", refs, " "), pageTxt)
  else
    pageNew = sub(">>\\s*endobj\\s*$", sprintf("/Annots [%s] >>\nendobj\n", refs), pageTxt)
  if (!grepl("endobj\\s*$", pageNew)) pageNew = paste0(pageNew, "\n")
  pageOff = emit(pageNew)

  xrefOff = pos
  offMap = c(); offMap[as.character(Root)] = catOff; offMap[as.character(pageNum)] = pageOff
  for (k in seq_len(nf)) offMap[as.character(newNums[k])] = widgetOffs[k]
  chg = sort(c(Root, pageNum, newNums))
  xref = "xref\n"; i = 1
  while (i <= length(chg)) {
    j = i; while (j < length(chg) && chg[j+1] == chg[j] + 1) j = j + 1
    run = chg[i:j]
    xref = paste0(xref, sprintf("%d %d\n", run[1], length(run)),
                  paste(sprintf("%010d 00000 n \n", offMap[as.character(run)]), collapse=""))
    i = j + 1
  }
  newSize = max(Size, max(newNums) + 1L)
  trailer = sprintf("trailer\n<< /Size %d /Root %d 0 R%s /Prev %d >>\nstartxref\n%d\n%%%%EOF\n",
                    newSize, Root, if (!is.na(Info)) sprintf(" /Info %d 0 R", Info) else "",
                    oldStartxref, xrefOff)

  writeBin(c(raw, charToRaw(paste0(paste(pieces, collapse=""), xref, trailer))), out)
  apath = normalizePath(out, winslash="/", mustWork=FALSE)
  cat(sprintf("Added %d Acrobat signature field(s) [%s] to page %d of:\n  %s\n",
              nf, paste(fieldNames, collapse=", "), page, apath))
  invisible(apath)
}
