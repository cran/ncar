# QualReport.R
# Shared PDF renderer for the qualification reports (pdfIQ, pdfOQ).
# - 1 inch margins on every side
# - paper size: 'letter' for the United States, 'A4' elsewhere (auto by locale),
#   overridable via the paper argument
# - the signature (approval) page is rendered FIRST, the detailed evidence follows
# Uses only base R graphics (monospace Courier), so no LaTeX/pandoc is needed.

#-- internal: is this a United States locale? -----------------------------------
.qIsUS = function()
{
  s = paste(Sys.getlocale(), Sys.getenv("LANG"), Sys.getenv("LANGUAGE"),
            Sys.getenv("LC_ALL"), Sys.getenv("LC_CTYPE"), sep=";")
  if (grepl("United States|English_United|en[_-]US|\\bUSA?\\b", s, ignore.case=TRUE))
    return(TRUE)
  # fall back to R's configured paper size when the locale is uninformative (C/POSIX)
  if (!grepl("[A-Za-z]", gsub("[;C.UTF8POSIX -]", "", s)))
    return(identical(tolower(getOption("papersize", "")), "letter"))
  FALSE
}

#-- internal: resolve paper to name + size in inches ----------------------------
.qPaper = function(paper="auto")
{
  paper = tolower(paper)
  if (paper == "auto") paper = if (.qIsUS()) "letter" else "a4"
  if (paper == "letter") return(list(name="letter", w=8.5, h=11.0))
  return(list(name="a4", w=210/25.4, h=297/25.4))   # A4 = 210 x 297 mm
}

#-- internal: render signature page + body to a paginated PDF -------------------
# sigLines : character vector for page 1 (the approval/signature page)
# bodyLines: character vector for the remaining pages (detailed evidence)
.qRenderPDF = function(fileName, sigLines, bodyLines, title="", verdict="",
                       footer="", paper="auto", margin=1, pointsize=9)
{
  pg = .qPaper(paper)
  cw = 0.6 * pointsize / 72                 # Courier advance width (inch)
  lh = 1.2 * pointsize / 72                 # line height (inch)
  pw = pg$w - 2 * margin                     # printable width (inch)
  ph = pg$h - 2 * margin                      # printable height (inch)
  nCol = floor(pw / cw)
  nRow = floor(ph / lh)

  # Everything is drawn INSIDE the 1-inch margins (nothing in the margin itself).
  # The bottom printable row of every page is reserved for a footer line.
  footRow = nRow
  perPage = nRow - 2                                          # rows for content
  # paginate body honoring explicit page breaks ("\f") and the perPage limit
  bodyPages = list(); cur = character(0)
  for (ln in bodyLines) {
    if (identical(ln, "\f")) {                                # forced new page
      if (length(cur) > 0) { bodyPages[[length(bodyPages) + 1]] = cur; cur = character(0) }
      next
    }
    if (length(cur) >= perPage) { bodyPages[[length(bodyPages) + 1]] = cur; cur = character(0) }
    cur = c(cur, ln)
  }
  if (length(cur) > 0) bodyPages[[length(bodyPages) + 1]] = cur
  pages = c(list(sigLines), bodyPages)
  nPages = length(pages)

  pdf(fileName, paper="special", width=pg$w, height=pg$h,
      pointsize=pointsize, family="Courier", title=title)
  on.exit(dev.off())

  for (p in seq_len(nPages)) {
    par(omi=c(0, 0, 0, 0), mai=c(margin, margin, margin, margin), adj=0, xpd=NA,
        xaxs="i", yaxs="i")                                  # no axis padding
    plot.new()
    plot.window(xlim=c(0, nCol), ylim=c(nRow + 0.5, 0.5))   # row 1 at top, exact
    ln = pages[[p]]
    if (length(ln) > perPage) ln = ln[seq_len(perPage)]      # safety clamp
    for (i in seq_along(ln)) text(0, i, ln[i], adj=c(0, 0.5))
    # footer on the bottom printable row (inside the margins, not in them)
    foot = if (nzchar(footer)) footer else title
    if (nzchar(foot)) text(0, footRow, foot, adj=c(0, 0.5))
    text(nCol, footRow, sprintf("Page %d of %d", p, nPages), adj=c(1, 0.5))
  }

  invisible(list(paper=pg$name, nPages=nPages, nCol=nCol, nRow=nRow,
                 pointsize=pointsize, margin=margin, Hin=pg$h, cw=cw, lh=lh))
}

# internal: PDF-point rectangle for a signature box placed just below sig-page row
# 'line' (1-based row index on page 1), spanning the next 'rows' rows. Uses the
# geometry returned by .qRenderPDF so it matches the rendered text exactly.
.qSigRect = function(g, line, rows=3, col0=19, col1=47) {
  rowY = function(i) (g$Hin - g$margin) * 72 - ((i - 0.5) / g$nRow) * (g$Hin - 2 * g$margin) * 72
  cwpt = g$cw * 72
  x0 = g$margin * 72 + col0 * cwpt
  x1 = g$margin * 72 + col1 * cwpt
  c(x0, rowY(line + rows + 0.3), x1, rowY(line + 0.6))      # c(x0, ylow, x1, yhigh)
}
