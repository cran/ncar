pdfNCA = function(fileName="Temp-NCA.pdf", concData, key = "Subject",
                  colTime="Time", colConc="conc", dose=0, adm="Extravascular",
                  dur=0, doseUnit="mg", timeUnit="h", concUnit="ug/L",
                  down="Linear", R2ADJ=0, MW=0, SS=FALSE, iAUC="",
                  excludeDelta=1, UsePoints=NULL, performedBy="")
{
  class(concData) = "data.frame"
  defPar = par(no.readonly=TRUE)

  PrepPDF(fileName)
  AddPage()
  Text1(1, 1, "Individual Noncompartmental Analysis Result", Cex=1.2)
  Text1(3, 1, paste0("ncar ", packageVersion("ncar"), "    ", R.version.string), Cex=0.8)
  Text1(4, 1, paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), Sys.timezone()), Cex=0.8)
  if (nzchar(performedBy)) Text1(5, 1, paste("Performed by:", performedBy), Cex=0.8)
  Text1(7, 1, "Approval", Cex=0.8)
  Text1(8, 1, paste(rep("-", 70), collapse=""), Cex=0.8)
  Text1(10, 1, "Performed by : ______________________________   Date: ____________", Cex=0.8)
  Text1(12, 1, "Reviewed by  : ______________________________   Date: ____________", Cex=0.8)

  maxx = max(concData[, colTime], na.rm=TRUE)
  maxy = max(concData[, colConc], na.rm=TRUE)
  miny = min(concData[concData[, colConc] > 0, colConc], na.rm=TRUE)

  nKey = length(key)
  IDs = unique(as.data.frame(concData[, key], ncol=nKey))
  nID = nrow(IDs)

  if (length(dose) == 1) {
    dose = rep(dose, nID)
  } else if (length(dose) != nID) {
    stop("Count of dose does not match with number of NCAs!")
  }

  if (length(dur) == 1) {
    dur = rep(dur, nID)
  } else if (length(dur) != nID) {
    stop("Count of dur does not match with number of NCAs!")
  }

  if (!is.null(UsePoints)) {
    if (length(UsePoints) != nID) stop("Length of UsePoints does not match the number of NCAs!")
    R2ADJ = 0 # suppress R2ADJ option when used with UsePoints
  }

  Res = vector()
  for (i in 1:nID) {
    strHeader = paste0(key[1], "=", IDs[i, 1])
    tData = concData[concData[[key[1]]] == IDs[i, 1], , drop=FALSE]
    if (nKey > 1) {
      for (j in 2:nKey) {
        strHeader = paste0(strHeader, ", ", key[j], "=", IDs[i, j])
        tData = tData[tData[[key[j]]] == IDs[i, j], , drop=FALSE]
      }
    }
    if (nrow(tData) > 0) {
      if (!is.null(UsePoints)) {
        tUse = UsePoints[[i]]
      } else {
        tUse = NULL
      }
      x = tData[, colTime]
      y = tData[, colConc]
      tabRes = sNCA(x, y, dose=dose[i], adm=adm, dur=dur[i], doseUnit=doseUnit, timeUnit=timeUnit, concUnit=concUnit, down=down, R2ADJ=R2ADJ, MW=MW, SS=SS, iAUC=iAUC, Keystring=strHeader, excludeDelta=excludeDelta, UsePoints=tUse)
      UsedPoints = attr(tabRes, "UsedPoints")
      txtRes = Res2Txt(tabRes, x, y, dose=dose[i], adm=adm, dur=dur[i], doseUnit=doseUnit, down=down)
      Res = c(Res, txtRes)

      AddPage(Header1=strHeader)
      TextM(txtRes, StartRow=1, Header1=strHeader)

      scrnmat = matrix(0, 3, 4)
      scrnmat[1,] = c(0, 1, 0, 1)
      scrnmat[2,] = c(0.1, 0.9, 0.50, 0.95)
      scrnmat[3,] = c(0.1, 0.9, 0.05, 0.50)
      ScrNo = split.screen(scrnmat)

      screen(ScrNo[1])
      par(adj=0)
      Text1(1, 1, strHeader, Cex=1.0)

      screen(ScrNo[2])
      par(oma=c(1,1,1,1), mar=c(4,4,3,1), adj=0.5)
      plot(x, y, type="b", cex=0.7, xlim=c(0,maxx), ylim=c(0,maxy), xlab=paste0("Time (", timeUnit, ")"), ylab=paste0("Concentration (", concUnit, ")"))

      screen(ScrNo[3])
      par(oma=c(1,1,1,1), mar=c(4,4,3,1), adj=0.5)
      x0 = x[!is.na(y) & y > 0]
      y0 = y[!is.na(y) & y > 0]
      if (length(x0) > 0) {
        plot(x0, log10(y0), type="b", cex=0.7, xlim=c(0, maxx), ylim=c(log10(miny), log10(maxy)), yaxt="n", xlab=paste0("Time (", timeUnit, ")"), ylab=paste0("Concentration (log interval) (", concUnit, ")"))
        points(x[UsedPoints], log10(y[UsedPoints]), pch=16)
        yticks = seq(round(min(log10(y0))), ceiling(max(log10(y0))))
        ylabels = sapply(yticks, function(i) as.expression(bquote(10^ .(i))))
        axis(2, at=yticks, labels=ylabels)
        x1 = tabRes["LAMZLL"]
        x2 = tabRes["LAMZUL"]
        deltaX = x1 * 0.05
        y1 = log10(exp(1))*(tabRes["b0"] - tabRes["LAMZ"] * (x1 - deltaX))
        y2 = log10(exp(1))*(tabRes["b0"] - tabRes["LAMZ"] * (x2 + deltaX))
        lines(c(x1 - deltaX, x2 + deltaX), c(y1, y2), lty=2, col="red")
      }
      close.screen(all.screens=TRUE)
    }
  }
  par(defPar)
  ClosePDF()
}
