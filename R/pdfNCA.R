pdfNCA = function(fileName="Temp-NCA.pdf", concData, key = "Subject", 
                  colTime="Time", colConc="conc", dose=0, adm="Extravascular", 
                  dur=0, doseUnit="mg", timeUnit="h", concUnit="ug/L", 
                  down="Linear", R2ADJ=0, MW=0, SS=FALSE, iAUC="", 
                  excludeDelta=1)
{
  class(concData) = "data.frame"
  defPar = par(no.readonly=TRUE)

  PrepPDF(fileName)
  AddPage()
  Text1(1, 1, "Individual Noncompartmental Analysis Result", Cex=1.2)

  maxx = max(concData[,colTime], na.rm=TRUE)
  maxy = max(concData[,colConc], na.rm=TRUE)
  miny = min(concData[concData[,colConc]>0, colConc], na.rm=TRUE)

  nKey = length(key)
  IDs = unique(as.data.frame(concData[,key], ncol=nKey))
  nID = nrow(IDs)

  if (length(dose) == 1) {
    dose = rep(dose, nID)
  } else if (length(dose) != nID) {
    stop("Count of dose does not match with number of NCAs!")
  }

  Res = vector()
  for (i in 1:nID) {
    strHeader = paste0(key[1], "=", IDs[i, 1])
    strCond = paste0("concData[concData$", key[1], "=='", IDs[i, 1], "'")
    if (nKey > 1) {
      for (j in 2:nKey) {
        strCond = paste0(strCond, " & concData$", key[j], "=='", IDs[i,j], "'")
        strHeader = paste0(strHeader, ", ", key[j], "=", IDs[i,j])
      }
    }
    strCond = paste0(strCond, ",]")
    tData = eval(parse(text=strCond))
    if (nrow(tData) > 0) {
      x = tData[,colTime]
      y = tData[,colConc]
      tabRes = sNCA(x, y, dose=dose[i], adm=adm, dur=dur, doseUnit=doseUnit, timeUnit=timeUnit, concUnit=concUnit, down=down, R2ADJ=R2ADJ, MW=MW, SS=SS, iAUC=iAUC, Keystring=strHeader, excludeDelta=excludeDelta)
      UsedPoints = attr(tabRes, "UsedPoints")
      txtRes = Res2Txt(tabRes, x, y, dose=dose[i], adm=adm, dur=dur, doseUnit=doseUnit, down=down)
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
        y1 = log10(2.718281828)*(tabRes["b0"] - tabRes["LAMZ"] * (x1 - deltaX))
        y2 = log10(2.718281828)*(tabRes["b0"] - tabRes["LAMZ"] * (x2 + deltaX))
        lines(c(x1 - deltaX, x2 + deltaX), c(y1, y2), lty=2, col="red")
      }
      close.screen(all.screens=TRUE)
    }
  }
  par(defPar)
  ClosePDF()
}
