tabNCA = function(concData, colSubj="Subject", colTime="Time", colConc="conc", dose=0, adm="Extravascular", dur=0, doseUnit="mg", timeUnit="h", concUnit="ug/L", down="Linear", MW=0, returnNA=FALSE) 
{
  IDs = sort(as.character(unique(concData[,colSubj])))
  IDs = sort(as.numeric(unique(concData[,colSubj])))
  nID = length(IDs)

  Res = vector()
  for (i in 1:nID) {
    tRes = sNCA(concData[concData[,colSubj]==IDs[i],colTime], concData[concData[,colSubj]==IDs[i],colConc], dose=dose, adm=adm, dur=dur, doseUnit=doseUnit, timeUnit=timeUnit, concUnit=concUnit, down=down, MW=MW, returnNA=returnNA)
    Res = rbind(Res, c(ID=IDs[i], tRes))
  }
  rownames(Res) = NULL
  attr(Res, "units") = c("", attr(tRes, "units"))
  return(Res)
}


