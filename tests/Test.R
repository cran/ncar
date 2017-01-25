require(ncar)
data(RptCfg)
Equal = function(Wres, Rres)
{
  Wres[,"Subject"] = as.character(Wres[,"Subject"])
  ColName0 = colnames(Rres)
  rownames(RptCfg) = RptCfg[,"PPTESTCD"]
  colnames(Rres) = c(ColName0[1], RptCfg[ColName0[-1],"WNL"])
  Inter = intersect(colnames(Wres), ColName0)
  return(all.equal(Wres[,Inter], Rres[,Inter]))
}

Wres = read.csv("Final_Parameters_Pivoted_Theoph_Linear.csv")
Rres = NCA(Theoph, "Subject", "Time", "conc", Dose=320)
if (!Equal(Wres, Rres)) stop("Test Failed!")

Wres = read.csv("Final_Parameters_Pivoted_Theoph_Log.csv")
Rres = NCA(Theoph, "Subject", "Time", "conc", Dose=320, Method="Log")
if (!Equal(Wres, Rres)) stop("Test Failed!")

Wres = read.csv("Final_Parameters_Pivoted_Indometh_Linear.csv")
Rres = NCA(Indometh, "Subject", "time", "conc", Dose=25, AdmMode="Bolus")
if (!Equal(Wres, Rres)) stop("Test Failed!")

Wres = read.csv("Final_Parameters_Pivoted_Indometh_Log.csv")
Rres = NCA(Indometh, "Subject", "time", "conc", Dose=25, AdmMode="Bolus", Method="Log")
if (!Equal(Wres, Rres)) stop("Test Failed!")

Wres = read.csv("Final_Parameters_Pivoted_Indometh_Linear_Infusion.csv")
Rres = NCA(Indometh, "Subject", "time", "conc", Dose=25, AdmMode="Infusion", TimeInfusion=0.25)
if (!Equal(Wres, Rres)) stop("Test Failed!")

Wres = read.csv("Final_Parameters_Pivoted_Indometh_Log_Infusion.csv")
Rres = NCA(Indometh, "Subject", "time", "conc", Dose=25, AdmMode="Infusion", TimeInfusion=0.25, Method="Log")
if (!Equal(Wres, Rres)) stop("Test Failed!")

Wres = read.csv("Final_Parameters_Pivoted_Indometh_Linear_Wrong_Extravascular.csv")
Rres = NCA(Indometh, "Subject", "time", "conc", Dose=25)
if (!Equal(Wres, Rres)) stop("Test Failed!")

Wres = read.csv("Final_Parameters_Pivoted_Indometh_Log_Wrong_Extravascular.csv")
Rres = NCA(Indometh, "Subject", "time", "conc", Dose=25, Method="Log")
if (!Equal(Wres, Rres)) stop("Test Failed!")
