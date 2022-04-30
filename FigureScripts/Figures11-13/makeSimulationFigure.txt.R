source("../SharedCode/NormalizeData.txt.R")
source("../SharedCode/removeOutliers.txt.R")
source("../SharedCode/helperFunctions.txt.R")
source("../SharedCode/simulateData.txt.R")
source("../SharedCode/createGraphs.txt.R")
source("../SharedCode/createFigurePanels.txt.R")
source("../SharedCode/specimenMetadata.txt.R")


#file list can be changed to make figures for other species
files = c(
"../SpecimenFiles/AnemonoidesQuinquefolia/AnemoneQuinquifolia_All_Geo/occurrence.Parsed.csv",

"../SpecimenFiles/MertensiaVirginica/MertensiaVirginica_All_Geo/occurrence.Parsed.csv",

"../SpecimenFiles/PodophyllumPeltatum/PodophyllumPeltatum_All_Geo/occurrence.Parsed.csv"
)

wd <- getwd()


print("Running figure creation")
if (!is.null(wd)) setwd(wd)
figureB(files, "simulationResults.Empirical") 

print("Running correlated")
if (!is.null(wd)) setwd(wd)
figureB.correlated(files, "simulationResults.correlated") 

print("Running uncorrelated")
if (!is.null(wd)) setwd(wd)
figureB.uncorrelated(files, "simulationResults.uncorrelated") 
