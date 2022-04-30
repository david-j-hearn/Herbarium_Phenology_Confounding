wd = getwd()

print("sourcing simulatedCorrelatedBiasedData")
source("makeFigurePanelsForAllSpecies.simulatedCorrelatedBiasedData.txt.R")

setwd(wd)
print("sourcing simulatedUncorrelatedBiasedData")
source("makeFigurePanelsForAllSpecies.simulatedUncorrelatedBiasedData.txt.R")

setwd(wd)
print("sourcing figure panels scripts")
source("makeFigurePanelsForAllSpecies.txt.R")


