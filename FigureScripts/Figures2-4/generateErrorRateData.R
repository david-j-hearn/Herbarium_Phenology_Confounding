wd = getwd()

source("runAnalysesForAllSpecies.simulatedUncorrelatedBiasedData.txt.R")

setwd(wd)
source("runAnalysesForAllSpecies.simulatedCorrelatedBiasedData.txt.R")

setwd(wd)
source("runAnalysesForAllSpecies.txt.R")


