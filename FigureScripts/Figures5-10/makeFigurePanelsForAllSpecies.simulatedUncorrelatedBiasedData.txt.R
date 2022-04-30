source("../SharedCode/NormalizeData.txt.R")
source("../SharedCode/removeOutliers.txt.R")
source("../SharedCode/helperFunctions.txt.R")
source("../SharedCode/simulateData.txt.R")
source("../SharedCode/createGraphs.txt.R")
source("../SharedCode/createFigurePanels.txt.R")
source("../SharedCode/specimenMetadata.txt.R")


createPanels.simulatedUncorrelatedData = function(file) {

print(getwd())
print(dirname(file))
dir.create(paste(dirname(file), "/uncorrelatedAnalysis",sep=""), showWarnings = FALSE)
data = read.table(file, header=T, sep='\t')
setwd(paste(dirname(file), "/uncorrelatedAnalysis",sep=""))
sampledData = simulateUncorrelatedBiasedData(data)
sampledData$dayOfYear = sampledData$dayOfYear.simulated
      makePanels_withData(sampledData,"panel.simulatedUncorrelatedBiasedData")
}

#filelist from specimenMetadata.txt.R file
for(file in filelist)
{

print(paste("Simulated uncorrelated analysis of ", file, sep=""))

wd = getwd()
createPanels.simulatedUncorrelatedData(file)
setwd(wd)

}

