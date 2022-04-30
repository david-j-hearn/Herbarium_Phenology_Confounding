source("../SharedCode/NormalizeData.txt.R")
source("../SharedCode/removeOutliers.txt.R")
source("../SharedCode/helperFunctions.txt.R")
source("../SharedCode/simulateData.txt.R")
source("../SharedCode/createGraphs.txt.R")
source("../SharedCode/createFigurePanels.txt.R")
source("../SharedCode/specimenMetadata.txt.R")

createPanels.simulatedCorrelatedData = function(file) {

print(paste("current working directory", getwd()))
print(paste("input file directory", dirname(file)))
dir.create(paste(dirname(file), "/correlatedAnalysis",sep=""), showWarnings = FALSE, recursive = TRUE)
print(paste("Reading data", file))
data = read.table(file, header=T, sep='\t')
setwd(paste(dirname(file), "/correlatedAnalysis",sep=""))
sampledData = simulateCorrelatedBiasedData(data)
sampledData$dayOfYear = sampledData$dayOfYear.simulated
      makePanels_withData(sampledData,"panel.simulatedCorrelatedBiasedData")
      
}

#filelist is from specimenMetadata.txt.R file
for(file in filelist) {

print(paste("Simulated correlated analysis of ", file, sep=""))

wd = getwd()
createPanels.simulatedCorrelatedData(file)
setwd(wd)

}


