source("../SharedCode/NormalizeData.txt.R")
source("../SharedCode/removeOutliers.txt.R")
source("../SharedCode/helperFunctions.txt.R")
source("../SharedCode/simulateData.txt.R")
source("../SharedCode/runAnalyses_NoPlots.txt.R")
source("../SharedCode/specimenMetadata.txt.R")

nReps = 1000
type = "AA"

runUncorrelatedAnalysis = function(file, rep=1, type="AA") {

wd = getwd()

data = read.table(file, header=T, sep='\t')

dir.create(paste(dirname(file), "/uncorrelatedAnalysis",sep=""), showWarnings = FALSE)
setwd(paste(dirname(file), "/uncorrelatedAnalysis",sep=""))
sampledData = simulateUncorrelatedBiasedData(data)

sampledData$dayOfYear = sampledData$dayOfYear.simulated
append = TRUE
if(rep == 1 ) { append = FALSE }
sink("uncorrelatedAnalysis.results.txt", append=append)
runAnalysis_wData(sampledData,type=type)
sink()

setwd(wd)

}

#filelist from specimenMetadata.txt.R
for(file in filelist) {
	print(paste("Uncorrelated: Analyzing ", file))

	for(i in 1:nReps) {
		print(paste("rep", i))
		runUncorrelatedAnalysis(file=file,rep=i,type=type)
	}
}


