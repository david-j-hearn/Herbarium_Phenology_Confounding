source("../SharedCode/NormalizeData.txt.R")
source("../SharedCode/removeOutliers.txt.R")
source("../SharedCode/helperFunctions.txt.R")
source("../SharedCode/simulateData.txt.R")
source("../SharedCode/runAnalyses_NoPlots.txt.R")
source("../SharedCode/specimenMetadata.txt.R")

nReps = 1000
type = "AA"
strength = -0.1

runCorrelatedAnalysis = function(file, rep=1, type="AA") {

wd = getwd()

data = read.table(file, header=T, sep='\t')

dir.create(paste(dirname(file), "/correlatedAnalysis",sep=""), showWarnings = FALSE)
setwd(paste(dirname(file), "/correlatedAnalysis",sep=""))

sampledData = simulateCorrelatedBiasedData(data,strength=strength)
sampledData$dayOfYear = sampledData$dayOfYear.simulated
append = TRUE
if(rep == 1 ) { append = FALSE }
sink("correlatedAnalysis.results.txt", append=append)
runAnalysis_wData(sampledData,type)
sink()

setwd(wd)

}

#filelist from specimenMetadata.txt.R


for(file in filelist) {
	print(paste("Correlated: Analyzing ", file))
	for(i in 1:nReps) {

		print(paste("correlated rep", i))
		runCorrelatedAnalysis(file=file,rep=i,type=type)
	}

}


