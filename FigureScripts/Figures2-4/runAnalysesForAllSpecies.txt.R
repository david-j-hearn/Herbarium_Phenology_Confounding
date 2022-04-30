source("../SharedCode/NormalizeData.txt.R")
source("../SharedCode/removeOutliers.txt.R")
source("../SharedCode/helperFunctions.txt.R")
source("../SharedCode/runAnalyses_NoPlots.txt.R")
source("../SharedCode/specimenMetadata.txt.R")

#filelist from specimenMetadata.txt.R

type = "AA"

for(file in filelist) {

	print(paste("Analyzing ", file))
	runAnalysis(file,type=type)

}




