source("../SharedCode/NormalizeData.txt.R")
source("../SharedCode/removeOutliers.txt.R")
source("../SharedCode/helperFunctions.txt.R")
source("../SharedCode/simulateData.txt.R")
source("../SharedCode/createGraphs.txt.R")
source("../SharedCode/createFigurePanels.txt.R")
source("../SharedCode/specimenMetadata.txt.R")


print(paste("Current working directory: ", getwd()))


#filelist from specimenMetadata.txt.R
for(file in filelist)
{

print(paste("Analysis of empirical data from ", file, sep=""))

wd = getwd()
makePanels(file)
setwd(wd)

}

