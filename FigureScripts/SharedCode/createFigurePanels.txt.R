#_________________________________________________________
#
# Author: David J. Hearn
# Citation: 
# Use Policy: Free to use and modify provided citation is included in any resulting works
#
# Description: Functions to create figure panels for the above cited paper
#
#FUNCTIONS:
#
#	All panel functions require as input:
#		data frame with fields named dayOfYear, latitude, date
#		outFileBase: the base name of the output file. 
#			‘panel[1,2,3].pdf’ is appended to the base name
#
# panel1(data,outFileBase)
#	creates the following graphs:
#		createQQPlot(data)
#		createXMinDists(data, c(1,10,75,500,7500))
#		createExpectedMinimumGraph(data)
#
# panel2(data,outFileBase)
#	creates the following graphs:
#		createDateVLat(data)
#		createLatVDOY(data)
#		createLatVDOYMLat(data)
#		createDateVDOY(data)
#		createDateVDOYMLat(data)
#
# panel3(data,outFileBase)
#	creates the following graphs:
#		createYearVSS(data)
#		createYearVMinDOY(data)
#		createYearVMinDOYMLat(data)
#		createYearVMinDOYMLatMSS(data)
#
# panelA(data, outFileBase)
#		createLatVDOY(data)
#		createDateVLat(data)
#		createYearVSS(data)
#		createDateVDOY(data)
#		createYearVMinDOY(data)
#		createDateVDOYMLat(data)
#		createYearVMinDOYMLatMSS(data)
#
# figureB(files,outFileBase)
#	for each of the input files, creates the following graphs all in one figure:
#		createXMinDists(data, c(1,10,75,500,7500))
#		createExpectedMinimumGraph(data)	
#
# makePanels(inFile)
#	creates the above three panels
#	requires the name of an input file
#	the text, tab-separated input file has the following columns:
#		latitude
#		date
#		dayOfYear
#
#_________________________________________________________


source("../SharedCode/simulateData.txt.R")

figureB.uncorrelated = function(files, outFileBase=character(0)) {

	wd = getwd()

if(length(outFileBase)<=0) {
outFileBase = "default"
}

pdf(paste(outFileBase, ".ExpectedMin.uncorrelated.pdf", sep=""))

par(mar=c(4,4,1,1))

nPanels = length(files)

layout(matrix(1:(2*nPanels),nPanels,2, byrow=T))


for(file in files)
	{
	print(paste("figureB.uncorrelated",file))

	dir.create(paste(dirname(file), "/uncorrelatedAnalysis",sep=""), 	showWarnings = TRUE)
	print(dirname(file))

	data = read.table(file, header=T, sep='\t')

	setwd(paste(dirname(file), "/uncorrelatedAnalysis",sep=""))
#The simulation function removes outliers and normalized data, when necessary
	sampledData = simulateUncorrelatedBiasedData(data)
	sampledData$dayOfYear = sampledData$dayOfYear.simulated
	
	createXMinDists(sampledData, c(1,10,75,500,7500))
	createExpectedMinimumGraph(sampledData)

	setwd(wd)
	}
dev.off()


}

figureB.correlated = function(files, outFileBase=character(0)) {

wd = getwd()


if(length(outFileBase)<=0) {
outFileBase = "default"
}

pdf(paste(outFileBase, ".ExpectedMin.correlated.pdf", sep=""))

par(mar=c(4,4,1,1))

nPanels = length(files)

layout(matrix(1:(2*nPanels),nPanels,2, byrow=T))


for(file in files)
	{
	print(paste("figureB.correlated",file))

	print(dirname(file))
	dir.create(paste(dirname(file), "/correlatedAnalysis",sep=""), 	showWarnings = TRUE)
	data = read.table(file, header=T, sep='\t')

	setwd(paste(dirname(file), "/correlatedAnalysis",sep=""))

#The simulation function removes outliers and normalized data, when necessary
	sampledData = simulateCorrelatedBiasedData(data)
	sampledData$dayOfYear = sampledData$dayOfYear.simulated
	
	createXMinDists(sampledData, c(1,10,75,500,7500))
	createExpectedMinimumGraph(sampledData)

	setwd(wd)

	}
dev.off()


}


figureB = function(files, outFileBase=character(0)) {

wd = getwd()

if(length(outFileBase)<=0) {
outFileBase = "default"
}

pdf(paste(outFileBase, ".ExpectedMin.empirical.pdf", sep=""))

par(mar=c(4,4,1,1))

nPanels = length(files)

layout(matrix(1:(2*nPanels),nPanels,2, byrow=T))


for(inFile in files) {
	print(paste("figureB ",inFile))


print(dirname(inFile))
data = read.table(inFile, header=T, sep='\t')

setwd(dirname(inFile))

file = basename(inFile)

data = removeOutliers(data)
if(shapiro.test(data$dayOfYear)$p.value<0.05) { 
	data = normalizeData(data)
}

createXMinDists(data, c(1,10,75,500,7500))
createExpectedMinimumGraph(data)

setwd(wd)
}

dev.off()
}




panelA = function(data, outFileBase=character(0)) {

if(length(outFileBase)<=0) {
outFileBase = "default"
}

pdf(paste(outFileBase, ".panelA.pdf", sep=""))

par(mar=c(4,4,1,1))

layout(matrix(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,7,8,8,8),3,6, byrow=T))

		#Holder for the plant image
		plot(1, type="n", xlab="", ylab="", axes=FALSE, frame.plot=F, xlim=c(0, 10), ylim=c(0, 10))

		createDateVLat(data)
		createYearVSS(data)		
		createLatVDOY(data)
		createDateVDOY(data)
		createYearVMinDOY(data)
		createDateVDOYMLat(data)
		createYearVMinDOYMLatMSS(data)

dev.off()
}



panel1 = function(data, outFileBase=character(0)) {

if(length(outFileBase)<=0) {
outFileBase = "default"
}

pdf(paste(outFileBase, ".panel1.pdf", sep=""))

layout(matrix(c(1,2,2,3,3,3),2,3, byrow=T))

createQQPlot(data)
createXMinDists(data, c(1,10,75,500,7500))
createExpectedMinimumGraph(data)

dev.off()
}

panel2 = function(data, outFileBase) {

if(length(outFileBase)<=0) {
outFileBase = "default"
}

pdf(paste(outFileBase, ".panel2.pdf", sep=""))

layout(matrix(c(1,1,2,3,4,5),3,2, byrow=T))

createDateVLat(data)
createLatVDOY(data)
createLatVDOYMLat(data)
createDateVDOY(data)
createDateVDOYMLat(data)

dev.off()

}

panel3 = function(data,outFileBase) {

if(length(outFileBase)<=0) {
outFileBase = "default"
}

pdf(paste(outFileBase, ".panel3.pdf", sep=""))

layout(matrix(c(1,1,2,3,4,4),3,2,byrow=T))

createYearVSS(data)
createYearVMinDOY(data)
createYearVMinDOYMLat(data)
createYearVMinDOYMLatMSS(data)

dev.off()

}

makePanels = function(inFile) {

wd = getwd()
print(dirname(inFile))

data = read.table(inFile, header=T, sep='\t')

setwd(dirname(inFile))

file = basename(inFile)

data = removeOutliers(data)
if(shapiro.test(data$dayOfYear)$p.value<0.05) { 
	data = normalizeData(data)
}

panel1(data,file)
panel2(data,file)
panel3(data,file)
panelA(data,file)

setwd(wd)
}

makePanels_withData = function(data,outFile)
	{
	wd = getwd()
      panel1(data, outFile)
      panel2(data, outFile)
      panel3(data, outFile)
	panelA(data,outFile)
	setwd(wd)
	}

