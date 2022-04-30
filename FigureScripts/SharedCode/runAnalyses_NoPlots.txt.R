#_________________________________________________________
#
# Author: David J. Hearn
# Citation: 
# Use Policy: Free to use and modify provided citation is included in any resulting works
#
# Description: Functions to carry out a slew of analyses associated with specimen data
#
#FUNCTIONS:
#
# runAnalysis_wData(data):
#	takes a dataframe as input that has the following fields: 
#		ID, species, latitude, longitude, date (as a decimal format), dayOfYear
#	provides a series of analyses of the input data
#
#
# runAnalysis(file):
#	takes a filename as input.
#	The input text file contains a tab-separated table that has the following fields: 
#		ID, species, latitude, longitude, date (as a decimal format), dayOfYear
#	provides a series of analyses of the input data
#
#
#_________________________________________________________


#Given a dataframe, runs through a series of analyses
runAnalysis_wData = function(data, normalizeData=FALSE, removeOutliers=TRUE,type="DMC")
{

#_________________________________ REMOVE OUTLIERS________________________
if(removeOutliers) { data = removeOutliers(data) }

#___________________NORMALIZE via BOX-COX_______________________
if(shapiro.test(data$dayOfYear)$p.value < 0.05 || normalizeData==TRUE) { data = normalizeData(data) }

#___________________REASSIGN VARIABLES FOR CONVENIENCE________________
doy = data$dayOfYear
lat = data$latitude
date = data$date

#___________________ANALYSES______________________

	#Shapiro Wilks

print(paste("Shapiro.Wilks", format(shapiro.test(doy)$p.value
,format="e", digits=2)))


	#Date vs. Latitude

Date_Lat = lm( lat ~ date )

print(paste("DateVLat", format(summary(Date_Lat)$coefficients[,4][[2]],format="e", digits=2)))

	#Sample size vs. Year 

hData = hist(data$date, breaks=seq(min(floor(data$date)), max(ceiling(data$date)), by=1), plot=F)
counts = hData$counts
years = hData$breaks[-length(hData$breaks)]
ts = (counts != 0)
years = years[ts]
counts = counts[ts]
cModel = lm(counts ~ years)

print(paste("YearVSS", format(summary(cModel)$coefficients[,4][[2]],format="e", digits=2)))

	#Latitude vs DOY

Lat_DOY = lm(doy ~ lat)

print(paste("LatVDOY", format(summary(Lat_DOY)$coefficients[,4][[2]],format="e", digits=2)))

	#Latitude vs. DOY (transformed for latitude)

Lat_DOY = lm(doy ~ lat)
doy = doy - (Lat_DOY$coefficients[[2]] * lat + Lat_DOY$coefficients[[1]])
mu = mean(doy)
stdev = sd(doy)
Lat_DOY_Trans = lm(doy ~ lat)

print(paste("LatVDOYMLat", format(summary(Lat_DOY_Trans)$coefficients[,4][[2]],format="e", digits=2)))

	#Date vs. DOY

DOY_date = lm(data$dayOfYear ~ data$date)

print(paste("DateVDOY", format(summary(DOY_date)$coefficients[,4][[2]],format="e", digits=2)))

	#DOY ~ Date + latitude

DOY_DateLat = lm(data$dayOfYear ~ data$date + data$latitude)

print(paste("DatePLatVDOY", format(summary(DOY_DateLat)$coefficients[,4][[2]],format="e", digits=2), format(summary(DOY_DateLat)$coefficients[,4][[3]],format="e", digits=2)))

	#Date vs. DOY (transformed for latitude)

DOYtrans_date = lm(doy~ date)

print(paste("DateVDOYMLat", format(summary(DOYtrans_date)$coefficients[,4][[2]],format="e", digits=2)))

	#Year vs. min DOY

dateFloor = floor(date)
yearList = c(min(dateFloor):max(dateFloor))
minDOYsRaw = sapply(yearList,returnMinDOY, years=dateFloor, DOYs=data$dayOfYear)
tb = !is.na(minDOYsRaw)
minDOYsRaw= minDOYsRaw[tb]
yearList1 = yearList[tb]
minRaw_vs_date = lm(minDOYsRaw ~  yearList1)

print(paste("DateVMinDOY", format(summary(minRaw_vs_date)$coefficients[,4][[2]],format="e", digits=2)))

	#Year vs. min DOY (transformed for latitude)

minDOYsT1 = sapply(yearList,returnMinDOY, years=dateFloor, DOYs=doy)
tb = !is.na(minDOYsT1)
minDOYsT1= minDOYsT1[tb]
yearList1 = yearList[tb]
minT1_vs_date = lm(minDOYsT1 ~  yearList1)

print(paste("DateVMinDOYMLat", format(summary(minT1_vs_date)$coefficients[,4][[2]],format="e", digits=2)))

	#Year vs. min DOY (transformed for sample size)

minDOYsT3 = sapply(yearList,returnTransformedMinDOY, years=dateFloor, DOYs=data$dayOfYear, mu=mean(data$dayOfYear), sd=sd(data$dayOfYear), minSS=3, MCReps=500, dx=0.05, type=type)
tb = !is.na(minDOYsT3)
minDOYsT3 = minDOYsT3[tb]
yearList1 = yearList[tb]
minT3_vs_date = lm(minDOYsT3 ~  yearList1)

print(paste("DateVMinDOYMSS", format(summary(minT3_vs_date)$coefficients[,4][[2]],format="e", digits=2)))

	#Year vs. min DOY (transformed for latitude, transformed for sample size)
#paste("Here")
minDOYsT2 = sapply(yearList,returnTransformedMinDOY, years=dateFloor, DOYs=doy, mu=mu, sd=stdev, minSS=3, MCReps=500, dx=0.05, type=type)

#print(minDOYsT2)
tb = !is.na(minDOYsT2)
minDOYsT2 = minDOYsT2[tb]
yearList1 = yearList[tb]
minT2_vs_date = lm(minDOYsT2 ~  yearList1)

print(paste("DateVMinDOYMLatMSS", format(summary(minT2_vs_date)$coefficients[,4][[2]],format="e", digits=2)))

}

#Given a text file with a data table, runs through a series of analyses
#The data table needs the following fields and column headers: ID, species, latitude, longitude, date, dayOfYear

runAnalysis = function(file,type="DMC") {

wd = getwd()

data = read.table(file=file, sep='\t', header=T)

setwd(dirname(file))
sink(paste(basename(file), ".ROutput.txt", sep=""))
runAnalysis_wData(data,type)
sink()

setwd(wd)

}

