wd = getwd()
setwd("..")
source("biasCorrections.txt.R")
setwd(wd)
source("NormalizeData.txt.R")
source("removeOutliers.txt.R")

par(mfrow = c(3, 2))

#read in the occurrence data
	data = read.table("occurrence.Parsed.csv", header=T, sep='\t')
	plot(data$date, data$dayOfYear, main="Raw")

#remove outliers
	data = removeOutliers(data)
	plot(data$date, data$dayOfYear, main="No Outliers")

#normalize data
	data = normalizeData(data)
	plot(data$date, data$dayOfYear, main="Normalized")

#detrend for latitude
	data = detrendDOY_by_Latitude(data)
	plot(data$date, data$dayOfYear_detrended, main="Detrended for latitude")

#detrend for sample size effect
	data_earliestDOY = detrendEarilestDOY_by_SampleSize(data, type="AA", MCReps=500, minSS=3, dx=0.05, useDetrended=TRUE)
	plot(data_earliestDOY$year, data_earliestDOY$earliest_dayOfYear_detrended, main="Detrended for sampling effort")
	hist(data_earliestDOY$earliest_dayOfYear_detrended, main="Detrended for sampling effort")
