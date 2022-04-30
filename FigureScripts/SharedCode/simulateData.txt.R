#_________________________________________________________
#
# Author: David J. Hearn
# Citation: 
# Use Policy: Free to use and modify provided citation is included in any resulting works
#
# Description: 
#	Function to generate a simulated data set. 
#	For every point provided in the input data set, a day of year is simulated based on the 
#		empirical mean and standard deviation of the (transformed) day of year 
#
#FUNCTION:
#
# simulateUncorrelatedBiasedData(data):
#	takes a dataframe as input that has the following fields: 
#		ID, species, latitude, longitude, date (as a decimal format), dayOfYear
#	provides the original data frame as output 
#		with the added field of the simulated day of year 
#	Data are simulated so that date and day of year are independent
#
# simulateCorrelatedBiasedData(data):
#	takes a dataframe as input that has the following fields: 
#		ID, species, latitude, longitude, date (as a decimal format), dayOfYear
#	provides the original data frame as output 
#		with the added field of the simulated day of year
#	Data are simulated so there is a linear correlation between date
#		and day of year
#
#_________________________________________________________

simulateUncorrelatedBiasedData = function(data, normalizeData = FALSE, removeOutliers=TRUE) {

#remove outliers
if(removeOutliers) { data = removeOutliers(data) }

#normalize data
if(shapiro.test(data$dayOfYear)$p.value < 0.05 || normalizeData) { data = normalizeData(data) }


#make model of latitude vs DOY to get beta and alpha
LatVDOY = lm(data$dayOfYear ~ data$latitude)
alpha = summary(LatVDOY)$coefficients[[1]] 
beta = summary(LatVDOY)$coefficients[[2]]

#remove correlation with latitude
dayOfYear.adjusted = data$dayOfYear - (beta * data$latitude + alpha)

#Sample DOY from single distribution (Null: uncorrelated with date) ,
#	and add back in the effect of latitude on DOY
dayOfYear.simulated = rnorm(length(data$latitude), mean(dayOfYear.adjusted), sd(dayOfYear.adjusted)) + beta * data$latitude + alpha

#Box-Cox requires positive data, so shift values up if there are negative values in case further analysis of simulated data applies Box-Cox; this won’t influence correlation statistics
if(min(dayOfYear.simulated)<0) {
dayOfYear.simulated = dayOfYear.simulated - min(dayOfYear.simulated) + 0.0001
}


data$dayOfYear.simulated = dayOfYear.simulated


#return the modified data frame
return(data)
}

#Default strength comes from estimate from Mertensia virginica
simulateCorrelatedBiasedData = function(data, strength=-0.005, removeOutliers = TRUE, normalizeData = FALSE) {

#remove outliers
if(removeOutliers) { data = removeOutliers(data) }

#normalize data
if(shapiro.test(data$dayOfYear)$p.value < 0.05 || normalizeData) { data = normalizeData(data) }

#make model of latitude vs DOY to get beta and alpha
LatVDOY = lm(data$dayOfYear ~ data$latitude)
alpha = summary(LatVDOY)$coefficients[[1]] 
beta = summary(LatVDOY)$coefficients[[2]]

#remove correlation with latitude
dayOfYear.adjusted = data$dayOfYear - (beta * data$latitude + alpha)

#Sample DOY from single distribution (Null: uncorrelated with date) ,
#	and add back in the effect of latitude on DOY
dayOfYear.simulated = rnorm(length(data$latitude), mean(dayOfYear.adjusted), sd(dayOfYear.adjusted)) + beta * data$latitude + alpha

#put in correlation with date based on provided value of slope "strength"
dayOfYear.simulated = dayOfYear.simulated + strength * sd(dayOfYear.adjusted) * data$date

#Box-Cox requires positive data, so shift values up if there are negative values in case further analysis of simulated data applies Box-Cox; this won’t influence correlation statistics 
if(min(dayOfYear.simulated)<0) {
dayOfYear.simulated = dayOfYear.simulated - min(dayOfYear.simulated) + 0.0001
}

#prepare data frame for return
data$dayOfYear.simulated = dayOfYear.simulated

#return the modified data frame
return(data)
}

