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
# detrendDOY_by_Latitude(data):
#	takes a dataframe as input that has the following fields (minimally): 
#		latitude, date (as a decimal format), dayOfYear
#	returns detrended dayOfYear (field named dayOfYear_detrended) by subtracting a latitudinal effect
#
#
# detrendEarilestDOY_by_SampleSize(data):
#	Expects day of year to be detrended for latitude. If not, set option 'useDetrended=FALSE'
#	takes a dataframe as input that has the following fields (minimally): 
#		date (as a decimal format), dayOfYear, dayOfYear_detrended (if using detrended day of year from detrendDOY_by_Latitude function)
#	returns a new dataframe with fields: year and earliest_dayOfYear_detrended (detrended earliest_dayOfYear resulting from subtracting a sample size effect)
#	optional arguments include (Expert use only; defaults are typically fine):
#		type: a string representing the type of approximation for the expected earliest day of year (default: "AA")
#			AA (normal asymtotic approximation; fast, accurate, most statistically conservative)
#			GMC (Monte Carlo approximation using gaussian)
#			DMC (Monte Carlo approximation using data resampling)
#			DNA (discretized normal approximation using approximation of integral)
#			LSD (low accuracy, not recommended)
#			WD (Weibull distribution approximation from Pearse et al. 2017; low accuracy, not recommended)
#		MCReps: a number of Monte Carlo replicates used for types GMC and DMC (default: 500)
#		minSS: a number representing the minimum sample size required for a year (default: 3)
#		dx: a size of a discretization interval for type DNA (default: 0.05)
#		useDetrended: Boolean; if TRUE, uses latitude-detrended day of year values. Must run detrendDOY_by_Latitude first - no warning! (default: TRUE)
#	
#_________________________________________________________


source("helperFunctions.txt.R")


detrendDOY_by_Latitude = function(data)
	{
	doy = data$dayOfYear
	lat = data$latitude
	date = data$date

	Lat_DOY = lm(doy ~ lat)

	data$dayOfYear_detrended = doy - (Lat_DOY$coefficients[[2]] * lat + Lat_DOY$coefficients[[1]])

	return(data)
	}

detrendEarilestDOY_by_SampleSize = function(data, type="AA", MCReps=500, minSS=3, dx=0.05, useDetrended=TRUE)
	{
	doy = data$dayOfYear 
	if(useDetrended) { doy = data$dayOfYear_detrended }
	date = data$date

	dateFloor = floor(date)
	yearList = c(min(dateFloor):max(dateFloor))
	minDOYsT3 = sapply(yearList,returnTransformedMinDOY, years=dateFloor, DOYs=doy, mu=mean(doy), sd=sd(doy), minSS=minSS, MCReps=MCReps, dx=dx, type=type)
	tb = !is.na(minDOYsT3)
	minDOYsT3 = minDOYsT3[tb]
	yearList1 = yearList[tb]
	minT3_vs_date = lm(minDOYsT3 ~  yearList1)

	return( data.frame(year = yearList1, earliest_dayOfYear_detrended = minDOYsT3) )
	}
