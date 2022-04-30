#_________________________________________________________
#
# Author: David J. Hearn
# Citation: 
# Use Policy: Free to use and modify provided citation is included in any resulting works
#
# Description: 
#	Function performs an optimal Box-Cox transformation of day of year data
#	Relies on geoR library:
# Paulo J. Ribeiro Jr, Peter J. Diggle, Martin Schlather, Roger Bivand and Brian Ripley (2020).
# geoR: Analysis of Geostatistical Data. R package version 1.8-1.
# https://CRAN.R-project.org/package=geoR
#
#FUNCTION:
#
# normalizeData(data, returnType=1)
# 	Takes a data frame as input
#	Returns the normalized data using an optimal Box-Cox transformation
#	Data frame needs a field called dayOfYear with all positive values
#	Returns the data frame with the dayOfYear field transformed in place
#	Return type is:
#		1 -> just the data frame (default)
#		non-1 value -> a list with the data frame and the optimal Box-Cox lambda
#
#
#_________________________________________________________


if(!require(geoR)) {
install.packages(geoR)
}
library(geoR)


normalizeData = function(data, returnType=1)
	{

      bcfit = boxcoxfit(data$dayOfYear)
      data$dayOfYear = ((data$dayOfYear^bcfit$lambda) - 1) / bcfit$lambda
      
      if(returnType == 1) {
	return(data)
	}
	
	return(list(data, bcfit$lambda))
	
	}
