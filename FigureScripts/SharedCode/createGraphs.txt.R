#_________________________________________________________
#
# Author: David J. Hearn
# Citation: 
# Use Policy: Free to use and modify provided citation is included in any resulting works
#
# Description: 
#	Functions to graph properties of specimen data
#	Every function requires the same input (and possibly additional input as well):
#		A data frame with (at least) the following fields (names):
#			latitude, date (formatted as a decimal), dayOfYear
#	Most function names have the following format:
#		'create' followed by
#		name of independent variable followed by
#		'V' for versus followed by
#		name of the dependent variable followed by (optionally, one or more times)
#			'M' for minus a bias followed by
#			name of the bias
#	Abbreviations are:
#		Lat = latitude (decimal)
#		Date = date (decimal)
#		Year = year (integer)
#		SS = sample size (integer)
#		DOY = day of year (decimal, as it can be a transformed DOY)
#		minDOY = minimum day of year from a sample of days of year (decimal)
#		V = versus
#		M = minus
#
#FUNCTIONS:
# 
# createXMinDists(data, SSs):
#	creates distributions of the minimum of a sample for a list of sample sizes
#	requires a vector of sample sizes along with the input data frame
# createQQPlot
#	wrapper for R QQ plotting functions
# createExpectedMinimumGraph(data, maxSS=75, MCreps=5000, normSS=10000)
#	creates a graph of the sample size vs. the expected minimum of a random sample
#	in addition to the data frame, requires the following inputs:
#		maxSS: maximum sample size considered (default = 75)
#		MCreps: number of replicates for the Monte Carlo procedure (default = 5000)
#		normSS: sample size to be drawn from the normal distribution 
#			to calculate minimum during the Monte Carlo approximation
#			(default = 10000)
#	Output is graph of points for different estimation techniques:
#		Theory approx. of expected minimum of a sample from a Gaussian population 
#		Monte Carlo approx. of expected minimum 
#			of a sample from a Gaussian population 
#			with +/- 2 SD of the expected minimum
#		Expected minimum based on resampled data (with replacement)
#			with +/- 2 SD of the expected minimum
#
# Following the function name format described above, the remaining functions all create:
#	a scatter plot of the variables 
#	a least squares best fit regression line laid onto the scatter plot
#
#		createDateVLat
#		createLatVDOY
# 		createLatVDOYMLat
# 		createDateVDOY
# 		createDateVDOYMLat
# 		createYearVSS
# 		createYearVMinDOY
# 		createYearVMinDOYMLat
# 		createYearVMinDOYMLatMSS
#
#_________________________________________________________


#____________GRAPH OF MINX DISTRIBUTIONS___________

createXMinDists = function(data, SSs) {
if(!require(viridis)) {
install.packages(viridis)
}

library("viridis")
cols = viridis(n=length(SSs),alpha=0.5)
mean = mean(data$dayOfYear)
sd = sd(data$dayOfYear)
cnt = 1
minX = (mean-expectedMinGaussianApproximation(mean,sd,SSs[length(SSs)])) + 3*sd
points = minNormal(-minX, 5*sd, mean, sd, 1)

pointsMax = minNormal(-minX, 5*sd, mean, sd, SSs[length(SSs)])
plot(points$x,points$y, xlim=range(pointsMax$x),ylim=range(pointsMax$y), type="l", xlab="Minimum of sampled day of year", ylab="Frequency")
#abline(v= expectedMinGaussianApproximation(mean,sd,1))
polygon(points$x, points$y, border="black", col=cols[1])
cnt=1
for(i in SSs) {
      points = minNormal(-minX, 5*sd, mean, sd, i)
       expX = expectedMinGaussianApproximation(mean,sd,i)
       polygon(points$x, points$y, border="black", col=cols[cnt])
       maxY = max(points$y)
       text(expX, maxY,paste("N=",i,sep=""), pos=4)
       cnt = cnt+1	
      }
hist(data$dayOfYear, add=T, probability=T,col=rgb(0.5,0.5,0.5,0.1))
}

#createXMinDists(5,1,c(1,10,100,1000,100000))

#____________GRAPH OF QQPLOT________________
createQQPlot = function(data)
	{
	#QQ Plot
qqnorm(data$dayOfYear)
qqline(data$dayOfYear)
}




#___________GRAPH OF SAMPLE SIZE VS EXPECTED MINIMUM_________

createExpectedMinimumGraph = function(data, maxSS=75, MCreps=5000, normSS=10000) {
	#Sample size vs. Expected Minimum

if(!require(viridis)) {
install.packages(viridis)
}

library("viridis")
cols.dots = viridis(n=4, alpha=0.1)
cols.lines = viridis(n=4)


#approximation if data were Gaussian
mu=mean(data$dayOfYear)
stdev=sd(data$dayOfYear)
approx = sapply(1:maxSS,expectedMinGaussianApproximation, mu=mu,stdev=stdev)

#approximation based on fitting a density
#lsDensity = logspline(x=data$dayOfYear)
#expectedMinimumLSD = getExpected_MinimumLogsplineDensity_Repeat(data,maxSS)

#print("calculating LSD CI")
#LSD_lCI = sapply(1:maxSS, findCILimit_minLogspline, tol=0.001,inc=1000,maxIters=100, alpha=0.05,mean=mu ,sd=stdev, lower=T,logsplineDensity= lsDensity)
#LSD_uCI = sapply(1:maxSS, findCILimit_minLogspline, tol=0.001,inc=1000,maxIters=100, alpha=0.05,mean=mu ,sd=stdev, lower=F,logsplineDensity= lsDensity)
##sdMinimumLSD = (getVariance_MinimumLogsplineDensity_Repeat(data,maxSS))^0.5

#MC estimate of expected min when density is Gaussian
expectedMinimumGMC = sapply(1:maxSS,sampleMinRepeatedG, mu=mu,stdev=stdev, returnType=1, nRep= MCreps)
#sdMinimumGMC = sapply(1:maxSS,sampleMinRepeatedG, mu=mu,stdev=stdev, returnType=2, nRep= MCreps)
GMC_lCI = sapply(1:maxSS,sampleMinRepeatedG, mu=mu,stdev=stdev, returnType=3, nRep= MCreps)
GMC_uCI = sapply(1:maxSS,sampleMinRepeatedG, mu=mu,stdev=stdev, returnType=4, nRep= MCreps)


#resampling estimate with sample from actual data, no model fit
expectedMinimumDMC = sapply(1:maxSS, sampleMinRepeatedD, nRep=MCreps, data=data$dayOfYear, returnType=1)
#sdMinimumDMC = sapply(1:maxSS, sampleMinRepeatedD, nRep=MCreps, data=data$dayOfYear, returnType=2)
DMC_lCI= sapply(1:maxSS, sampleMinRepeatedD, nRep=MCreps, data=data$dayOfYear, returnType=3)
DMC_uCI=sapply(1:maxSS, sampleMinRepeatedD, nRep=MCreps, data=data$dayOfYear, returnType=4)

#discrete numerical approximation, Gaussian assumption
dx = 0.01
meanDOY = mean(data$dayOfYear)
sdDOY = sd(data$dayOfYear)
x = seq(meanDOY-4*sdDOY,meanDOY+3*sdDOY,dx)
expectedMinimumDNA = sapply(1:maxSS, expectedValueMinX_discreteNormalApprox, x=x, mean=meanDOY, sd=sdDOY, dx=dx)

#print("calculating DNA CI")

DNA_lCI = sapply(1:maxSS, findCILimit_minGaussian, alpha=0.05, tol=0.001,mean=meanDOY ,sd=sdDOY, lower=T, maxIters=100, inc=1000)
DNA_uCI = sapply(1:maxSS, findCILimit_minGaussian, alpha=0.05, tol=0.001,mean=meanDOY ,sd=sdDOY, lower=F, maxIters=100, inc=1000)
#sdMinimumDNA = (sapply(1:maxSS, varianceMinX_discreteNormalApprox, x=x, mean=meanDOY, sd=sdDOY, dx=dx))^0.5

#plotting, set up y plot bounds

#ymax=max((expectedMinimumGMC+2*sdMinimumGMC),( 2*sdMinimumDNA + expectedMinimumDNA),( 2*sdMinimumLSD + expectedMinimumLSD), (2*sdMinimumDMC + expectedMinimumDMC))
#ymin=min((expectedMinimumGMC-2*sdMinimumGMC),( -2*sdMinimumDNA + expectedMinimumDNA),(-2*sdMinimumLSD + expectedMinimumLSD), (-2*sdMinimumDMC + expectedMinimumDMC))

#LSD_uCI=meanDOY
#LSD_lCI=meanDOY
ymax = max(approx, GMC_uCI, DMC_uCI, DNA_uCI)
ymin = min(approx, GMC_lCI, DMC_lCI, DNA_lCI)

#set up the plot
plot(NULL, xlab="Sample Size", ylab="Expected Minimum DOY", ylim=c(ymin,ymax), xlim=c(1,maxSS))
#plot(1:maxSS, approx, col="black", main=NA, xlab="Sample Size", ylab=paste("Expected Minimum DOY"), ylim=c(ymin,ymax), pch=1)

#Discrete numeric approximation
#polygon(c(1:maxSS,maxSS:1), c(2*sdMinimumDNA + expectedMinimumDNA,rev(expectedMinimumDNA-2*sdMinimumDNA)), col = NA, border=rgb(0,0,1,1) )
polygon(c(1:maxSS,maxSS:1), c(DNA_uCI,rev(DNA_lCI)), col = NA, border= cols.lines[1] )


#logspline density
##polygon(c(1:maxSS,maxSS:1), c(2*sdMinimumLSD + expectedMinimumLSD,rev(expectedMinimumLSD-2*sdMinimumLSD)), col = NA, border = rgb(1,0,1,1) )
#polygon(c(1:maxSS,maxSS:1), c(LSD_uCI,rev(LSD_lCI)), col = NA, border= rgb(1,0,1,1)  )


#Monte Carlo
#polygon(c(1:maxSS,maxSS:1), c(2*sdMinimumGMC + expectedMinimumGMC,rev(expectedMinimumGMC-2*sdMinimumGMC)), col = NA, border = rgb(0,1,0,1) )
polygon(c(1:maxSS,maxSS:1), c(GMC_uCI,rev(GMC_lCI)), col = NA, border= cols.lines[2]  )


#Resampled data
#polygon(c(1:maxSS,maxSS:1), c(2*sdMinimumDMC + expectedMinimumDMC,rev(expectedMinimumDMC-2*sdMinimumDMC)), col = rgb(1, 0, 0,0.05), border=rgb(1,0,0,1) )
polygon(c(1:maxSS,maxSS:1), c(DMC_uCI,rev(DMC_lCI)), col = cols.dots[3], border= cols.lines[3]  )


#Draw white lines to cover up the colored ends of the error polygon
segments(x0=1,y0=ymin,x1=1,y1=ymax,col="white",lwd=2)
segments(x0=maxSS,y0=ymin,x1=maxSS,y1=ymax,col="white",lwd=2)

#Assymptotic approximation
#lines(1:maxSS, approx, col=cols.dots[4], type="b", pch=1,cex=1)
lines(1:maxSS, approx, col=cols.lines[4], lwd=1, lty=1)


#Discrete numerical approximation
#lines(1:maxSS, expectedMinimumDNA,col=cols.dots[1],type="b", pch=5,cex=1)
lines(1:maxSS, expectedMinimumDNA,col=cols.lines[1],lwd=1, lty=3)

#Resampled data
#lines(1:maxSS, expectedMinimumDMC,col=cols.dots[3] ,type="b",pch=15, cex=0.5)
lines(1:maxSS, expectedMinimumDMC,col=cols.lines[3], lwd=1, lty=4)


#Monte Carlo
#lines(1:maxSS, expectedMinimumGMC,col=cols.dots[2] ,type="b",pch=18,cex=0.75)
lines(1:maxSS, expectedMinimumGMC,col=cols.lines[2] ,lwd=1, lty=6)


#logspline density
#lines(1:maxSS, expectedMinimumLSD,col=rgb(1,0,1,1) ,type="b",pch=12,cex=0.75)

#legend("topright",legend=c("Approx Gaussian", "MC Gaussian", "Data Resampled", "Discrete Numerical Approx"), col=c("black","blue","red","yellow"),lty=rep(1,4))

#legend("topright", legend=c("Asymp Gaussian", "Data Resampled", "Discrete Numerical", "MC Gaussian", "Logspline Density"), col=c("black","red","blue","green",rgb(1,0,1,1)), pch=c(1,15,5,18,12))

#legend("topright", legend=c("Asymp Gaussian", "Data Resampled", "Discrete Numerical", "MC Gaussian"), col=c(cols.lines[4],cols.lines[3],cols.lines[1],cols.lines[2]), pch=c(1,15,5,18))

legend("topright", legend=c("Asymp Gaussian", "Data Resampled", "Discrete Numerical", "MC Gaussian"), col=c(cols.lines[4],cols.lines[3],cols.lines[1],cols.lines[2]), lty=c(2,4,3,6))


}

#___________________GRAPH OF DATE VS LAT_________________

createDateVLat = function(data) {

lat = data$latitude
date = data$date

	#Date vs. Latitude

Date_Lat = lm( lat ~ date )

print(paste("DateVLat", format(summary(Date_Lat)$coefficients[,4][[2]],format="e", digits=2)))
#print(summary(Date_Lat))

plot(date,lat,main=NA, xlab=paste("Date\np-value =", format(summary(Date_Lat)$coefficients[,4][[2]],format="e", digits=2)), ylab="Latitude", pch=20, col="red")
abline(Date_Lat)

}

#___________________GRAPH OF LATITUDE VS DAY OF YEAR__________

createLatVDOY = function(data) {

lat = data$latitude
doy = data$dayOfYear

	#Latitude vs DOY
Lat_DOY = lm(doy ~ lat)

print(paste("LatVDOY", format(summary(Lat_DOY)$coefficients[,4][[2]],format="e", digits=2)))
#print(summary(Lat_DOY))

plot(lat,doy,main=NA, xlab= paste("Latitude\np-value =", format(summary(Lat_DOY)$coefficients[,4][[2]],format="e", digits=2)), ylab="Day of Year ", pch=20, col="red")
abline(Lat_DOY)

}

#___________________GRAPH OF LATITUDE VS DAY OF YEAR ADJUSTED____

createLatVDOYMLat = function(data)
{
doy = data$dayOfYear
lat = data$latitude

	#Latitude vs. DOY (transformed for latitude)
Lat_DOY = lm(doy ~ lat)
doy = doy - (Lat_DOY$coefficients[[2]] * lat + Lat_DOY$coefficients[[1]])
mu = mean(doy)
stdev = sd(doy)
Lat_DOY_Trans = lm(doy ~ lat)
#print(summary(Lat_DOY_Trans))
plot(lat,doy,main=NA, xlab= paste("Latitude\np-value =", format(summary(Lat_DOY_Trans)$coefficients[,4][[2]],format="e", digits=2)), ylab="Day of Year", pch=20, col="blue")
abline(Lat_DOY_Trans)

}

#__________________GRAPH OF DATE VS DAY OF YEAR__________


createDateVDOY = function(data) {

DOY_date = lm(data$dayOfYear ~ data$date)

print(paste("DateVDOY", format(summary(DOY_date)$coefficients[,4][[2]],format="e", digits=2)))
#print(summary(DOY_date))

plot(data$date,data$dayOfYear,main=NA, xlab= paste("Date\np-value =", format(summary(DOY_date)$coefficients[,4][[2]],format="e", digits=2)), ylab="Day of Year", pch=20, col="red")
abline(DOY_date)

}

#____________________GRAPH OF DATE VS DAY OF YEAR ADJUSTED_____


createDateVDOYMLat = function(data)
{

doy = data$dayOfYear
lat = data$latitude
date = data$date

Lat_DOY = lm(doy ~ lat)
doy = doy - (Lat_DOY$coefficients[[2]] * lat + Lat_DOY$coefficients[[1]])

DOYtrans_date = lm(doy~ date)

print(paste("DateVDOYMLat", format(summary(DOYtrans_date)$coefficients[,4][[2]],format="e", digits=2)))
#print(summary(DOYtrans_date))

plot(date,doy,main=NA, xlab= paste("Date\np-value =", format(summary(DOYtrans_date)$coefficients[,4][[2]],format="e", digits=2)), ylab="Day of Year", pch=20, col="blue")
abline(DOYtrans_date)

}

#______________________GRAPH OF YEAR VS SAMPLE SIZE____________


createYearVSS = function(data) {

hData = hist(data$date, breaks=seq(min(floor(data$date)), max(ceiling(data$date)), by=1), plot=F)
counts = hData$counts
years = hData$breaks[-length(hData$breaks)]
ts = (counts != 0)
years = years[ts]
counts = counts[ts]
cModel = lm(counts ~ years)

print(paste("YearVSS", format(summary(cModel)$coefficients[,4][[2]],format="e", digits=2)))
#print(summary(cModel))

plot(years,counts,main=NA, xlab= paste("Year\np-value =", format(summary(cModel)$coefficients[,4][[2]],format="e", digits=2)), ylab="Sample Size", pch=20, col="red")
abline(cModel)

}

#_______________________GRAPH OF YEAR VS MINIMUM DAY OF YEAR_______



createYearVMinDOY = function(data) {

date = data$date

dateFloor = floor(date)
yearList = c(min(dateFloor):max(dateFloor))
minDOYsRaw = sapply(yearList,returnMinDOY, years=dateFloor, DOYs=data$dayOfYear)
tb = !is.na(minDOYsRaw)
minDOYsRaw= minDOYsRaw[tb]
yearList1 = yearList[tb]
minRaw_vs_date = lm(minDOYsRaw ~  yearList1)

print(paste("DateVMinDOY", format(summary(minRaw_vs_date)$coefficients[,4][[2]],format="e", digits=2)))
#print(summary(minRaw_vs_date))

plot(yearList1,minDOYsRaw, main=NA, xlab= paste("Year\np-value =", format(summary(minRaw_vs_date)$coefficients[,4][[2]],format="e", digits=2)), ylab="Minimum Day of Year", pch=20, col="red")
abline(minRaw_vs_date)

}

#_____GRAPH OF YEAR VS MINIMUM DAY OF YEAR MINUS LATITUDE BIAS___


createYearVMinDOYMLat = function(data) {

doy = data$dayOfYear
date = data$date
lat = data$latitude

Lat_DOY = lm(doy ~ lat)
doy = doy - (Lat_DOY$coefficients[[2]] * lat + Lat_DOY$coefficients[[1]])

dateFloor = floor(date)
yearList = c(min(dateFloor):max(dateFloor))

minDOYsT1 = sapply(yearList,returnMinDOY, years=dateFloor, DOYs=doy)
tb = !is.na(minDOYsT1)
minDOYsT1= minDOYsT1[tb]
yearList1 = yearList[tb]
minT1_vs_date = lm(minDOYsT1 ~  yearList1)

print(paste("DateVMinDOYMLat", format(summary(minT1_vs_date)$coefficients[,4][[2]],format="e", digits=2)))
#print(summary(minT1_vs_date))

plot(yearList1,minDOYsT1, main=NA, xlab= paste("Year\np-value =", format(summary(minT1_vs_date)$coefficients[,4][[2]],format="e", digits=2)), ylab="Minimum Day of Year", pch=20, col="red")
abline(minT1_vs_date)

}

#GRAPH OF Year VS MINIMUM DAY OF YEAR MINUS LAT BIAS MINUS SS BIAS


createYearVMinDOYMLatMSS = function(data) {

doy = data$dayOfYear
date = data$date
lat = data$latitude

Lat_DOY = lm(doy ~ lat)
doy = doy - (Lat_DOY$coefficients[[2]] * lat + Lat_DOY$coefficients[[1]])

mu = mean(doy)
stdev = sd(doy)

dateFloor = floor(date)
yearList = c(min(dateFloor):max(dateFloor))

minDOYsT2 = sapply(yearList,returnTransformedMinDOY, years=dateFloor, DOYs=doy,mu=mu, sd=stdev)
tb = !is.na(minDOYsT2)
minDOYsT2 = minDOYsT2[tb]
yearList1 = yearList[tb]
minT2_vs_date = lm(minDOYsT2 ~  yearList1)

print(paste("DateVMinDOYMLatMSS", format(summary(minT2_vs_date)$coefficients[,4][[2]],format="e", digits=2)))
#print(summary(minT2_vs_date))

plot(yearList1,minDOYsT2, main=NA, xlab= paste("Year\np-value =", format(summary(minT2_vs_date)$coefficients[,4][[2]],format="e", digits=2)), ylab="Minimum Day of Year", pch=20, col="blue")
abline(minT2_vs_date)

}

