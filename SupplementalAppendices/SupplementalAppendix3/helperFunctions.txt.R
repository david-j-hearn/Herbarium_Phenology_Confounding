print("Opening code from Pearse et al. 2017") 
source("Weibull_PeaseEstimation.txt.r")

#_________________________________________________________
#
# Author: David J. Hearn
# Citation: 
# Use Policy: Free to use and modify provided citation is included in any resulting works
#
# Description: 
#	Helper functions.
#	These functions are needed by other functions 
#		and are not intended to be called separately.
#	Therefore, details are not provided here, 
#		but each function is headed by a brief description
#	Use the R ‘source’ function to make these functions available to other functions
#
#FUNCTIONS:
#
# expectedValue_discreteApprox
# variance_discreteApprox
# getLogsplineDensity
# getExpected_MinimumLogsplineDensity
# getVariance_MinimumLogsplineDensity
# getExpected_MinimumLogsplineDensity_Repeat
# getVariance_MinimumLogsplineDensity_Repeat
# kthMinXDist_Normal
# expectedValueMinX_discreteNormalApprox
# varianceMinX_discreteNormalApprox
# sampleMinD
# sampleMinRepeatedD
# sampleMinG
# sampleMinRepeatedG
# returnMinDOY
# returnTransformedMinDOY
# minNormal
# expectedMinGaussianApproximation(mu,stdev,N)
#	might be useful as a stand-alone function
#		returns the approximate expected minimum 
#			of a sample from a Gaussian (normal) distribution 
#			with provided input:
#				mean (mu)
#				standard deviation (stdev)
#				sample size (N)
#
#_________________________________________________________


#for trapezoidal rule
expectedValue_discreteApprox = function(x,y,dx) {
	len = length(x)
	val = (x %*% (y*dx)) - x[1]*y[1]*dx/2 - x[len]*y[len]*dx/2
return( val )
}

#trapezoidal rule
variance_discreteApprox = function(x,y,dx) {
	len = length(x)
	mu = expectedValue_discreteApprox(x,y,dx)
	mu = rep(mu,len)
	val = ((x-mu)^2) %*% (dx*y) - ((x[1]-mu[1])^2)*y[1]*dx/2 - ((x[len]-mu[len])^2)*y[len]*dx/2
	return( val ) 
}


#_________________________logspline methods__________________________
if(!require(logspline)) { install.packages("logspline") }

library(logspline)

getLogsplineDensity = function(data) {
return(logspline(x=data$dayOfYear))
}

getExpected_MinimumLogsplineDensity = function(N, x, logsplineDensity) {

y=N*dlogspline(x,fit=logsplineDensity)* ((1-plogspline(x,fit=logsplineDensity))^(N-1))
dx = x[2]-x[1]

xBar = expectedValue_discreteApprox(x,y,dx)
return(xBar)

}

getVariance_MinimumLogsplineDensity = function(N, x, logsplineDensity) {

y=N*dlogspline(x,fit=logsplineDensity)* ((1-plogspline(x,fit=logsplineDensity))^(N-1))
dx = x[2]-x[1]
var = variance_discreteApprox(x,y,dx)
return(var)

}


getExpected_MinimumLogsplineDensity_Repeat = function(data, maxSS) {
logsplineDensity = getLogsplineDensity(data)

mean=mean(data$dayOfYear)
sd = sd(data$dayOfYear)

dx=0.05
x = seq(mean-4*sd,mean+3*sd,by=dx)

xBar = sapply(1:maxSS,getExpected_MinimumLogsplineDensity,x=x,logsplineDensity=logsplineDensity)
	
return(xBar)
}

getVariance_MinimumLogsplineDensity_Repeat = function(data, maxSS) {
logsplineDensity = getLogsplineDensity(data)

mean=mean(data$dayOfYear)
sd = sd(data$dayOfYear)

dx=0.05
x = seq(mean-4*sd,mean+3*sd,by=dx)

var = sapply(1:maxSS,getVariance_MinimumLogsplineDensity,x=x,logsplineDensity=logsplineDensity)

return(var)
}


#__________________________________________________________________



#resamples from data with replacement and returns the minimum value of the sample

sampleMinD <- function(n,data) {
      x = min(sample(data, size=n, replace=T))
      return(x)
}

#Repeats the procedure of: resamples from data with replacement and returns the minimum value of the sample
#Depending on return type, it returns the mean (returnType=1), sd (2), 95% CI lower limit (3), 95% CI upper limit (4), or data

sampleMinRepeatedD <- function(n,data,nRep,returnType=T) {
      reps = sapply(rep(n,nRep),sampleMinD,data)
      if(returnType==1) {
      return(mean(reps))
      }
      else if(returnType==2) {
      return(sd(reps))
      }

else if(returnType==3) {
	return(	findCILimit_data(alpha=0.05,reps,lower=T))
	}

else if(returnType==4) {
	return(	findCILimit_data(alpha=0.05,reps,lower=F))
	}

      else { 
	      return(reps) 
      }
}

#samples from a normal distribution (Gaussian, G) with provided mean and sd and returns the minimum of the sample

sampleMinG <- function(n,mu,stdev) {
      x = min(rnorm(n=n,mean=mu,sd=stdev))
      return(x)
}

#Repeats the procedure: samples from a normal distribution (Gaussian, G) with provided mean and sd and returns the minimum of the sample
#Depending on return type, it returns the mean (returnType=1), sd (2), or data (3)


sampleMinRepeatedG <- function(n,mu,stdev,nRep,returnType=T) {
      reps = sapply(rep(n,nRep),sampleMinG,mu=mu,stdev=stdev)
      if(returnType==1) {
      return(mean(reps))
      }
      else if(returnType==2) {
      return(sd(reps))
      }
else if(returnType==3) {
	return(	findCILimit_data(alpha=0.05,reps,lower=T))
	}

else if(returnType==4) {
	return(	findCILimit_data(alpha=0.05,reps,lower=F))
	}
      else { 
      return(reps) 
      }
}


kthMinXDist = function(x,k,N,f,F,...) {
return(N*f(x,...)*choose(N-1,k-1)*(F(x,...)^(k-1))*((1-F(x,...))^(N-k)))
}

expectedValueMinX_discreteApprox = function(x,k,N,f,F,...) {
if(length(x)<3) {
stop("The domain of the density function needs to be discretized into more than 2 points")
}
dx = x[2]-x[1]
y = kthMinXDist(x,k,N,f,F,...)
return(expectedValue_discreteApprox(x,y,dx))
}

varianceMinX_discreteApprox = function(x,k,N,f,F,...) {

if(length(x)<3) {
stop("The domain of the density function needs to be discretized into more than 2 points")
}
dx = x[2]-x[1]
y = kthMinXDist(x,k,N,f,F,...)
return(variance_discreteApprox(x,y,dx))
}


kthMinXDist_Normal = function(x,k,N,mean,sd) {

return(N*dnorm(x,mean,sd)*choose(N-1,k-1)*(pnorm(x,mean,sd)^(k-1))*((1-pnorm(x,mean,sd))^(N-k)))

}

expectedValueMinX_discreteNormalApprox = function(N,x,mean,sd,dx) {

y = kthMinXDist_Normal(x,1,N,mean,sd)
return(expectedValue_discreteApprox(x,y,dx))

}

varianceMinX_discreteNormalApprox = function(N,x,mean,sd,dx) {

y = kthMinXDist_Normal(x,1,N,mean,sd)
return(variance_discreteApprox(x,y,dx))

}

findCILimit_data = function(alpha=0.05,x,lower=T)
	{
	copy = sort(x)
	if(lower) {
		index = floor(0.5 + length(x) * alpha / 2)
		}
	else {
		index = floor(0.5 + length(x) * (1 - alpha/2))
		}
	return(copy[index])
	}

findCILimit_minGaussian = function(N, tol=0.001, inc=1000, alpha=0.05, mean, sd, lower=T ,maxIters=100) {

exp = expectedMinGaussianApproximation(mean,sd,N)
l = exp - 4*sd
u = exp + 3*sd

al = l
#print(paste("Gaussian sample size, exp, l, u, al, tol, inc, alpha, mean, sd, lower, maxIters:", N, exp, l, u, al, tol, inc, alpha, mean, sd, lower, maxIters))

target = 1 - alpha/2
if(lower) { target = alpha/2 }

error = 1
cnt=1
while(error>tol || cnt > maxIters)
	{
	mid = l + (u-l)/2
	cnt = cnt+1	

	x=seq(al,mid,(mid-al)/inc)
	dx=x[2]-x[1]
	y = kthMinXDist_Normal(x,1,N,mean,sd)
	a =  sum(dx*y) 

	if(is.na(a)) { stop("a is NA in findCILimit_minGaussian") }

	error = abs(target-a)
	if(error<=tol) { 
		#print(paste("\tCI limit:", mid))
	return(mid) 

}
	else if(a < target) {
		l=mid
		} 
	else if(a >= target) {
		u=mid
		}
	}
stop("findCILimit_minGaussian: Failed to converge")
}

findCILimit_minLogspline = function(N, tol=0.001, inc=1000, alpha=0.05, mean, sd, lower=T,logsplineDensity,maxIters=100) {

x=seq(mean-6*sd,mean+3*sd,by= 9*sd/inc,)
exp = getExpected_MinimumLogsplineDensity(N, x, logsplineDensity)
 l = exp - 4*sd
u = exp + 3*sd
al = l

print(paste("sample size, exp, l, u, al:", N, exp, l, u, al))

target = 1 - alpha/2
if(lower) { target = alpha/2 }

error = 1
cnt=1
while(error>tol || cnt > maxIters)
	{
	cnt = cnt+1
	mid = l + (u-l)/2
	
	x=seq(al,mid,(mid-al)/inc)
	dx=x[2]-x[1]
	y=N*dlogspline(x,fit=logsplineDensity)* ((1-plogspline(x,fit=logsplineDensity))^(N-1))
	a =  sum(dx*y) 

	#print(paste("l, u, mid, a, lower, N, dx:", l,u,mid,a,lower,N,dx))

if(is.na(a)) { stop(paste("a is NA in findCILimit_minLogspline", N, lower, maxIters, tol, inc, alpha, mean, sd, l, u, mid, cnt, y, dx, x)) }
	
	error = abs(target-a)
	if(error<=tol) { 

#print(paste("\tCI limit:", mid))

return(mid) 
}
	else if(a < target) {
		l=mid
		} 
	else if(a >= target) {
		u=mid
		}
	}
stop("findCILimit_minLogspline: Failed to converge")
}



# returns the minimum DOY for a provided year

returnMinDOY = function(year, years, DOYs) 
{
      DOYs = DOYs[years == year]
      N = length(DOYs)
      if(N==0)  {
      return(NA)
      }
      return(min(DOYs))
}

#returns the approximate expected minimum of a sample from a Gaussian with provided mean, SD

expectedMinGaussianApproximation <- function(mu,stdev,N) {
      ev = mu+stdev*qnorm((1-(pi/8))/(N-(pi/4)+1))
      return(ev)
}

#returns the transformed min DOY based on normal approximation for a given year

returnTransformedMinDOY = function(year, years, DOYs, mu, sd, minSS=1, MCReps=500, dx=0.05, type="AA") 
{
	DOYOrig = DOYs
      DOYs = DOYs[years == year]
      N = length(DOYs)
      if(N<minSS)  {
      return(NA)
      }
      minDOY = min(DOYs)

	x = seq(mu-4*sd,mu+3*sd,dx)
	dayOfYear = DOYOrig
	data = data.frame(dayOfYear)

	exp=0
	if(type=="AA")
		{
		exp = expectedMinGaussianApproximation(mu=mu,stdev=sd,N=N)
		}
	else if(type == "GMC")
		{
		exp = sampleMinRepeatedG(n=N,mu=mu,stdev=sd, returnType=1, nRep= MCreps)
		}
	else if(type == "DMC")
		{
		exp = sampleMinRepeatedD(n=N,nRep=MCReps,data=data$dayOfYear, returnType=1)
		}
	else if(type == "DNA")
		{
		exp = expectedValueMinX_discreteNormalApprox(N=N,x=x, mean=mu, sd=sd, dx=dx)
		}
	else if(type == "LSD")
		{
		exp=getExpected_MinimumLogsplineDensity(N=N,x=x,logsplineDensity=getLogsplineDensity(data))
		}
	else if(type == "WD")
		{
		exp = est.limit(x=DOYs)[[1]]
		}
	else {
		warning("Unsupported type")
		return(NA)
	}

      return(minDOY - exp)
}

#creates the distribution of the minimum of a random sample from a normally distributed population

minNormal = function(rmin,rmax,mean,var,n) {
d = seq(from=rmin+mean,to=rmax+mean,by=(rmax-rmin)/1000)
f=n*dnorm(d,mean,var)*(1-pnorm(d,mean,var))^(n-1)
#plot(d,f,xlim=range(d),ylim=range(f), type="l")
return(list("x"=d,"y"=f))
}



