source("../SharedCode/helperFunctions.txt.R")

if(!require(fitdistrplus)) { install.packages(fitdistrplus) }
library(fitdistrplus)

createSS_V_beta = function(
population.sizes=c(100,1000,1000000),
col=c("red","black","blue"), fill=c(rgb(1,0,0,0.1),rgb(.5,.5,.5,.5),rgb(0,0,1,0.1)),
pch=c(1,20,3), cex=c(0.5,0.25,0.15), lty=c(4,3,2), 
minSS=7,maxSS=90,by=1,
nReps=500,
min=1,max=366,
s1= 68.93748,s2=129.846,
outFile=NA,
show.legend=FALSE) 
{


ss = seq(minSS, maxSS, by = by)
nss = length(ss)

if(!is.na(outFile)) 
	{
	pdf(outFile)
	}

plot(NULL, xlab="Sample Size", ylab="Expected Minimum DOY", ylim=c(40,120), xlim=c(minSS,maxSS))

psCnt=1
for(population.size in population.sizes) 
	{

	print(paste("Population size: ", population.size))
	true.min = rep(0,nss)

	ll = rep(0,nss)
	ul = rep(0,nss)

	yave = rep(0,nss)

	x = double(nss*nReps)
	y = double(nss*nReps)

	x.seq = seq(0.00001,0.9999, 0.001)

	for( i in 1:nReps ) 
		{
		data.raw = rbeta(population.size, s1,s2)
		data = data.raw * (max - min) + min
		t.min = min(data)
		cnt = 1
		for(j in ss) 
			{
			true.min[cnt] = true.min[cnt] + t.min
			sub = sample(size=j,x=data.raw,replace=F)
			fdb = fitdist(sub,"beta")
			shape1 = fdb$estimate[[1]]
			shape2 = fdb$estimate[[2]]

			el = expectedValueMinX_discreteApprox(x=x.seq,k=1,N=population.size, dbeta,pbeta,shape1=shape1,shape2=shape2) *(max-min)+min
			print(paste("el, rep, ss, shape1, shape2, pop.size", el, i, j, shape1, shape2, population.size))

#el = expectedMinGaussianApproximation(mu=mean(sub), stdev=sd(sub),population.size)
			x[(i-1)*nss+cnt] = j
			y[(i-1)*nss+cnt] = el
			cnt = cnt+1
			}
		}
	true.min = true.min / nReps

	cnt = 1
	for(j in ss) 
		{

		vals = y[which(x %in% j)]

		ll[cnt] = findCILimit_data (alpha=0.05,x=vals,lower=T)
		ul[cnt] = findCILimit_data (alpha=0.05,x= vals,lower=F)
		yave[cnt] = mean(vals)
		cnt = cnt+1
		}

	lines(ss,true.min, col="white", lwd=3)
	lines(ss,true.min, col=col[psCnt])


	polygon(c(ss,rev(ss)), c(ll,rev(ul)), col = fill[psCnt], border=NA )
	lines(ss,ll,col=col[psCnt])
	lines(ss,ul,col=col[psCnt])
	lines(ss,yave,col=col[psCnt],lty=lty[psCnt],lwd=2)

	psCnt = psCnt+1
	}


if(show.legend) { legend(55,70, legend=population.sizes,  col=col, pch=rep(20,length(col))) }

if(!is.na(outFile)) 
	{
	dev.off()
	}
}

createSS_V_est.limit = function(
population.sizes=c(100,1000,1000000),
col=c("red","black","blue"), fill=c(rgb(1,0,0,0.1),rgb(.5,.5,.5,.5),rgb(0,0,1,0.1)),
pch=c(1,20,3), cex=c(0.5,0.25,0.15), lty=c(4,3,2), 
minSS=7,maxSS=90,by=1,
nReps=500,
min=1,max=366,
s1= 68.93748,s2=129.846,
outFile=NA,
show.legend=FALSE) 

{

ss = seq(minSS, maxSS, by = by)
nss = length(ss)

if(!is.na(outFile)) {
	pdf(outFile)
		}

plot(NULL, xlab="Sample Size", ylab="Expected Minimum DOY", ylim=c(40,120), xlim=c(minSS,maxSS))


psCnt=1
for(population.size in population.sizes) {
	print(paste("Population size: ", population.size))
true.min = rep(0,nss)
ll = rep(0,nss)
ul = rep(0,nss)
xave = rep(0,nss)
yave = rep(0,nss)
	x = double(nss*nReps)
	y = double(nss*nReps)

for( i in 1:nReps ) {
	print(i)
data = rbeta(population.size, s1,s2) * (max - min) + min
t.min = min(data)
cnt = 1
for(j in ss) {
	true.min[cnt] = true.min[cnt] + t.min
	#print(paste("\t",(i-1)*nss+cnt))
sub = sample(size=j,x=data,replace=F)
#est.limit(x=output[[2]], k=SS, upper=TRUE, alpha=0.05)
#el = est.limit(-1*sub,k=length(sub), upper=TRUE, alpha=0.05 )
#x[(i-1)*nss+cnt] = j
#y[(i-1)*nss+cnt] = -1*el[[1]]
#ll[cnt] = ll[cnt] + -1*el[[2]]
#ul[cnt] = ul[cnt] + -1*el[[3]]
el = est.limit(sub,k=length(sub), upper=FALSE, alpha=0.05 )
x[(i-1)*nss+cnt] = j
y[(i-1)*nss+cnt] = el[[1]]
ll[cnt] = ll[cnt] + el[[2]]
ul[cnt] = ul[cnt] + el[[3]]

cnt = cnt+1
}
	}
true.min = true.min / nReps
ll = ll/nReps
ul = ul/nReps

cnt = 1
for(j in ss) {

	vals = y[which(x %in% j)]

	yave[cnt] = mean(vals)
	cnt = cnt+1

}


#points(x,y, col=col[psCnt],pch=pch[psCnt],cex=cex[psCnt])

lines(ss,true.min, col="white", lwd=3)
lines(ss,true.min, col=col[psCnt])

polygon( c(ss,rev(ss)), c(ll,rev(ul)), col = fill[psCnt], border=NA )
lines(ss,ll,col=col[psCnt])
lines(ss,ul,col=col[psCnt])
lines(ss,yave,col=col[psCnt],lty=lty[psCnt],lwd=2)


#lmt = lm(y ~ x)
#lml = lm(y ~ log(x))
#curve(summary(lml)$coefficients[1]+ summary(lml)$coefficients[2]*log(x), add=T)
#print(summary(lmt))
#abline(lmt, lty=lty[psCnt], col=col[psCnt],lwd=2)


psCnt = psCnt+1
	}

#legend("bottomright", legend=population.sizes, col=col, pch=pch)

if(show.legend) { legend(65,70, legend=population.sizes,  col=col, pch=rep(20,length(col))) }

if(!is.na(outFile)) {
	dev.off()
	}
}


createWeibullComparisonGraph = function( 
population.sizes=c(100,1000,1000000),
col=c("red","black","blue"), fill=c(rgb(1,0,0,0.1),rgb(.5,.5,.5,.3),rgb(0,0,1,0.1)),
pch=c(1,20,3), cex=c(0.5,0.25,0.15), lty=c(4,3,2), 
minSS=10,maxSS=75,by=1,
nReps=1000,
min=1,max=366,
s1= 68.93748,s2=129.846,
outFile=NA) 
{

if(!is.na(outFile)) {
pdf(outFile)
}

par(mar=c(0, 0, 0, 0))


layout(matrix(c(1,2,2,2,2,3,3,3,3,3),2,5, byrow=T))
#par(mai=c(1, 1, 0.1, 0.1))

#plot(NULL , bty='n',ylab= NA,xlab= NA, xlim=0:1, ylim=0:1)

#par(mai=c(0,0,0,0))
par(mar=c(0, 0, 0, 0))


plot(NULL,type='n',axes=FALSE,ann=FALSE, xlim=0:1, ylim=0:1)

#legend("center", legend =population.sizes, pch=rep(20,length(population.sizes)), pt.cex=3, cex=1.5, bty='n', col = col)
legend("center", legend =population.sizes, pch=rep(20,length(population.sizes)), bty='n', col = col, pt.cex=3, cex=1.5)
text(.5,.7,"Population size",cex=1.5)
#mtext("Population size",cex=1.5)



createSS_V_est.limit(
population.sizes=population.sizes,
col=col, fill=fill,
pch=pch, cex=cex, lty=lty, 
minSS=minSS, maxSS=maxSS, by=by,
nReps=nReps,
min=min, max=max,
s1= s1, s2=s2
)
createSS_V_beta(
population.sizes=population.sizes,
col=col, fill=fill,
pch=pch, cex=cex, lty=lty, 
minSS=minSS, maxSS=maxSS, by=by,
nReps=nReps,
min=min, max=max,
s1= s1, s2=s2
)


if(!is.na(outFile)) {
dev.off()
}

}
