if(!require(plotrix)) {
install.packages(plotrix)
}

library(plotrix)

makeNormalDistInset = function(mean,sd,ulx,uly,width,height,col,title="Distribution of DOY\nfor every date") {

heightText = 0.2*height
height=0.8*height

x=seq(from=mean-3*sd, to=mean+3*sd, by=0.1)
y=dnorm(x,mean,sd)
rX = max(x) - min(x)
x = ulx+width*(x - min(x)) /rX
rY = max(y)-min(y)
y = uly - height + height * (y - min(y)) / rY
polygon(x,y,col=col)
segments(x0=ulx,y0=uly,x1=ulx,y1=uly-height)
text(ulx+width/2,uly-(heightText+height)-0.5,title)
}

addXAxis = function(xmin,xmax,ymin,ymax,label,col=rgb(1,0,0,0.1)) {
x=c(xmin,xmax, xmax,xmin)
y=c(ymax-0.8*(ymax-ymin), ymax-0.8*(ymax-ymin),ymin,ymin)
polygon(x,y,col=col)

text((xmax-xmin)/2,ymax-0.9*(ymax-ymin),label)
}

addYAxis = function(xmin,xmax,ymin,ymax,label,col=rgb(1,0,0,0.1),yoffset=0) {
x=c(xmin,xmin+0.1*(xmax-xmin), xmin+0.1*(xmax-xmin),xmin)
y=c(ymax,ymax,ymin,ymin)
polygon(x,y,col=col)
text(0.05*(xmax-xmin),ymax-(ymax-ymin)/2+yoffset,label,srt=90)

}

addPanelCircle = function(xmin,xmax,ymin,ymax,label,col=rgb(1,0,0,0.1)) {
x = (xmin + 0.1*(xmax-xmin))/2
y = ymax - 0.9*(ymax-ymin)
draw.ellipse(x=x,y=y,a=0.05*(xmax-xmin),b=0.1*(ymax-ymin),border="black", col=col)
text(x,y,label,font =2)

}

pdf("Figure1_BiasIntroduction.pdf")
layout(matrix(c(1,2,3,4,5,6,7,8), 4,2,byrow=TRUE))
par(mar = rep(0, 4))

#1
curve(-1*x+45,from=0,to=10,ylim=c(15,50), xlab="Date", ylab="Latitude",xaxt='n',yaxt='n')
addXAxis(0,10,15,50,"Date")
addYAxis(0,10,15,50,"Latitude")
addPanelCircle(0,10,15,50,"A",col="white")

#2
plot(1, type="n", xlab="Date", ylab="Day of year", axes=FALSE, frame.plot=T, xlim=c(0, 10), ylim=c(0, 10))
segments(x0=0,x1=10,y0=3,y1=3)
makeNormalDistInset(10,3,6,10,4,3,rgb(0,1,0,0.1))
addXAxis(0,10,0,10,"Date",col=rgb(0,0,1,0.1))
addYAxis(0,10,0,10,"Day of year",col=rgb(0,0,1,0.1))
addPanelCircle(0,10,0,10,"B",col="white")

#3
curve(0.2*x+1,from=0,to=10,ylim=c(0,4), xlab="Latitude", ylab="Day of year",xaxt='n',yaxt='n')
addXAxis(0,10,0,4,"Latitude")
addYAxis(0,10,0,4,"Day of year")
addPanelCircle(0,10,0,4,"C",col="white")

#3 (empty)
#plot(1, type="n", xlab="", ylab="", axes=FALSE, frame.plot=F, xlim=c(0, 10), ylim=c(0, 10))
#text(5,5,"Add the above two components \nto produce the graph with artifactual correlation -->")

#4
curve(-0.5*x+45,from=0,to=10,ylim=c(15,50), xlab="Date", ylab="Day of year",xaxt='n',yaxt='n')
addXAxis(0,10,15,50,"Date")
addYAxis(0,10,15,50,"Day of year")
text(5,32.5,"Add components from panels A, B, C\nresulting in model with\nartifactual correlation")
addPanelCircle(0,10,15,50,"D",col="white")

#5
curve(0.05*x+1,from=0,to=10,ylim=c(0,4), xlab="Year", ylab="Sample size",xaxt='n',yaxt='n')
addXAxis(0,10,0,4,"Year")
addYAxis(0,10,0,4,"Sample size")
addPanelCircle(0,10,0,4,"E",col="white")

#6
curve(-0.5*x+7,from=0,to=10,ylim=c(0,10), xlab="Year", ylab="Minimum day of year",xaxt='n',yaxt='n')
addXAxis(0,10,0,10,"Year")
addYAxis(0,10,0,10,"Minimum day of year",yoffset=1)
text(5,8,"Add components from panels A, B, C, E\nresulting in model with\nartifactual correlation")
addPanelCircle(0,10,0,10,"F",col="white")

#7
plot(1, type="n", xlab="", ylab="", axes=FALSE, frame.plot=F, xlim=c(0, 10), ylim=c(0, 10))
text(5,5,"Remove latitudinal sampling bias (panel A) \nand sample size bias (panel E) to produce\nthe model with the true correlation (panel H)")
addPanelCircle(0,10,0,10,"G",col="white")

#8
plot(1, type="n", xlab="Year", ylab="Minimum day of year", axes=FALSE, frame.plot=T, xlim=c(0, 10), ylim=c(0, 10))
segments(x0=0,x1=10,y0=3,y1=3)
addXAxis(0,10,0,10,"Year",col=rgb(0,0,1,0.1))
addYAxis(0,10,0,10,"Minimum day of year",col=rgb(0,0,1,0.1) ,yoffset=1)
addPanelCircle(0,10,0,10,"H",col="white")

dev.off()
