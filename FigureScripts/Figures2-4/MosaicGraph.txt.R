if(!require(viridis)) { install.packages(viridis) }
if(!require(colorspace)) { install.packages(colorspace) }

library(viridis)
library(colorspace)

rgba2rgb = function(background,color,alpha) {
background = background * 255
color = color * 255
r = floor((1-alpha)*background[1] + alpha*color[1] + 0.5)/255
g = floor((1-alpha)*background[2] + alpha*color[2] + 0.5)/255
b = floor((1-alpha)*background[3] + alpha*color[3] + 0.5)/255
return(c(r,g,b))
}

getRAlpha = function(value) {
#sigmoid
#alpha = 2 * (1/(exp(100 * (value - 0.05)) + 1) - 0.5)
#exponential decay
a = 1
b = log(0.2)/0.05
alpha = a * exp(b * value)
return(alpha)
}

getBAlpha = function(value) {
#sigmoid
#alpha = 2 * (exp(5 * (value - 0.05))/(exp(5 * (value - 0.05)) + 1) - 0.5)
#exponential
b = log(0.1)/(-0.95)
ap = -b
a = exp(ap)
alpha = a*exp(b*value)
return(alpha)
}


makePercentageLegend = function(xUL, yUL, width, height, title="Percent significant") {

	polygon(x=c(xUL,xUL+width,xUL+width,xUL), y=c(yUL,yUL,yUL-height,yUL-height),border="black",col=NA) 

	textHeight = 0.3*height
	text((xUL+width)/2, yUL - 0.5 * textHeight, title)
	
	yUL = yUL - 0.3*height
	height = 0.7*height	

	legendLength = 0.8*width
	gradientHeight = 0.8*height 
	gradientHeightR = 0.75*gradientHeight
	bars = seq(from=0,to=legendLength, by=legendLength/100)
	legendLength = legendLength + bars[2]
	#cols = rainbow(101, start=0.6, end=0.3)
	cols = divergingx_hcl(101, palette="Zissou1")
	nBars = length(bars)
	cnt=0
	for(pos in bars)
		{

       
		xul = xUL+0.1*width + pos 
		xur = xUL+0.1*width + pos + bars[2]
		xlr = xUL+0.1*width + pos + bars[2]
		xll = xUL+0.1*width + pos 
		#yul = yUL-0.1*height
		#yur = yUL-0.1*height
		#ylr = yUL-0.1*height-gradientHeightR
		#yll = yUL-0.1*height-gradientHeightR
		yul = yUL 
		yur = yUL 
		ylr = yUL -gradientHeightR
		yll = yUL -gradientHeightR

       x=c(xul,xur,xlr,xll)
       y=c(yul,yur,ylr,yll)

		cnt = cnt+1
		col = cols[cnt]
		polygon (x=x, y=y, col=col, border=NA)

		}

		xul = xUL+0.1*width 
		xur = xUL+0.1*width + legendLength
		xlr = xUL+0.1*width + legendLength
		xll = xUL+0.1*width
		yul = yUL 
		yur = yUL 
		ylr = yUL - gradientHeightR
		yll = yUL - gradientHeightR
       x=c(xul,xur,xlr,xll)
       y=c(yul,yur,ylr,yll)
		polygon (x=x, y=y, col=NA, border="black")

	text(xUL+0.1*width,yUL -gradientHeight,0)
	segments(x0=xUL+0.1*width,y0= yUL, x1= xUL+0.1*width, y1=yUL-0.05*height-gradientHeightR, col="black")

	text(xUL+0.1*width+legendLength/2,yUL-gradientHeight,50)
segments(x0=xUL+0.1*width+legendLength/2,y0= yUL, x1= xUL+0.1*width+legendLength/2, y1=yUL-0.05*height-gradientHeightR, col="black")

	text(xUL+0.1*width+legendLength,yUL-gradientHeight,100)
segments(x0=xUL+0.1*width+legendLength,y0= yUL, x1= xUL+0.1*width+legendLength, y1=yUL-0.05*height-gradientHeightR, col="black")

}

makeRBLegend = function(xUL,yUL,cutoff,rMinAlpha,bMinAlpha, width, height, title = "p-value")
	{
	polygon(x=c(xUL-3,xUL+width+3,xUL+width+3,xUL-3), y=c(yUL,yUL,yUL-height - 0.1*height,yUL-height - 0.1*height),border="black",col=NA) 

	textHeight = 0.3*height
	text((xUL+width)/2, yUL - 0.5 * textHeight, title)
	
	yUL = yUL - 0.3*height
	height = 0.7*height	

	legendLength = 0.8*width
	gradientHeight = 0.8*height / 2
	gradientHeightR = gradientHeight/2
	bars = seq(from=0,to=legendLength, by=legendLength/100)
	legendLength = legendLength + bars[2]

	nBars = length(bars)

	cnt=0
	for(pos in bars)
		{
       
	#red gradient

		alphaR = getRAlpha(rMinAlpha*cnt/nBars)

		xul = xUL+0.1*width + pos 
		xur = xUL+0.1*width + pos + bars[2]
		xlr = xUL+0.1*width + pos + bars[2]
		xll = xUL+0.1*width + pos 
		yul = yUL + 0.05 * height 
		yur = yUL + 0.05 * height 
		ylr = yUL + 0.05 * height - gradientHeightR
		yll = yUL + 0.05 * height - gradientHeightR
     		x=c(xul,xur,xlr,xll)
    		y=c(yul,yur,ylr,yll)
     		nCol = rgba2rgb(c(1,1,1),c(1,0,0),alphaR)
		polygon (x=x, y=y, col=rgb(nCol[1],nCol[2],nCol[3],1), border=NA)

#blue gradient

		alphaB = getBAlpha(bMinAlpha + (1-bMinAlpha) * cnt/nBars)

		xul = xUL+0.1*width + pos 
		xur = xUL+0.1*width + pos + bars[2]
		xlr = xUL+0.1*width + pos + bars[2]
		xll = xUL+0.1*width + pos 
		yul = yUL - 0.1*height - gradientHeight 
		yur = yUL - 0.1*height - gradientHeight
		ylr = yUL - 0.1*height - gradientHeight - gradientHeightR
		yll = yUL - 0.1*height - gradientHeight - gradientHeightR
       	x=c(xul,xur,xlr,xll)
       	y=c(yul,yur,ylr,yll)
       	nCol = rgba2rgb(c(1,1,1),c(0,0,1),alphaB)
		polygon (x=x, y=y, col=rgb(nCol[1],nCol[2],nCol[3],1), border=NA)
		cnt = cnt+1
		}

#red box and numbering
		xul = xUL+0.1*width 
		xur = xUL+0.1*width + legendLength
		xlr = xUL+0.1*width + legendLength
		xll = xUL+0.1*width
		yul = yUL + 0.05 * height
		yur = yUL + 0.05 * height
		ylr = yUL + 0.05 * height - gradientHeightR
		yll = yUL + 0.05 * height - gradientHeightR
       x=c(xul,xur,xlr,xll)
       y=c(yul,yur,ylr,yll)
		polygon (x=x, y=y, col=NA, border="black")

	text(xUL+0.1*width,yUL  + 0.05 * height -gradientHeight,0)
	segments(x0=xUL+0.1*width,y0= yUL + 0.05 * height, x1= xUL+0.1*width, y1=yUL -gradientHeightR, col="black")

	text(xUL+0.1*width+legendLength/2,yUL + 0.05 * height -gradientHeight,cutoff/2)
segments(x0=xUL+0.1*width+legendLength/2,y0= yUL + 0.05 * height, x1= xUL+0.1*width+legendLength/2, y1=yUL -gradientHeightR, col="black")

	text(xUL+0.1*width+legendLength,yUL + 0.05 * height -gradientHeight,cutoff)
segments(x0=xUL+0.1*width+legendLength,y0= yUL + 0.05 * height, x1= xUL+0.1*width+legendLength, y1=yUL - gradientHeightR, col="black")

#blue box and numbering

		xul = xUL+0.1*width 
		xur = xUL+0.1*width + legendLength
		xlr = xUL+0.1*width + legendLength
		xll = xUL+0.1*width
		yul = yUL-0.1*height - gradientHeight
		yur = yUL-0.1*height - gradientHeight
		ylr = yUL-0.1*height - gradientHeight - gradientHeightR
		yll = yUL-0.1*height - gradientHeight - gradientHeightR
       x=c(xul,xur,xlr,xll)
       y=c(yul,yur,ylr,yll)
		polygon (x=x, y=y, col=NA, border="black")

	text(xUL+0.1*width,yUL - 0.1*height - 2*gradientHeight, cutoff)
	segments(x0=xUL+0.1*width,y0= yUL-0.1*height-gradientHeight, x1= xUL+0.1*width, y1=yUL-0.15*height-gradientHeight - gradientHeightR, col="black")

	text(xUL+0.1*width+legendLength/2,yUL- 0.1*height -2*gradientHeight,(1-cutoff)/2)
	segments(x0=xUL+0.1*width+legendLength/2,y0= yUL-0.1*height - gradientHeight, x1= xUL+0.1*width+legendLength/2, y1=yUL-0.15*height-gradientHeight - gradientHeightR, col="black")

	text(xUL+0.1*width+legendLength,yUL- 0.1*height -2*gradientHeight,1)
	segments(x0=xUL+0.1*width+legendLength,y0= yUL-0.1*height-gradientHeight, x1= xUL+0.1*width+legendLength, y1=yUL-0.15*height-gradientHeight-gradientHeightR, col="black")

	}


makeMosaicGraph = function(data, headerH, nameW, itemW,boxes=c(), boxes.col=c(), out=character(0), percentage=FALSE)
	{
	nCol = ncol(data)
	nRow = nrow(data)
	colNames = names(data)
#create empty plot of appropriate dimensions
	width = nameW + (nCol-1)*itemW
	height = headerH + nRow*itemW
	if(length(out)>0) {
		pdf(out)
	}
	plot(1, type="n", xlab="", ylab="", axes=FALSE, frame.plot=F, xlim=c(0, width), ylim=c(0, height))

if(percentage) {
	#pal = rainbow(101, start=0.6, end=0.3)
	pal = divergingx_hcl(101, palette="zissou1")
	makePercentageLegend(xUL=0+0.01*width,yUL=height, width=0.3*width, height=0.25*height, title="Percent significant")

}

else {

makeRBLegend(xUL=0+0.01*width,yUL=height+5,cutoff=0.05,rMinAlpha=0.1,bMinAlpha=0.1, width=0.3*width, height=0.25*height, title="p-value")

}
       
       text(nameW/2, height-headerH+2, "Taxon", pos=4, srt=90)
       for( j in 2:nCol) 	{
       text(nameW + (j-2)*itemW + itemW/2-3,height-headerH+2,colNames[j], pos=4, srt=90)
       }
       
	for( i in 1:nRow)
		{
		#make a box for the name in each row
		ulx = 0
		uly = height - headerH - (i-1)*itemW
		urx = nameW
		ury = height - headerH - (i-1)*itemW
		llx = 0
		lly = height - headerH - i*itemW
		lrx = nameW
		lry = height - headerH - i*itemW
		polygon( c(ulx,urx,lrx,llx), c(uly,ury,lry,lly), border="black", col=NA)
		text(ulx-3,uly-itemW/2,data[i,colNames[1]],pos=4,font=3)
		for( j in 2:nCol) 
			{
       		ulx = nameW + (j-2)*itemW
			uly = height - headerH - (i-1)*itemW
			urx = nameW + (j-1)*itemW
			ury = height - headerH - (i-1)*itemW
			llx = nameW + (j-2)*itemW
			lly = height - headerH - i*itemW
			lrx = nameW+ (j-1)*itemW
			lry = height - headerH - i*itemW	
			value = data[i,colNames[j]]

			if(!percentage) {
			r=0
			b=0
			g=0
			alpha = 1
			if(value <= 0.05) {
				r = 1
				b = 0
				g = 0
				alpha = getRAlpha(value)
				}	
			else {
				r = 0
				b = 1
				g = 0
				alpha = getBAlpha(value)
				}
			col = rgb(r,g,b,alpha)	
			
			}

			else {
				col = pal[floor(100*value)+1]
				}
			polygon( c(ulx,urx,lrx,llx), c(uly,ury,lry,lly), border="black", col=col)
			}
		}

#Make larger boxes to group columns


ulx=nameW
uly = height+5
urx=nameW + itemW*sum(boxes)
ury = height+5
llx=nameW
lly=0
lrx= nameW + itemW*sum(boxes)
lry=0

polygon( c(ulx,urx,lrx,llx), c(uly,ury,lry,lly), border="black", col=NA, lwd=2)

lwd = 1.25

if(length(boxes.col) != length(boxes))
	{
	boxes.col = rep("black",length(boxes))
	}
	tot=0
	cnt=1
	for(k in boxes) {

	offset = floor(lwd/2)
	if(cnt==1) { offset = 0 }

	ulx = nameW + tot + offset
	uly = height + 5
	urx = nameW + tot + itemW*boxes[cnt] 
	ury = height  + 5
	llx = nameW + tot + offset
	lly = 0
	lrx = nameW + tot + itemW*boxes[cnt] 
	lry = 0	
			
	polygon( c(ulx,urx,lrx,llx), c(uly,ury,lry,lly), border=boxes.col[cnt], col=NA, lwd=lwd)
	tot = tot+itemW*boxes[cnt]
	cnt = cnt+1
		
	}

	if(length(out)>0) {
		dev.off()
	}
	
	}


#assumes vales are between 0 and 1 (probabilities)
makeMosaicGraph.txt = function(files, headerH, nameW, itemW,boxes=c(), boxes.col=c(), out=character(0), percentage=FALSE)
	{
	#special case!!
	if(length(files)==3) 
		{
		data.aa = read.table(files[1], sep='\t', header=T)
		data.dmc = read.table(files[2], sep='\t', header=T)
		data.wd = read.table(files[3], sep='\t', header=T)

		names(data.aa)[names(data.aa) == "YearVMinDOYMLatMSS"] = "YearVMinDOYMLatMSS_AA"
		data.aa$YearVMinDOYMLatMSS_DMC = data.dmc$YearVMinDOYMLatMSS
		data.aa$YearVMinDOYMLatMSS_WD = data.wd$YearVMinDOYMLatMSS

		data.aa = subset(data.aa, select = -c(YearVMinDOYMSS) )

		makeMosaicGraph(data.aa, headerH, nameW, itemW,boxes, boxes.col, out, percentage)	
		}
	else if(length(files)!=1) { 
		stop("Expecting either 3 files or 1 file.") 
		}
	else {
	data = read.table(files[1], sep='\t', header=T)
	data = subset(data, select = -c(YearVMinDOYMSS) )
	makeMosaicGraph(data, headerH, nameW, itemW,boxes, boxes.col, out, percentage) 
		}
	}

#makeMosaicGraph.csv = function(file, headerH, nameW, itemW,boxes=c(), boxes.col=c(), out=character(0) , percentage=FALSE)
#	{
#	data = read.csv(file,header=T)
#	makeMosaicGraph(data, headerH, nameW, itemW,boxes, boxes.col, out, percentage) 
#	}


