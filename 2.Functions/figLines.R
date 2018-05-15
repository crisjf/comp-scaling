figLines <- function(d,outfile,ycols,ecols,palette=c()) {
	setEPS()
	postscript(paste0('../4.Results/Figure3/',outfile))

	if (length(palette)==0){
		palette <- c('red','blue')
	}
	nCols <- length(ycols)
	lPalette <- length(palette)
	palette <- rep(palette, times=as.integer(nCols/lPalette)+1)

	plot(c(0),c(0),ylab = "", xlab = " ", cex = 1.5,ylim = c(min(d[,ycols[1]])-0.1,max(d[,ycols[1]])+0.1),xlim = c(1840, 2010), lwd = 2, pch = 21, axes = F, main = " ")

	for (i in seq(length(ycols))) {
		ycol <- ycols[i]
		points(d$Decade, d[,ycol], ylab = "", xlab = " ", cex = 1.5,
		     col=palette[i], lwd = 2, pch = 21)
		plot.errbars <- plotsegraph(d$Decade,
		                          d[,ycol],
		                          d[,ecols[i]], 5, color =palette[i])
		lines(d$Decade, d[,ycol], lwd = 2, type = "c",col=palette[i])
	}

	axis(1)
	mtext("Decade", side = 1, line = 3, cex = 1.5, font = 2)
	axis(2)
	par(las = 0)
	mtext("Urban Concentration", side = 2, line = 5.2, cex = 1.5, font = 2)
	mtext("(Scaling Exponent - Beta)", side = 2, line = 3.7, cex = 1)
	title(main="Technological Scaling (1850-2000)")

	dev.off()
}


figCatLines <- function(d,outfile,ycol,catCol,palette = c(),reverseY=FALSE) {
	if (length(palette)==0){
		palette <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#969696')
	}
	nCats <- length(unique(d[,catCol]))
	lPalette <- length(palette)
	palette <- rep(palette, times=as.integer(nCats/lPalette)+1)


	setEPS()
	postscript(paste0('../4.Results/Figure3/',outfile))

	pad <- 0.05*(max(d[,ycol])-min(d[,ycol]))
	if (reverseY) {
		yrange <- c(max(d[,ycol])+pad,min(d[,ycol])-pad)
	} else {
		yrange <- c(min(d[,ycol])-pad,max(d[,ycol])+pad)
	}
	plot(c(0),c(0),ylab = "", xlab = " ", cex = 1.5,ylim = yrange,xlim = c(1840, 2010), lwd = 2, pch = 21, axes = F, main = " ")

	categories <- unique(d[,catCol])
	categories <- categories[order(categories)]
	i <-1
	for (cat in categories){
		dc = subset (d, d[,catCol] == cat)
		dc = dc[order(dc$Decade), ]
		points(dc$Decade, dc[,ycol], ylab = "", xlab = " ", cex = 1.5,lwd = 2, pch = 21,col=palette[i])
		lines(dc$Decade, dc[,ycol], lwd = 2, type = "c",col=palette[i])
		points(1850, max(d[,ycol])-pad*i,lwd=2,cex=1.5,col=palette[i])
		text(1855, max(d[,ycol])-pad*i, cat, cex = 1., col = "black",font=1,adj=0)
		i <- i+1
	}

	axis(1)
	mtext("Decade", side = 1, line = 3, cex = 1.5, font = 2)
	axis(2)
	par(las = 0)


	mtext("Urban Concentration", side = 2, line = 5.2, cex = 1.5, font = 2)
	mtext("(Scaling Exponent - Beta)", side = 2, line = 3.7, cex = 1)
	title(main="Technological Scaling (1850-2000)")

	dev.off()
}