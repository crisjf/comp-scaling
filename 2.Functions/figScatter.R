library(ggplot2)
library(ggrepel)
library(ggthemes)
library(extrafont)
# font_import("Avenir Next Condensed",prompt=FALSE)

fig1Scatter <- function(df,xcol,ycol,plotName,ylabel,dirName) {
  if (nrow(df)>=3) {
    minix = min(log10(df[,xcol]))
    maxix = round (max(log10(df[,xcol])), digits = 0)
    
    miniy = min(log10(df[,ycol]))
    maxiy = round(max(log10(df[,ycol])), digits = 0)
    
    setEPS()
    if (dirName=='') {
      postscript(paste0('../4.Results/Figure1/',plotName, ".eps"))
    } else {
      postscript(paste0('../4.Results/Figure1/',dirName,'/',plotName, ".eps"))
    }
    
    plot(log10(df[,xcol]), log10(df[,ycol]), 
         xlim = c(minix, maxix), ylim = c(miniy,maxiy),
         col = "black", pch = 16, bg = "firebrick3", cex = 2,
         ylab = "", xlab = "", axes = FALSE)
    axis(1)
    axis(2) 
    
    reg1 <- lm(log10(df[,ycol]) ~ log10(df[,xcol]))
    ablineclip(reg1, lwd = 5, x1 = minix, x2 = maxix, col = "firebrick3") 
    
    df$predicted <- predict(reg1, newdata=df)
    a <- df[df[,xcol]==min(df[,xcol]),'predicted']
    
    par(las = 0)
    ablineclip(a = a - minix, b= 1, col = "black", lwd = 5, x1 = minix, x2 = maxix)
    
    mtext("Log Population", side = 1, line = 3.7, cex = 2)
    mtext(paste("Log ",ylabel), side = 2, line = 3.7, cex = 2)
    text(minix+1, maxiy-1, 
         paste("Beta =", round(summary(reg1)$coefficients[2, 1], digits = 2),'\n R2 =',
               round(summary(reg1)$adj.r.squared,digits=2)), 
         cex = 2, col = "black")
    title(main=paste('Scaling for',plotName))
    
    dev.off()
  }
}

fig2Scatter <- function(pub,xcol,ycol,plotName,xlabel,dirName='',reverseX=FALSE) {
  setEPS()
  print(dirName)
  if (dirName=='') {
    postscript(paste0('../4.Results/Figure2/',plotName, ".eps"))
  } else {
    postscript(paste0('../4.Results/Figure2/',dirName,'/',plotName, ".eps"))
  }

  if (reverseX){
    df <- pub[rev(order(pub[,xcol])),] 
    xrange <- rev(range(df[,xcol]))
  } else {
    df <- pub[order(pub[,xcol]),] 
    xrange <- range(df[,xcol])
  }
  x = df[,xcol]
  y = df[,ycol]
  z = df[,aNameCol]

  plot(x, y, 
       xlim = xrange, ylim = c(min(y)-0.05,max(y)),
       col = "black", pch = 21, bg = "blue", 
       ylab = "", xlab = "", axes = FALSE)

  text(x,y-0.025,z, cex = 0.75)
  axis(1)
  axis(2)

  ablineclip(h = 1, x1 = 1, x2 = 3, lty = 3, col = "black", lwd = 3)
  par(las = 0)

  mtext(xlabel, side = 1, line = 3.7, cex = 2)
  mtext("Scaling exponent", side = 2, line = 3.7, cex = 2)
   
  
   
  text(min(x)+0.1*(max(x)-min(x)),max(y)-0.1,paste("r =", round(cor(x, y), digits = 2)),cex = 2, col = "black")
  
  reg1 <- lm(y ~ x)
  ablineclip(reg1, lwd = 5, x1 = min(x), x2 = max(x), col = "blue")

  dev.off()
}



fig2ScatterByCat <- function(pub,xcol,ycol,catCol,plotName,xlabel,dirName='',reverseX=FALSE) {
  palette <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#969696')

  setEPS()
  if (dirName=='') {
    postscript(paste0('../4.Results/Figure2/',plotName, ".eps"))
  } else {
    postscript(paste0('../4.Results/Figure2/',dirName,'/',plotName, ".eps"))
  }

  if (reverseX){
    df <- pub[rev(order(pub[,xcol])),] 
    xrange <- rev(range(df[,xcol]))
  } else {
    df <- pub[order(pub[,xcol]),] 
    xrange <- range(df[,xcol])
  }
  x = df[,xcol]
  y = df[,ycol]

  nCats <- length(unique(df[,catCol]))
  lPalette <- length(palette)
  palette <- rep(palette, times=as.integer(nCats/lPalette)+1)
  
  plot(c(0),c(0),xlim = xrange, ylim = c(min(y)-0.05,max(y)),
       col = "black", pch = 21, bg = "blue", 
       ylab = "", xlab = "", axes = FALSE)
  j <- 1
  for (i in unique(df[,catCol])) {
    print(i)
    x = df[df[,catCol]==i,xcol]
    y = df[df[,catCol]==i,ycol]
    z = df[df[,catCol]==i,aNameCol]
    points(x,y, ylab = "", xlab = " ", cex = 1.5,lwd = 2, pch = 21,col=palette[j])
    text(x,y-0.025,z, cex = 0.75)
    j <- j+1
  }

  axis(1)
  axis(2)

  ablineclip(h = 1, x1 = 1, x2 = 3, lty = 3, col = "black", lwd = 3)
  par(las = 0)

  mtext(xlabel, side = 1, line = 3.7, cex = 2)
  mtext("Scaling exponent", side = 2, line = 3.7, cex = 2)
   
  x = df[,xcol]
  y = df[,ycol]
  text(min(x)+0.1*(max(x)-min(x)),max(y)-0.1,paste("r =", round(cor(x, y), digits = 2)),cex = 2, col = "black")
  
  reg1 <- lm(y ~ x)
  ablineclip(reg1, lwd = 5, x1 = min(x), x2 = max(x), col = "blue")

  dev.off()
}


fig2ScatterGG <- function(pub,xcol,ycol,ptitle,pxaxis,pyaxis,actType,grayscale=FALSE) {
  if (grayscale) {
    palette <- c('#999999','#000000')
  } else {
    palette <- c('#f68d43','#d62e27')
  }
  
  ggplot(pub,aes(x=pub[,xcol], y=pub[,ycol],label=pub[,aNameCol])) + 
    geom_point(color=palette[1],size=3) +
    geom_smooth(method = "lm", se = FALSE,color=palette[2]) +
    geom_text_repel(segment.size = 0,size=2,max.iter=10000,family="Avenir Next Condensed") + 
    xlab(pxaxis) + 
    ylab(pyaxis) + 
    ggtitle(ptitle) +
    annotate("text", label=paste0("r = ", round(cor(pub[,xcol], pub[,ycol]), digits = 2)),
             x=min(pub[,xcol]), y = max(pub[,ycol]), hjust=0,vjust=1,size=6) + 
    # geom_rangeframe() +
    theme_tufte() + 
    # scale_x_continuous(breaks = extended_range_breaks()(round(pub[,xcol],as.integer(log10(max(pub[,xcol])-min(pub[,xcol])))+1))) +
    # scale_y_continuous(breaks = extended_range_breaks()(round(pub[,ycol],as.integer(log10(max(pub[,ycol])-min(pub[,ycol])))+1))) +
    theme(plot.title = element_text(hjust = 0.5,family='sans',size=18),
          axis.title = element_text(family='sans',size=15),
          axis.text = element_text(family='Avenir Next Condensed',size=13),
          axis.line = element_line(color="black", size = 0.25))
  ggsave(paste0('../4.Results/Figure2/SM/',actType,'_',xcol,'_',ycol,".pdf"), device=cairo_pdf,width = 6, height = 5)
}