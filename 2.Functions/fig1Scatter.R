fig1Scatter <- function(df,xcol,ycol,plotName,ylabel) {
  minix = min(log(df[,xcol]))
  maxix = round (max(log(df[,xcol])), digits = 0)
  
  miniy = min(log(df[,ycol]))
  maxiy = round(max(log(df[,ycol])), digits = 0)
  
  setEPS()
  postscript(paste0('../4.Results/Figure1/',plotName, ".eps"))
  
  plot(log(df[,xcol]), log(df[,ycol]), 
       xlim = c(minix, maxix), ylim = c(miniy,maxiy),
       col = "black", pch = 16, bg = "firebrick3", cex = 2,
       ylab = "", xlab = "", axes = FALSE)
  axis(1)
  axis(2) 
  
  reg1 <- lm(log(df[,ycol]) ~ log(df[,xcol]))
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