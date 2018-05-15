### This script plots scaling exponent
### Over time
### Per complexity values

### Nov. 29, 2017

library(DT)
library (data.table)
library(zoo)
library (EconGeo)
library(plotrix)

plotsegraph <- function(loc, value, sterr, wiskwidth, color = "grey", linewidth = 2) {
  
  w <- wiskwidth/2
  segments(x0 = loc, x1 = loc, y0 = value - sterr, y1 = value + sterr, col = color, 
           lwd = linewidth)
  segments(x0 = loc - w, x1 = loc + w, y0 = value + sterr, y1 = value + sterr, 
           col = color, lwd = linewidth)  # upper whiskers
  segments(x0 = loc - w, x1 = loc + w, y0 = value - sterr, y1 = value - sterr, 
           col = color, lwd = linewidth)  # lower whiskers
}

#=======================#
# 1 - SCALING OVER TIME #
#=======================#


## load the pre-computed scaling exp
setwd("C:/Users/Balla103/Dropbox/University/1. Research/1. Papers/1. In progress/[1] Economic Complexity & Urban Scaling/3_Concentration/Historical Patents [USA]")

# scaling over time 
d = read.csv ("scaling-time-pat.csv")
d$Industry = d$Decade
d$Beta = d$Beta.pat
d$std.err = d$std.err.pat
d = subset (d, d$Decade > 1840)
d = subset (d, d$Decade < 2010)

setwd("C:/Users/Balla103/Dropbox/University/1. Research/1. Papers/1. In progress/[1] Economic Complexity & Urban Scaling/7_Scaling-Time")



pdf("Scaling-Time.pdf",width=10*2,height=8)

par(mfrow=c(1,2))

par(cex.main = 1.5, mar = c(8, 8, 8, 8) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)

plot(d$Industry, d$Beta, ylab = "", xlab = " ", cex = 1.5, 
     ylim = c(0.8, 1.6), 
     xlim = c(1840, 2010), lwd = 2, pch = 21, axes = F, main = " ")

axis(1)

mtext("Decade", side = 1, line = 3, cex = 1.5, font = 2)

axis(2)

par(las = 0)


mtext("Urban Concentration", side = 2, line = 5.2, cex = 1.5, font = 2)
mtext("(Scaling Exponent - Beta)", side = 2, line = 3.7, cex = 1)

points(d$Industry, d$Beta, cex = 1.5, lwd = 2, pch = 21)

plot.errbars <- plotsegraph(d$Industry, 
                            d$Beta,
                            d$std.err, 5, color = "black")  

lines(d$Industry, d$Beta, lwd = 2, type = "c")

title(main="Technological Scaling (1850-2000)")

#ablineclip(h = 1, x1 = 1860, x2 = 2000, lty = 3, col = "black", lwd = 3)

#dev.off()

#==========================#
# 2 - SCALING TOP/LOW COMP #
#==========================#


### load the pre-computed scaling exp
setwd("C:/Users/Balla103/Dropbox/University/1. Research/1. Papers/1. In progress/[1] Economic Complexity & Urban Scaling/3_Concentration/Historical Patents [USA]")

# scaling over time 
d = read.csv ("scaling-quartile-cat-pat.csv")

d = subset (d, d$Quartile == "top25pc")
d = subset (d, d$Decade > 1840)
d = subset (d, d$Decade < 2010)

d$Industry = d$Decade
d$Beta = d$Beta.pat
d$std.err = d$std.err.pat

#setwd("C:/Dropbox/University/1. Research/1. Papers/1. In progress/[1] Economic Complexity & Urban Scaling/7_Scaling-Time")
setwd("C:/Users/Balla103/Dropbox/University/1. Research/1. Papers/1. In progress/[1] Economic Complexity & Urban Scaling/7_Scaling-Time")

#pdf("Scaling-Time-Complex.pdf",width=10,height=8)

#bottom, left, top and right margins 
par(cex.main = 1.5, mar = c(8, 8, 8, 8) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)

plot(d$Industry, d$Beta, ylab = "", xlab = " ", cex = 1.5, 
     ylim = c(0.9, 2), 
     xlim = c(1850, 2000), lwd = 2, pch = 21, axes = F, main = " ")

axis(1)

mtext("Decade", side = 1, line = 3, cex = 1.5, font = 2)

axis(2)

par(las = 0)

mtext("Urban Concentration", side = 2, line = 5.2, cex = 1.5, font = 2)
mtext("(Scaling Exponent - Beta)", side = 2, line = 3.7, cex = 1)


points(d$Industry, d$Beta, cex = 1.5, lwd = 2, pch = 21, col = 'red')

plot.errbars <- plotsegraph(d$Industry, 
                            d$Beta,
                            d$std.err, 5, color = "red")  

lines(d$Industry, d$Beta, lwd = 2, type = "c", col = "red")

### botom

setwd("C:/Users/Balla103/Dropbox/University/1. Research/1. Papers/1. In progress/[1] Economic Complexity & Urban Scaling/3_Concentration/Historical Patents [USA]")
d = read.csv ("scaling-quartile-cat-pat.csv")

d = subset (d, d$Quartile == "top100pc")

d$Industry = d$Decade
d$Beta = d$Beta.pat
d$std.err = d$std.err.pat
d = subset (d, d$Decade > 1840)
d = subset (d, d$Decade < 2010)



points(d$Industry, d$Beta, cex = 1.5, lwd = 2, pch = 21, col = 'blue')

plot.errbars <- plotsegraph(d$Industry, 
                            d$Beta,
                            d$std.err, 5, color = "blue")  

lines(d$Industry, d$Beta, lwd = 2, type = "c", col = "blue")

points(1850, 2, pch = 21, lwd = 2, cex = 1.5, col = 'red')
text(1855, 2, "25% Most complex patents", cex = 1.2, font = 1, adj = 0, col = 'red')

points(1850, 1.8, pch = 21, lwd = 2, cex = 1.5, col = 'blue')
text(1855, 1.8, "25% Least complex patents", cex = 1.2, font = 1, adj = 0, col = 'blue')

title(main="Knowledge Complexity and Urban Concentration (1850-2000)")

ablineclip(h = 1, x1 = 1850, x2 = 2000, lty = 3, col = "black", lwd = 3)


dev.off()



