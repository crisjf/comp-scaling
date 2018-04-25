#================#
# 0 - LOAD DATA  #
#================#

options(stringsAsFactors = FALSE)

# for patents
setwd("C:/Dropbox/University/1. Research/1. Papers/1. In progress/[1] Economic Complexity & Urban Scaling/1_Bimodal/Patents [USA]")
pat = read.csv("msa.pat.all.csv")

# for publications
setwd("C:/Dropbox/University/1. Research/1. Papers/1. In progress/[1] Economic Complexity & Urban Scaling/1_Bimodal/Publications [USA]")
pub = read.csv("msa.pub.all.csv")

# for occupations
setwd("C:/Dropbox/University/1. Research/1. Papers/1. In progress/[1] Economic Complexity & Urban Scaling/1_Bimodal/Occupations [USA]")
occ = read.csv("msa.emp.all.csv")

# for industries
setwd("C:/Dropbox/University/1. Research/1. Papers/1. In progress/[1] Economic Complexity & Urban Scaling/1_Bimodal/Industries [USA]")
ind = read.csv("msa.GDP.all.csv")

library(plotrix)

setwd("C:/Dropbox/University/1. Research/1. Papers/1. In progress/[1] Economic Complexity & Urban Scaling/5_Scaling")

pdf("Scaling-All.pdf",width=7*2,height=7*2)

par(mfrow=c(2,2))


#==============#
# 1 - PATENTS  #
#==============#

mini = min(log(pat$pop.2000))
maxi = round (max(log(pat$pop.2000)), digits = 0)

op <- par(cex.main = 2.5, mar = c(6, 6, 6, 6) + 0.1, mgp = c(3.5, 1, 0), 
          cex.lab = 1.75 , font.lab = 1.75, cex.axis = 1.75, bty = "n", las = 1)

plot(log(pat$pop.2000), log(pat$pat.2000), 
     xlim = c(mini, maxi), ylim = c(1,12),
     col = "black", pch = 21, bg = "firebrick3", cex = 2,
     ylab = "", xlab = "", axes = FALSE)
axis(1)
axis(2) 

reg1 <- lm(log(pat$pat.2000) ~ log(pat$pop.2000))

ablineclip(reg1, lwd = 5, x1 = mini, x2 = maxi, col = "firebrick3") 
par(las = 0)
ablineclip(a = -7.16, b= 1, col = "black", lwd = 5, x1 = mini, x2 = maxi)

mtext("Log Population", side = 1, line = 3.7, cex = 2)
mtext("Log Patents", side = 2, line = 3.7, cex = 2)
text(12, 11, 
     paste("Beta =", round(summary(reg1)$coefficients[2, 1], digits = 2)), 
     cex = 2, col = "black")
title(main="Technological Scaling")


#===================#
# 2 - PUBLICATIONS  #
#===================#

mini = min(log(pub$pop.2010))
maxi = round (max(log(pub$pop.2010)), digits = 0)

op <- par(cex.main = 2.5, mar = c(6, 6, 6, 6) + 0.1, mgp = c(3.5, 1, 0), 
          cex.lab = 1.75 , font.lab = 1.75, cex.axis = 1.75, bty = "n", las = 1)

plot(log(pub$pop.2010), log(pub$pub.2010), 
     xlim = c(mini, maxi), ylim = c(1,15),
     col = "black", pch = 21, bg = "blue", cex = 2,
     ylab = "", xlab = "", axes = FALSE)
axis(1)
axis(2) 

reg1 <- lm(log(pub$pub.2010) ~ log(pub$pop.2010))

ablineclip(reg1, lwd = 5, x1 = mini, x2 = maxi, col = "blue") 
par(las = 0)
ablineclip(a = -6, b= 1, col = "black", lwd = 5, x1 = mini, x2 = maxi)

mtext("Log Population", side = 1, line = 3.7, cex = 2)
mtext("Log Publications", side = 2, line = 3.7, cex = 2)
text(12, 13.5, 
     paste("Beta =", round(summary(reg1)$coefficients[2, 1], digits = 2)), 
     cex = 2, col = "black")
title(main="Scientific Scaling")


#==================#
# 3 - Occupations  #
#==================#

mini = min(log(occ$pop.est.2015))
maxi = round (max(log(occ$pop.est.2015)), digits = 0)

op <- par(cex.main = 2.5, mar = c(6, 6, 6, 6) + 0.1, mgp = c(3.5, 1, 0), 
          cex.lab = 1.75 , font.lab = 1.75, cex.axis = 1.75, bty = "n", las = 1)

plot(log(occ$pop.est.2015), log(occ$emp.2015), 
     xlim = c(mini, maxi), ylim = c(9,16),
     col = "black", pch = 21, bg = "darkgreen", cex = 2,
     ylab = "", xlab = "", axes = FALSE)
axis(1)
axis(2) 

reg1 <- lm(log(occ$emp.2015) ~ log(occ$pop.est.2015))

ablineclip(reg1, lwd = 5, x1 = mini, x2 = maxi, col = "darkgreen") 
par(las = 0)
ablineclip(a = -0.95, b= 1, col = "black", lwd = 5, x1 = mini, x2 = maxi)

mtext("Log Population", side = 1, line = 3.7, cex = 2)
mtext("Log Employment", side = 2, line = 3.7, cex = 2)
text(12, 14.75, 
     paste("Beta =", round(summary(reg1)$coefficients[2, 1], digits = 2)), 
     cex = 2, col = "black")
title(main="Employment Scaling")

#=================#
# 4 - Industries  #
#=================#

mini = min(log(ind$pop.est.2015))
maxi = round (max(log(ind$pop.est.2015)), digits = 0)

op <- par(cex.main = 2.5, mar = c(6, 6, 6, 6) + 0.1, mgp = c(3.5, 1, 0), 
          cex.lab = 1.75 , font.lab = 1.75, cex.axis = 1.75, bty = "n", las = 1)

plot(log(ind$pop.est.2015), log(ind$gdp.2015), 
     xlim = c(mini, maxi), ylim = c(6,15),
     col = "black", pch = 21, bg = "yellow1", cex = 2,
     ylab = "", xlab = "", axes = FALSE)
axis(1)
axis(2) 

reg1 <- lm(log(ind$gdp.2015) ~ log(ind$pop.est.2015))

ablineclip(reg1, lwd = 5, x1 = mini, x2 = maxi, col = "yellow1") 
par(las = 0)
ablineclip(a = -3.3, b= 1, col = "black", lwd = 5, x1 = mini, x2 = maxi)

mtext("Log Population", side = 1, line = 3.7, cex = 2)
mtext("Log Gross Domestic Product", side = 2, line = 3.7, cex = 2)
text(12, 13.5, 
     paste("Beta =", round(summary(reg1)$coefficients[2, 1], digits = 2)), 
     cex = 2, col = "black")

title(main="Industrial Scaling")

###

dev.off()