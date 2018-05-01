library(plotrix)
options(stringsAsFactors = FALSE)
source("../2.Functions/loadData.R")
source("../2.Functions/scaling.R")
if (!('EconGeo' %in% installed.packages()[,"Package"])) {
  library('devtools')
  install_github("PABalland/EconGeo")
}
library(EconGeo)

actType <- 'techs'

loadUSParams(actType,FALSE)
data <- loadUSActivity(0,actType,aggregate)
region <- loadUSRegs(useDec,year)
comp   <- loadUSComplexity(actType,aggregate)


sizeCol <- 'nb.pat.class'
compCol <- 'age.class'
ycol    <- 'dec'

comp <- read.csv(paste0('../1.Data/',AYfname))
comp <- comp[comp[,ycol]==year,]


comp <- unique(comp[,c(acol,sizeCol,compCol)])
comp    <- merge(comp,unique(read.csv(paste0("../1.Data/",Afname))[,c(acol,aNameCol)]),by=acol)
comp$comp <- comp[,compCol]




beta = scaling(get.matrix(data[,c(rcol,acol,'Ec.Output')]), region[,c(rcol,'pop')])
colnames(beta) <- c(acol,'Beta','r.sq','std.err')

exclude <- c("Agricultural and Biological Sciences","Environmental Science",
             "Earth and Planetary Sciences","Veterinary") # 2-digit for fields
for (n in exclude) {
  comp <- comp[comp[,aNameCol]!=n,]
}

#### FIGURE: 

pub <- merge(beta, comp, by = acol) 

plotName <- 'Scaling'

postscript(paste0('../4.Results/Figure2/',plotName, ".eps"))

x = pub$comp
y = pub$Beta
z = pub[,aNameCol]
cor(x, y)

# op <- par(cex.main = 2.5, mar = c(8, 6, 8, 8) + 0.1, mgp = c(3.5, 1, 0), 
          # cex.lab = 1.75 , font.lab = 1.75, cex.axis = 1.75, bty = "n", las = 1)

plot(x, y, 
     xlim = c(min(x), max(x)), ylim = c(min(y),max(y)),
     col = "black", pch = 21, bg = "blue", cex = 2,
     ylab = "", xlab = "", axes = FALSE)
axis(1)
axis(2) 

ablineclip(h = 1, x1 = 1, x2 = 3, lty = 3, col = "black", lwd = 3)

par(las = 0)

mtext("Knowledge Complexity", side = 1, line = 3.7, cex = 2)
mtext("(Mean Number of Authors)", side = 1, line = 5.7, cex = 1.5)

mtext("Urban Concentration", side = 2, line = 5.7, cex = 2)
mtext("(Scaling Exponent - Beta)", side = 2, line = 3.7, cex = 1.5)


# text(x,y-0.025,z, cex = 0.75)

text(1.25, 1.8,
paste("r =", round(cor(x, y), digits = 2)),
cex = 2, col = "black")

reg1 <- lm(y ~ x)
ablineclip(reg1, lwd = 5, x1 = min(x), x2 = max(x), col = "blue") 


title(main="Publications")

dev.off()

