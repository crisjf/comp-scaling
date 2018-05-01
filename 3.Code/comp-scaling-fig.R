options(stringsAsFactors = FALSE)
# library('devtools')
# install_github("PABalland/EconGeo")
library(EconGeo)

#================#
# 0 - LOAD DATA  #
#================#
library('devtools')
install_github("PABalland/EconGeo")

setwd("D:/Dropbox/comp-scaling/1.Data")
# the xxx.Yr.csv files gives the different
# complexity indicators
comp = read.csv("US_FieldYr.csv")

# get names (also used to move to 2D)
n = read.csv("US_Field.csv")
comp = merge (comp, n, by = "ASJC.4D")

# to compute Beta (and other concentration indicators)
conc = read.csv("US_RegFieldYr.csv")
conc = get.matrix (conc[, c("CBSA", "ASJC.4D", "Nb.Papers.96.08")])
pop = read.csv("US_RegYr.csv")[,1:2]


#===================#
# 1 - COMPUTE Beta  #
#===================#

scaling = function (mat, pop) 
  
{
  d = NULL
  
  mat = as.matrix(mat)
  mat = get.list(mat)
  mat = merge (mat, pop, by.x = 1, by.y = 1)
  colnames (mat) = c("geo", "Industry", "Count", "pop")
  
  for(i in unique (mat$Industry)){
    
    xs = subset(mat, mat$Industry == i)
    xs$Count[xs$Count==0] <- NA
    lm = lm(log(xs$Count)~log(xs$pop))
    summary(lm)$coefficients[2, 1]
    
    beta = round (summary(lm)$coefficients[2, 1], digits = 3)
    r.sq = summary(lm)$adj.r.squared
    std.err = summary(lm)$coefficients[2, 2]
    
    econ = unique (xs$Industry)
    
    d = rbind(d, data.frame(econ, beta, r.sq, std.err))
    
  }
  colnames (d) = c("Industry", "Beta", "r.sq", "std.err")
  return (d)
}

df = scaling (conc, pop)
pub = merge (df, comp, by.x = "Industry", by.y = "ASJC.4D")


library(plotrix)
setwd("D:/Dropbox/comp-scaling/4.Results/Figure2")

# change to eps
pdf("comp-scal.pdf",width=10,height=8)

pub = subset(pub, pub$ASJC.4D.Name != "Agricultural and Biological Sciences")
pub = subset(pub, pub$ASJC.4D.Name != "Environmental Science")
pub = subset(pub, pub$ASJC.4D.Name != "Earth and Planetary Sciences")
pub = subset(pub, pub$ASJC.4D.Name != "Veterinary")


x = pub$Mean.Nb.Aut.96.08
#x = rank(pat$m_invcount)
#x = pat$avg.claims
y = pub$Beta
z = pub$ASJC.4D.Name

op <- par(cex.main = 2.5, mar = c(8, 6, 8, 8) + 0.1, mgp = c(3.5, 1, 0), 
          cex.lab = 1.75 , font.lab = 1.75, cex.axis = 1.75, bty = "n", las = 1)

plot(x, y, 
     xlim = c(1, 3), ylim = c(0.5,2),
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


text(x,y-0.025,z, cex = 0.75)

text(1.25, 1.8, 
     paste("r =", round(cor(x, y), digits = 2)), 
     cex = 2, col = "black")

reg1 <- lm(y ~ x)
ablineclip(reg1, lwd = 5, x1 = 1.25, x2 = 2.75, col = "blue") 


title(main="Publications")

dev.off()
