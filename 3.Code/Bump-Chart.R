#========================#
# 0 - LOAD DATA SCALING  #
#========================#

# load scaling function 
setwd("C:/Users/Balla103/Dropbox/University/1. Research/1. Papers/1. In progress/[1] Economic Complexity & Urban Scaling/_Functions")
source ("scaling.R")

library (EconGeo)


setwd("C:/Users/Balla103/Dropbox/University/1. Research/1. Papers/1. In progress/[1] Economic Complexity & Urban Scaling/3_Concentration/Historical Patents [USA]")
d = read.csv ("scaling-time-sub-cat-pat.csv")

# read pat per MSA and field
setwd("C:/Users/Balla103/Dropbox/University/1. Research/1. Papers/1. In progress/[1] Economic Complexity & Urban Scaling/0_Data/Patents [USA]")
df = read.table ("cbsaSubtech_ct.txt", header = T, check.names = F, sep = ",")

# merge with file create by Mathieu
ID = read.csv ("ID.patents.csv", header = T)
df = merge (df, ID, by.x = "Cbsa", by.y = "cbsa")
df = unique (subset (df, Flag_CBSA_availability == 1))


df = na.omit(df)

df$NBER.Sub.Cat.Name = trimws(df$NBER.Sub.Cat.Name)
df$dec = df$Period
df$ID = paste(df$dec, df$NBER.Sub.Cat.Name, sep = "-")
#df = merge (df, f, by.y = "CBSAID2013", by.x = "Cbsa")


d = NULL
df = df[complete.cases(df),]
df$pat.count = df$pat_count1 
df = subset (df, newpop>0)

for (i in unique (df$ID)) {
  
  c = subset (df, df$ID == i)
  m = get.matrix(c[, c("Cbsa.Name", "ID",
                       "pat.count")])
  m = m+0.1
  p = unique(c[, c("Cbsa.Name", "newpop")])
  scal = scaling(m, p)
  d = rbind (d, scal)
  
}

d = d[order(d$Industry),]
d$Decade = substr(d$Industry, 0, 4)
d$Cat = trimws(substr(d$Industry, 6, 200))

d = d[, c("Cat", "Decade", "Beta", "r.sq", "std.err")]
colnames (d) = c("Sub.Cat", "Decade", "Beta.Pat", "r.sq.Pat", "std.err.Pat")



setwd("C:/Users/Balla103/Dropbox/University/1. Research/1. Papers/1. In progress/[1] Economic Complexity & Urban Scaling/3_Concentration/Historical Patents [USA]")

a = read.csv ("Class.Att.csv")
a = unique (a[, c("NBER.Sub.Cat.Name", "NBER.Cat.Name")])
a$NBER.Cat.Name = trimws(a$NBER.Cat.Name)
a$NBER.Sub.Cat.Name = trimws(a$NBER.Sub.Cat.Name)
a$color = "red"
a$color[a$NBER.Cat.Name == "Computers & Communications"] = "yellow1"
a$color[a$NBER.Cat.Name == "Electrical & Electronic"] = "red"
a$color[a$NBER.Cat.Name == "Drugs & Medical"] = "green"
a$color[a$NBER.Cat.Name == "Chemical"] = "black"
a$color[a$NBER.Cat.Name == "Mechanical"] = "blue"
a$color[a$NBER.Cat.Name == "Others"] = "grey"

d = merge (d, a, by.x = "Sub.Cat", by.y = "NBER.Sub.Cat.Name")

d$ind = trimws(d$Sub.Cat)
d$year = d$Decade
d$Beta = d$Beta.Pat
d = subset(d, d$year > 1840)
d = subset(d, d$year < 2010)
d$ID = paste(d$Decade, d$ind)


d = transform(d, 
                rank = ave(Beta, Decade, 
                           FUN = function(x) rank(-x, ties.method = "first")))
d$Beta =  d$rank * (-1)

d$dec = d$Decade

d = subset (d, d$dec<2010)


lab = subset(d, dec == 2000)
lab = lab[, c("Sub.Cat", "rank")]
colnames (lab) = c("Sub.Cat", "lab")
d = merge (d, lab, by = "Sub.Cat")
d = d[order(d$dec),]


#==================#
# 1 - PLOT SCALING #
#==================#

setwd("C:/Users/Balla103/Dropbox/University/1. Research/1. Papers/1. In progress/[1] Economic Complexity & Urban Scaling/9-Scaling-Bump-Chart")
pdf("Scaling-Time-Sub-Cat.pdf",width=38,height=12)

par(mfrow=c(1,2))


#bottom, left, top and right margins 
par(cex.main = 2.5, mar = c(8, 8, 8, 4) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)


x = subset(d, d$ind == i)
x = subset(x, x$year > 1840)

plot (x$year, x$Beta+100, ylab = "", xlab = " ", cex = 1.5, 
      ylim = c(-36, 0), 
      xlim = c(1850, 2070), lwd = 2, pch = 21, axes = F, main = " ", col = x$color)

axis(1)

mtext("Decade", side = 1, line = 3, cex = 2, font = 2)

axis(2)

par(las = 0)

mtext("Urban Concentration", side = 2, line = 5.2, cex = 2, font = 2)
mtext("(Rank - Scaling Exponent - Beta)", side = 2, line = 3.7, cex = 1.5)

for (i in unique (d$ind)) {
  
  x = subset(d, d$ind == i)
  x = subset(x, x$year > 1840)

points(x$year, x$Beta, cex = 2, lwd = 2, pch = 21, col = x$color)
text(x$year, x$Beta, label=x$lab, col=x$color, cex = 0.6)
lines(x$year, x$Beta, lwd = 2, type = "c", col = x$color)

text(2010, x$Beta[x$Decade==2000], i, cex = 1.2, font = 1, adj = 0, col = x$color)

}
title(main="Urban Concentration of Technologies (1860-2000)")

t=d
t = t[, c("Sub.Cat", "Decade")]

#===========================#
# 3 - LOAD DATA COMPLEXITY  #
#===========================#

setwd("C:/Users/Balla103/Dropbox/University/1. Research/1. Papers/1. In progress/[1] Economic Complexity & Urban Scaling/2_Complexity/Historical Patents [USA]")

d = read.csv ("comp-age-sub-cat.csv")
d$NBER.Sub.Cat.Name = trimws(d$NBER.Sub.Cat.Name)

d = merge (d, t, by.x = c("NBER.Sub.Cat.Name", "dec"), by.y = c("Sub.Cat", "Decade"))

#same dim


d$NBER.Sub.Cat.Name = trimws(d$NBER.Sub.Cat.Name)

d = merge (d, a, by = "NBER.Sub.Cat.Name")

d$ind = d$NBER.Sub.Cat.Name
d$year = d$dec
d$Beta = -d$age.cat.dec
d = subset(d, d$year > 1840)
d = subset(d, d$year < 2010)
d$ID = paste(d$dec, d$ind)

d = unique(d[, c("NBER.Sub.Cat.Name", "NBER.Cat.Name", "year", "color","ind", "Beta", "dec")])

d = transform(d, 
              rank = ave(Beta, dec, 
                         FUN = function(x) rank(-x, ties.method = "first")))
d$Beta =  d$rank * (-1)

d = subset (d, d$dec<2010)
d = d[order(d$dec),]

lab = subset(d, dec == 2000)
lab = lab[, c("NBER.Sub.Cat.Name", "rank")]
colnames (lab) = c("NBER.Sub.Cat.Name", "lab")
d = merge (d, lab, by = "NBER.Sub.Cat.Name")
d = d[order(d$dec),]


#===============#
# 4 - PLOT COMP #
#===============#



#bottom, left, top and right margins 
par(cex.main = 2.5, mar = c(8, 4, 8, 8) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)


x = subset(d, d$ind == i)
x = subset(x, x$year > 1840)

plot (x$year, x$Beta+100, ylab = "", xlab = " ", cex = 1.5, 
      ylim = c(-36, 0), 
      xlim = c(1850, 2070), lwd = 2, pch = 21, axes = F, main = " ", col = x$color)

axis(1)

mtext("Decade", side = 1, line = 3, cex = 2, font = 2)

axis(2)

par(las = 0)

mtext("Knowledge Complexity", side = 2, line = 5.2, cex = 2, font = 2)
mtext("(Rank - Recombinatorial Age)", side = 2, line = 3.7, cex = 1.5)

for (i in unique (d$ind)) {
  
  x = subset(d, d$ind == i)
  x = subset(x, x$year > 1840)
  
  points(x$year, x$Beta, cex = 2, lwd = 2, pch = 21, col = x$color)
  text(x$year, x$Beta,label=x$lab,col=x$color, cex = 0.6)
  lines(x$year, x$Beta, lwd = 2, type = "c", col = x$color)
  
  text(2010, x$Beta[x$dec==2000], i, cex = 1.2, font = 1, adj = 0, col = x$color)
  
}

title(main="Knowledge Complexity of Technologies (1860-2000)")

dev.off()

