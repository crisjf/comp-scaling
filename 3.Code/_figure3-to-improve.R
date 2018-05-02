# This script computes the scaling exponent for different 
# count of claims, per tech, per different levels of comp

# load scaling function 
setwd("../2.Functions")
source ("scaling.R")

library (EconGeo)

setwd("../1.Data")

#===============#
# 1. FIGURE 3.A #
#===============#

# for patents

# nb of patents by msa by dec, + pop, per complexity cat etc
df = read.csv("US_RegDec.csv")

d = NULL
df = df[complete.cases(df),]

for (i in unique (df$dec)) {
  
  c = subset (df, df$dec == i)
  m = get.matrix(c[, c("CBSA", "dec",
                       "pat.ct")])
  p = unique(c[, c("CBSA", "newpop")])
  scal = scaling(m, p)
  d = rbind (d, scal)
  
}

d = d[order(d$Industry),]
colnames (d) = c("Decade", "Beta.pat", "r.sq.pat",  "std.err.pat")

plot(d$Decade, d$Beta.pat)

#=======================================#
# 1. FIGURE 3.A - appendix - for claims #
#=======================================#

# nb of patents by msa by dec, + pop, per complexity cat etc
df = read.csv("US_RegDec.csv")

d = NULL
df = df[complete.cases(df),]

for (i in unique (df$dec)) {
  
  c = subset (df, df$dec == i)
  m = get.matrix(c[, c("CBSA", "dec",
                       "claim.ct")])
  p = unique(c[, c("CBSA", "newpop")])
  scal = scaling(m, p)
  d = rbind (d, scal)
  
}

d = d[order(d$Industry),]
colnames (d) = c("Decade", "Beta.claim", "r.sq.pat",  "std.err.pat")

plot(d$Decade, d$Beta.claim)

#======================================#
# 1. FIGURE 3.B - per complexity level #
#======================================#

# nb of patents by msa by dec, + pop, per complexity cat etc
df = read.csv("US_RegDec.csv")
df = df+0.01

d = NULL
df = df[complete.cases(df),]

for (i in unique (df$dec)) {
  
  c = subset (df, df$dec == i)
  m = get.matrix(c[, c("CBSA", "dec",
                       "top5pc.pat")])
  p = unique(c[, c("CBSA", "newpop")])
  scal = scaling(m, p)
  d = rbind (d, scal)
  
}

d = d[order(d$Industry),]
colnames (d) = c("Decade", "top5pc.pat", "r.sq.pat",  "std.err.pat")

plot(d$Decade, d$top5pc.pat)

# nb of patents by msa by dec, + pop, per complexity cat etc
df = read.csv("US_RegDec.csv")
df = df+0.01

d = NULL
df = df[complete.cases(df),]

for (i in unique (df$dec)) {
  
  c = subset (df, df$dec == i)
  m = get.matrix(c[, c("CBSA", "dec",
                       "top100pc.pat")])
  p = unique(c[, c("CBSA", "newpop")])
  scal = scaling(m, p)
  d = rbind (d, scal)
  
}

d = d[order(d$Industry),]
colnames (d) = c("Decade", "top100pc.pat", "r.sq.pat",  "std.err.pat")


lines (d$Decade, d$top100pc.pat)

# appendix: 
# we need to play with the delta (+0.1, +0.001...)
# different levels: top 5%, bottom 5%, top 25%, bottom 25%
# for patents and claims

#===================================#
# 1. FIGURE 3.C - per tech category #
#===================================#

# nb of patents per class 
df2 = read.csv("US_RegTechYr.csv")

# merge with pop
pop = read.csv("US_RegDec.csv")[, c("CBSA", "dec",
                                    "newpop")]

df2 = merge (df2, pop, by = c("CBSA", "dec"))

# merge with tech category (1-digit)

cat = read.csv ("US_Tech.csv")[, c("Class", "NBER.Cat", "NBER.Cat.Name")]
df2 = merge (df2, cat, by = "Class")
df2$ID = paste(df2$Class, df2$CBSA, df2$dec, sep = "-")

df2$pat.s = ave(df2$pat.count, df2$ID, FUN = sum)
df2$claim.s = ave(df2$claim.count, df2$ID, FUN = sum)

df2 = unique (df2[, c("CBSA", "NBER.Cat", "NBER.Cat.Name", 
                      "dec", "pat.s", "claim.s", "newpop")])

df2$ID = paste(df2$dec, df2$NBER.Cat.Name, sep = "-")
#df = merge (df, f, by.y = "CBSAID2013", by.x = "Cbsa")


d = NULL
df2 = df2[complete.cases(df2),]
df2$pat.count = df2$pat.s + 0.1

for (i in unique (df2$ID)) {
  
  c = subset (df2, df2$ID == i)
  m = get.matrix(c[, c("CBSA", "ID",
                       "pat.count")])
  p = unique(c[, c("CBSA", "newpop")])
  scal = scaling(m, p)
  d = rbind (d, scal)
  
}

d = d[order(d$Industry),]
d$Decade = substr(d$Industry, 0, 4)
d$Cat = trimws(substr(d$Industry, 6, 200))

d = d[, c("Cat", "Decade", "Beta", "r.sq", "std.err")]
colnames (d) = c("Cat", "Decade", "Beta.Pat", "r.sq.Pat", "std.err.Pat")



d1 = subset (d, d$Cat == "Computers & Communications")
d1 = d1[order(d1$Decade), ]
plot(d1$Decade, d1$Beta.Pat, col = "yellow")

d1 = subset (d, d$Cat == "Others")
d1 = d1[order(d1$Decade), ]
lines(d1$Decade, d1$Beta.Pat)

d1 = subset (d, d$Cat == "Mechanical")
d1 = d1[order(d1$Decade), ]
lines(d1$Decade, d1$Beta.Pat, col = "blue")

d1 = subset (d, d$Cat == "Electrical & Electronic")
d1 = d1[order(d1$Decade), ]
lines(d1$Decade, d1$Beta.Pat, col = "red")

d1 = subset (d, d$Cat == "Drugs & Medical")
d1 = d1[order(d1$Decade), ]
lines(d1$Decade, d1$Beta.Pat, col = "green")

# rob checks for patents, for claims, different deltas (+0.01), maybe different groups of cities (353), top 100...

#================================================#
# 1. FIGURE 3.D - per sub-cat category (2-digits)#
#================================================#

# same structure as 3.C, but as a bump chart (i.e. ranks)

