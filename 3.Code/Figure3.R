remove(list = ls())
source ("../2.Functions/crunching.R")
source ("../2.Functions/figLines.R")
source("../2.Functions/loadData.R")
library (EconGeo)

#===============#
# 1. FIGURE 3.A #
#===============#

delta<- 1
use <- 'claim'
outfile <- 'Figure3A_claim.eps'
RYfname <- "US_RegDec.csv"
Rfname  <- "US_Reg.csv"

df = read.csv(paste0("../1.Data/",RYfname))[,c('CBSA','dec','newpop','pat.ct','claim.ct')]
df <- merge(df,read.csv(paste0("../1.Data/",Rfname))[,c('CBSA','Flag.CBSA.availability')],by='CBSA')
# df <- df[df$Flag.CBSA.availability==1,]

df$Ec.Output <- df[,paste0(use,'.ct')]
df$Ec.Output <- df$Ec.Output+delta

d <- scalingDec(df,'Ec.Output')


betaCol <- 'Beta.pat'
errCol <- 'std.err.pat'

figLines(d,outfile,c(betaCol),c(errCol),palette=c('black'))


#======================================#
# 1. FIGURE 3.B - per complexity level #
#======================================#

RYfname <- "US_RegDec.csv"
Rfname  <- "US_Reg.csv"
df = read.csv(paste0("../1.Data/",RYfname))
df <- merge(df,read.csv(paste0("../1.Data/",Rfname))[,c('CBSA','Flag.CBSA.availability')],by='CBSA')
# df <- df[df$Flag.CBSA.availability==1,]

use <- 'claim'
outfile <- "Figure3B_claim.eps"
th <- 25
delta<- 0.1

df$bottom = bottomQ(df,th,use,delta)
df$top = topQ(df,th,use,delta)

dtop <- scalingDec(df,'top')
colnames(dtop) <- c('Decade','Beta.top','r.sq.top','std.err.top')
dbot <- scalingDec(df,'bottom')
colnames(dbot) <- c('Decade','Beta.bot','r.sq.bot','std.err.bot')
d <- merge(dtop,dbot,by='Decade')


ycols <- c('Beta.top','Beta.bot')
ecols <- c('std.err.top','std.err.bot')

figLines(d,outfile,ycols,ecols)


#===================================#
# 1. FIGURE 3.C - per tech category #
#===================================#

delta <- 0.1
use <- 'claim'
level <- 'Cat'
outfile <- "Figure3C_claim.eps"

df <- loadUSTechsDec(delta,use,level,allCities=TRUE)

d <- scalingDecByCat(df,use)
d <- d[(d$Decade<2010)&(d$Decade>=1850),]

ycol <- 'Beta.Pat'
figCatLines(d,outfile,ycol,'Cat',reverseY=FALSE)



#================================================#
# 1. FIGURE 3.D - per sub-cat category (2-digits)#
#================================================#

delta <- 0.1
use <- 'pat'
level <- 'Sub.Cat'
outfile <- "Figure3D.eps"

df <- loadUSTechsDec(delta,use,level)

d <- scalingDecByCat(df,use)
d <- merge (d, read.csv("../1.Data/US_Tech.csv")[, c('NBER.Sub.Cat', 'NBER.Cat')], by.x = "Cat", by.y = "NBER.Sub.Cat")
d <- d[(d$Decade<2010)&(d$Decade>=1850),]

ycol <- 'rank'
d = transform(d, rank = ave(Beta.Pat, Decade, FUN = function(x) rank(-x, ties.method = "first")))

figCatLines(d,outfile,ycol,'Cat',reverseY=TRUE)


