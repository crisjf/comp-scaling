remove(list = ls())
library(plotrix)
options(stringsAsFactors = FALSE)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(extrafont)
font_import("Avenir Next Condensed",prompt=FALSE)
source("../2.Functions/loadData.R")
source("../2.Functions/figScatter.R")
source("../2.Functions/crunching.R")
if (!('EconGeo' %in% installed.packages()[,"Package"])) {
  library('devtools')
  install_github("PABalland/EconGeo")
}
library(EconGeo)

actType <- 'field'
aggregate <- FALSE
delta <- 1
reverseX<- FALSE

loadUSParams(actType,aggregate)

comp <- loadUSComplexity(actType,aggregate,exclude=TRUE)
comp <- comp[!is.na(comp$comp),]

#write.csv(comp,file='~/Downloads/AgeOfRecombination2D.csv')

for (altComp in alternativeComp) {
  compAlt <- loadUSComplexity(actType,aggregate,exclude=TRUE,compCol = altComp)
  compAlt <- compAlt[!is.na(compAlt$comp),]
  colnames(compAlt) <- c(acol,aNameCol,altComp)
  comp <- merge(comp,compAlt)
}

data   <- loadUSActivity(delta,actType,aggregate,allCities=FALSE)
# region <- loadUSRegs(TRUE,2010,allCities=TRUE)
region <- loadUSRegs(useDec,year)

ivDec <- 1910
df = read.csv("../1.Data/US_RegDec.csv")[,c('CBSA','dec','newpop')]
df <- merge(df,read.csv(paste0("../1.Data/",Rfname))[,c('CBSA','Flag.CBSA.availability')],by='CBSA')
df <- df[df$Flag.CBSA.availability==1,]
df <- df[df$dec==ivDec,c('CBSA','newpop')]
colnames(df) = c('CBSA','pop.iv')
region <- merge(region,df,by='CBSA',all.x=TRUE)

length(unique(region$CBSA))
length(unique(data$CBSA))

data <- data[complete.cases(data),]
#beta <- scaling(get.matrix(data[,c(rcol,acol,'Ec.Output')]), region[,c(rcol,'pop','pop.iv')],th=150,delta=delta,useIv=TRUE,dropMissingIv=FALSE)

region <- region[region$pop>1e6,]

beta <- scaling(get.matrix(data[,c(rcol,acol,'Ec.Output')]), region[,c(rcol,'pop')],th=25,delta=delta,useIv=FALSE)
colnames(beta) <- c(acol,'Beta','r.sq','std.err')
beta <- beta[!is.na(beta$Beta),]



# matched <- unique(merge(data[,c(rcol,acol,'Ec.Output')],region[,c(rcol,'pop')],by=rcol))
# mat <- get.matrix(matched[order(matched$CBSA),c(rcol,acol,'Ec.Output')])
# vec <- unique(matched[order(matched$CBSA),'pop'])
# gini <- Hoover.Gini(mat, vec)
# colnames(gini) <- c(acol,'HGini')
# 
# data <- data[complete.cases(data),]
# beta <- scaling(get.matrix(data[,c(rcol,acol,'Ec.Output')]), region[,c(rcol,'pop')],th=150,delta=delta)
# colnames(beta) <- c(acol,'Beta','r.sq','std.err')
# beta <- beta[!is.na(beta$Beta),]


#==============#
# 3 - FIGURE 2 #
#==============#

pub <- merge(beta, comp, by = acol)
# pub <- merge(pub, gini, by = acol)

df <- merge(region,data,by='CBSA')
df <- df[df$Ec.Output>delta,]
df$total.Out <- ave(df[,'Ec.Output'], df[,acol], FUN = sum)
df$city.count <- ave(df[,'CBSA'], df[,acol], FUN = length)
df <- unique(df[,c(acol,'total.Out','city.count')])
#df <- df[df$city.count>=200,]
pub <- merge(pub,df,by=acol)



# dirName <- ''
# fig2Scatter(pub,'comp','Beta',acol,paste0('Complexity measure for ',acol),dirName='',reverseX=reverseX)

# fig2Scatter(pub,'comp','H_MEAN',acol,paste0('Complexity measure for ',acol),dirName='',reverseX=reverseX)


# unique(pub$Class[duplicated(pub$Class)])





ptitle <- actName

pxaxis <- compName
pyaxis <- 'Scaling exponent (β)'
fig2ScatterGG(pub,'comp','Beta',ptitle,pxaxis,pyaxis,actType)



# 
for (i in 1:length(alternativeComp)) {
  pxaxis <- compName
  pyaxis <- alternativeCompNames[i]
  fig2ScatterGG(pub,'comp',alternativeComp[i],ptitle,pxaxis,pyaxis,actType,grayscale=TRUE)

  pxaxis <- alternativeCompNames[i]
  pyaxis <- 'Scaling exponent (β)'
  fig2ScatterGG(pub,alternativeComp[i],'Beta',ptitle,pxaxis,pyaxis,actType)
}
#   
# pxaxis <- 'Scaling exponent (β)'
# pyaxis <- 'Hoover Gini'
# fig2ScatterGG(pub,'Beta','HGini',ptitle,pxaxis,pyaxis,actType,grayscale=TRUE)
# 
# pxaxis <- compName
# pyaxis <- 'Hoover Gini'
# fig2ScatterGG(pub,'comp','HGini',ptitle,pxaxis,pyaxis,actType)
# 
# 
# 
